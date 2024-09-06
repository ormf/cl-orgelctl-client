;;; 
;;; osc-midi.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :cl-orgelctl)

(defparameter *localhost* "127.0.0.1")

;;; this has to be defined for every registered host:
;;; (defparameter *oscout* (incudine.osc:open :port 4711 :host *remote-host* :direction :output :protocol :udp))
(defparameter *osc-midi-in* nil)
(defparameter *midictl-osc-responders* (make-hash-table)) ;;; a hashtable with the handles of all registered midi responders

(defparameter *midictl-osc-remote-connections* nil)

(defun osc-midi-add-remote-connection (host &key (port 4710))
  (let ((entry (first (member host *midictl-osc-remote-connections* :key #'incudine.osc:host :test #'string-equal))))
    (when entry
      (incudine.osc:close entry)
      (setf *midictl-osc-remote-connections*
            (delete entry *midictl-osc-remote-connections*))))
  (let ((connection (incudine.osc:open :port port :host host :direction :output :protocol :udp)))
    (push connection *midictl-osc-remote-connections*)
    (incudine.osc:message connection "/msg" "s" "connected")))

(defun osc-midi-remove-remote-connection (host)
  (let ((entry (first (member host *midictl-osc-remote-connections* :key #'incudine.osc:host :test #'string-equal))))
    (when entry
      (incudine.osc:close entry)
      (setf *midictl-osc-remote-connections*
            (delete host *midictl-osc-remote-connections* :key #'incudine.osc:host :test #'string-equal)))))

(defun osc-midi-remove-all-remote-connections ()
  (dolist (conn *midictl-osc-remote-connections*)
    (incudine.osc:close conn))
  (setf *midictl-osc-remote-connections* nil))

;;; osc responder:

(defparameter +ml-note-off-opcode+ 8)
(defparameter +ml-note-on-opcode+ 9)
(defparameter +ml-key-pressure-opcode+ 10)
(defparameter +ml-control-change-opcode+ 11)
(defparameter +ml-program-change-opcode+ 12)
(defparameter +ml-channel-pressure-opcode+ 13)
(defparameter +ml-pitch-bend-opcode+ 14)
(defconstant +ml-opcode-mask+ #xf0)
(defconstant +ml-channel-mask+ #x0f)

(defparameter *ml-opcodes*
  `((,+ml-control-change-opcode+ . :cc)
    (,+ml-note-on-opcode+ . :note-on)
    (,+ml-note-off-opcode+ . :note-off)
    (,+ml-program-change-opcode+ . :pgm-change)
    (,+ml-pitch-bend-opcode+ . :pitch-bend)
    (,+ml-key-pressure-opcode+ . :key-pressure)
    (,+ml-channel-pressure-opcode+ . :channel-pressure)))

(defun status->opcode (st)
  (cdr (assoc (ash (logand st +ml-opcode-mask+) -4)
              *ml-opcodes*)))

(defun status->channel (st)
  (logand st +ml-channel-mask+))

(defun start-osc-midi-receive (&key (host "127.0.0.1") (port 4711))
  "start osc on host:port and its receivers."
  (when *osc-midi-in* (incudine.osc:close *osc-midi-in*))
  (maphash (lambda (key val) key (incudine:remove-responder val)) *midictl-osc-responders*)
  (setf *osc-midi-in* (incudine.osc:open :host host :port port :direction :input :protocol :udp))
  (setf (gethash :osc-midi-register *midictl-osc-responders*)
        (incudine::make-osc-responder
         *osc-midi-in* "/osc-midi-register" "sf"
         (lambda (host port)
           (let ((port (round port)))
             (incudine.util:msg :info "osc-midi-register: ~a ~a" host port)
             (osc-midi-add-remote-connection host :port port)))))
  (setf (gethash :midiin *midictl-osc-responders*)
        (incudine::make-osc-responder
         *osc-midi-in* "/midiin" "fff"
         (lambda (st d1 d2)
           (let ((st (round st))
                 (d1 (round d1)))
             (incudine::msg info "orgel-osc-midi-responder: ~S ~d ~,2f ~d "  (status->opcode st) d1 (float (/ d2 127) 1.0) (status->channel st))
             (dolist (responder (gethash cm:*midi-in1* incudine::*responders*))
               (funcall (incudine::responder-function responder) st d1 d2 cm:*midi-in1*))))))
  (incudine:recv-start *osc-midi-in*))

;;; (start-osc-midi-receive)

(defun stop-osc-midi-receive (&optional local-midi-in)
  (declare (ignore local-midi-in))
   (incudine::recv-stop *osc-midi-in*))

(defun osc-midi-broadcast (st d1 d2)
  (dolist (connection *midictl-osc-remote-connections*)
    (incudine.osc:message connection "/midiout" "iii" st d1 d2)))

(defun osc-midi-write-short (stream st d1 d2)
  (incudine.util:msg :info "osc-midi-write: ~a ~a ~a ~%" st d1 d2)
  (jackmidi:write-short stream (jackmidi:message st d1 d2) 3)
  (osc-midi-broadcast st (round d1) (round d2)))
