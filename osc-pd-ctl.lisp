;;; 
;;; osc-pd-ctl.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(defparameter *osc-pd-in* nil)
;;; (defparameter *osc-pd-responder* nil)
;;; (defparameter *pd-ctl-rec* nil)
(defparameter *pd-recorder* nil)

(defun start-osc-pd-in (&key (host "localhost") (port 3020))
  "Open an OSC port for pd-ctl messages on /host/ and /port/."
  (if *osc-pd-in* (incudine.osc:close *osc-pd-in*))
  (setf *osc-pd-in* (incudine.osc:open :host host :port port :direction :input :protocol :udp)))

(defun make-pd-recorder ()
  "Closure for a recorder of pd-ctl osc messages."
  (let* ((recording nil)
;;;         (start-time 0)
         (pd-ctl-rec nil)
         (osc-responder
           (incudine::make-osc-responder
            *osc-pd-in* "/pd-orgelctl-fader" "sfff"
            (lambda (target orgelno partialno value)
              (when pd-ctl-rec
                (push (list :time (now) :target (make-keyword target)
                            :keynum (+ (* (- 8 (round orgelno)) 16)
                                       (1- (round partialno)))
                            :orgelno (round orgelno) :partialno (round partialno) :value value)
                      recording)
                (incudine.util:msg :info "pd-orgelctl-in: ~a ~a ~a ~a" target orgelno partialno value))))))
    (lambda (cmd)
      (case cmd
        (:start
         (setf recording nil)
         (setf pd-ctl-rec t))
        (:stop
         (setf pd-ctl-rec nil))
        (:list
         recording)
        (:save nil)
        (:clear (incudine:remove-responder osc-responder))))))

(defun rec->midi (recording &key (min-dur 0.1))
  "Convert a recording of orgel :level events into a midi sequence. The
recording has to be sorted prior to calling this function."
  (labels ((next-time (keynum rest)
             "return the time of the next evt with /keynum/ in /rest/ or nil if none
exists."
               (dolist (evt rest)
                 (if (= keynum (getf evt :keynum))
                     (return (getf evt :time))))))
      (loop
        with start-time = (getf (first recording) :time)
        for (curr . rest) on recording
        for curr-time = (getf curr :time)
        for next-time = (next-time (getf curr :keynum) rest)
        do (remf curr :time)
        if (not (zerop (getf curr :value)))
          collect (new midi
                    :time (float (- curr-time start-time) 1.0)
                    :keynum (getf curr :keynum)
                    :duration (if next-time (float (- next-time curr-time) 1.0) min-dur)
                    :amplitude (getf curr :value)
                    :channel 7))))


#|
;;; Intialisierung:

(start-osc-pd-in)
(incudine:recv-start *osc-pd-in*)
(setf (incudine.util:logger-level) :warn)



;;; "Instanz" erzeugen:

(setf *pd-recorder* (make-pd-recorder))

;;; Bedienung:


(funcall *pd-recorder* :start)
(funcall *pd-recorder* :stop)

(funcall *pd-recorder* :list)
|#
