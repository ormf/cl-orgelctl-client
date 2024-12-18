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
(defparameter *osc-pd-responder* nil)
(defparameter *pd-ctl-rec* nil)
(defparameter *pd-recorder* nil)

(defun start-osc-pd-in (&key (host "localhost") (port 3020))
  "Open an OSC port for pd-ctl messages on /host/ and /port/."
  (if *osc-pd-in* (incudine.osc:close *osc-pd-in*))
  (setf *osc-pd-in* (incudine.osc:open :host host :port port :direction :input :protocol :udp)))

(defun make-pd-recorder ()
  "Closure for a recorder of pd-ctl osc messages."
  (let* ((recording nil)
         (pd-ctl-rec nil)
         (osc-responder
           (incudine::make-osc-responder
            *osc-pd-in* "/pd-orgelctl-fader" "sfff"
            (lambda (target orgelno partialno value)
              (when pd-ctl-rec
                (push (list :time (now) :target (make-keyword target)
                            :orgelno (round orgelno) :partialno (round partialno) :value value)
                      recording)
                (incudine.util:msg :info "pd-orgelctl-in: ~a ~a ~a ~a" target orgelno partialno value))))))
    (lambda (cmd)
      (case cmd
        (:start
         (setf pd-ctl-rec t)
         (setf recording nil))
        (:stop
         (setf pd-ctl-rec nil))
        (:list
         recording)
        (:save nil)
        (:clear (incudine:remove-responder osc-responder))))))

#|
;;; Intialisierung:

(start-osc-pd-in)
(incudine:recv-start *osc-pd-in*)
(setf (incudine.util:logger-level) :info)



;;; "Instanz" erzeugen:

(setf *pd-recorder* (make-pd-recorder))

;;; Bedienung:


(funcall *pd-recorder* :start)
(funcall *pd-recorder* :stop)

(funcall *pd-recorder* :list)
|#
