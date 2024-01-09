;;; 
;;; midi-cc-handler.lisp
;;;
;;; framework to set midi cc responders
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

(defparameter *global-midi-channel* 5)

(defparameter *midi-cc-state*
  (make-array 16 :element-type '(simple-array model-slot)
                 :initial-contents
                 (loop repeat 16
                       collect (make-array
                                128
                                :element-type 'model-slot
                                :initial-contents
                                (v-collect (n 128) (make-instance 'model-slot))))))

(defparameter *midi-cc-responders*
  (make-array 16 :element-type '(simple-array list)
                 :initial-contents (loop repeat 16
                                         collect
                                         (make-array 128 :element-type 'list
                                                         :initial-element nil))))


(defun ccin (ccnum &optional (channel *global-midi-channel*))
  (val (aref (aref *midi-cc-state* channel) ccnum)))

(defsetf ccin (ccnum &optional (channel *global-midi-channel*)) (value)
  `(progn
     (setf (val (aref (aref *midi-cc-state* ,channel) ,ccnum)) ,value)
     ,value))

(defparameter *orgel-cc-responder* nil)

(defun make-orgel-cc-responder ()
  (setf *orgel-cc-responder*
        (incudine:make-responder
         cm:*midi-in1*
         (lambda (st d1 d2)
           (case (cm:status->opcode st)
             (:cc (let ((channel (cm:status->channel st))
                        (val (float (/ d2 127) 1.0)))
                    (incudine::msg info "orgel-midi-responder: :cc ~d ~d ~,2f" channel d1 val)
                    (setf (ccin d1 channel) val))))))))

(defun remove-orgel-cc-responder ()
  (incudine:remove-responder *orgel-cc-responder*))

;;; (incudine::remove-responder *orgel-midi-responder*)
;;; (remove-orgel-cc-responder)
;;; (make-orgel-cc-responder)
;;; (incudine.util::set-logger-level) :warn)
;;; (incudine::remove-all-responders cm:*midi-in1*)

(defun add-cc-responder (ccnum fn &key (channel *global-midi-channel*))
  "push fn to the the responders of <ccnum> at <channel>."
  (push fn (aref (aref *midi-cc-responders* channel) ccnum)))

(defun remove-cc-responders (ccnum &key (channel *global-midi-channel*))
  "clear all responders of <ccnum> at <channel>."
  (setf (aref (aref *midi-cc-responders* channel) ccnum) nil))

(defun remove-channel-cc-responders (channel)
  "clear all cc responders."
  (dotimes (ccnum 128)
    (setf (aref (aref *midi-cc-responders* channel) ccnum) nil)))

(defun remove-all-cc-responders ()
  "clear all cc responders."
  (dotimes (channel 16)
    (remove-channel-cc-responders channel)))

(defun register-cc-ref-cell-hooks ()
  (dotimes (chan 16)
    (dotimes (ccnum 128)
      (let ((cell (aref (aref *midi-cc-state* chan) ccnum)))
        (setf (set-cell-hook cell)
              (let ((chan chan)
                    (ccnum ccnum))
                (lambda (ccval)
                  (dolist (fn (aref (aref *midi-cc-responders* chan) ccnum))
                    (funcall fn ccval)))))))))


#|
(add-cc-responder 0 (lambda (val) (format t "~&Lieber Robin midi-in channel: ~d cc: ~d ~a" 5 0 val)))
(add-cc-responder 6 (lambda (val) (format t "~&midi-in channel: ~d cc: ~d ~a" 5 1 val)))

(remove-cc-responders 0)

|#



;;; (setf (ccin 0) 64)

