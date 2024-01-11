;;; 
;;; midi-notein.lisp
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

(defparameter *midi-note-state*
  (make-array 16 :element-type '(simple-array single-float)
                 :initial-contents
                 (loop repeat 16
                       collect (make-array
                                128
                                :element-type 'model-slot
                                :initial-contents
                                (v-collect (n 128) (make-instance 'model-slot))))))

(defparameter *midi-note-responders*
  (make-array 16 :element-type '(simple-array list)
                 :initial-contents (loop repeat 16
                                         collect
                                         (make-array 128 :element-type 'list
                                                         :initial-element nil))))

(defparameter *orgel-note-responder* nil)

#|

(defun notein (keynum &optional (channel *global-midi-channel*))
  (val (aref (aref *midi-note-state* channel) keynum)))

(defsetf notein (keynum &optional (channel *global-midi-channel*)) (velo)
  `(progn
     (setf (val (aref (aref *midi-note-state* ,channel) ,keynum)) ,velo)
     ,velo))

|#

(defun notein (keynum &optional (channel *global-midi-channel*))
  (val (aref (aref *midi-note-state* channel) keynum)))

(defun (setf notein) (value keynum &optional (channel *global-midi-channel*))
  (progn
    (setf (val (aref (aref *midi-note-state* channel) keynum)) value)))

(defun make-orgel-note-responder ()
  (if *orgel-note-responder* (incudine::remove-responder *orgel-note-responder*))
  (setf *orgel-note-responder*
        (incudine:make-responder
         cm:*midi-in1*
         (lambda (st d1 d2)
           (case (cm:status->opcode st)
             (:note-on (let ((channel (cm:status->channel st))
                             (val (float (/ d2 127) 1.0)))
                         (incudine::msg info "orgel-note-on-responder: ~d ~d ~,2f" channel d1 val)
                         (setf (notein d1 channel) val)))
             (:note-off (let ((channel (cm:status->channel st)))
                          (incudine::msg info "orgel-note-off-responder: ~d ~d ~,2f" channel d1 0.0)
                          (setf (notein d1 channel) 0.0))))))))

;;; (make-orgel-note-responder)

(defun clear-orgel-note-responder ()
  (incudine:remove-responder *orgel-note-responder*))

;;; (incudine::remove-responder *orgel-note-responder*)

(defun add-note-responder (keynum fn &key (channel *global-midi-channel*))
  "push fn to the the responders of <keynum> at <channel>."
  (push fn (aref (aref *midi-note-responders* channel) keynum)))

(defun remove-note-responders (keynum &key (channel *global-midi-channel*))
  "clear all responders of <keynum> at <channel>."
  (setf (aref (aref *midi-note-responders* channel) keynum) nil))

(defun remove-channel-note-responders (channel)
  "clear all note responders."
  (dotimes (keynum 128)
    (setf (aref (aref *midi-note-responders* channel) keynum) nil)))

(defun remove-all-note-responders ()
  "clear all note responders."
  (dotimes (channel 16)
    (remove-channel-note-responders channel)))

(defun all-notes-off (&optional chans)
  (dolist (chn (or chans (ou:range 16)))
    (dotimes (key 128) (setf (notein key chn) 0.0))))

(defun register-notein-ref-cell-hooks ()
  (dotimes (chan 16)
    (dotimes (keynum 128)
      (setf (set-cell-hook (aref (aref *midi-note-state* chan) keynum))
            (let ((chan chan)
                  (keynum keynum))
              (lambda (velo)
                (dolist (fn (aref (aref *midi-note-responders* chan) keynum))
                  (funcall fn velo))))))))

;;; (register-notein-ref-cell-hooks)

