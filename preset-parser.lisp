;;; 
;;; preset-parser.lisp
;;;
;;; parse orglctl presets by checking all slots to be observed in the
;;; preset form and creating and installing a value-change handler
;;; function for each of them.
;;;
;;; **********************************************************************
;;; Copyright (c) 2022 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

;;; association of orgel-slots to their setter functions

(defparameter *orgel-preset-def-lookup* (make-hash-table))

(defun set-preset-def-lookup ()
  "fill the *orgel-preset-def-lookup* hash."
  (setf *orgel-preset-def-lookup* (make-hash-table))
  (dolist (target *orgel-fader-targets*)
    (setf (gethash target *orgel-preset-def-lookup*)
          `(set-faders ,target))
    (dotimes (i 16)
      (let ((fader-target (read-from-string (format nil ":~a~2,'0d" target (1+ i)))))
        (setf (gethash fader-target 
               *orgel-preset-def-lookup*)
              `(orgel-ctl-fader ,(read-from-string (format nil ":~a" target)) ,(1+ i))))))
  (dolist (target *orgel-global-targets*)
    (setf (gethash target *orgel-preset-def-lookup*)
          `(orgel-ctl ,target)))
  (setf (gethash :global *orgel-preset-def-lookup*)
        `(set-global-faders)))

(set-preset-def-lookup)

;;; lookup of accessor function-names and keywords of all slots within
;;; the current orgel.

(defparameter *observed* (make-hash-table))

(progn
  (mapcar (lambda (x) (setf (gethash (read-from-string (format nil "~a" x)) *observed*) t))
          (append *orgel-global-targets* *orgel-fader-targets* *orgel-level-meter-targets* *midi-targets*))
  (mapcar (lambda (x) (loop
                   for slider from 1 to 16
                   do (setf (gethash
                             (read-from-string (format nil ":~a~2,'0d" x slider))
                             *observed*)
                            `(,(read-from-string (format nil "~a" x)) ,slider))))
          (append *orgel-fader-targets* *orgel-level-meter-targets*))
  (mapcar (lambda (x) (setf (gethash (read-from-string (format nil ":~a" x)) *observed*)
                       `(,(read-from-string (format nil "~a" x)))))
          *orgel-global-targets*)
;;;  (setf (gethash 'ccin *observed*) t)
;;;   (setf (gethash 'notein *observed*) t)  
 *observed*)

(defun replace-keywords (form orgelno)
  "replace all keywords of observed slots in form by their expansion into
a valid access function call form:

:level01 -> (level <orgelno> 1)

:bias-pos -> (bias-pos <orgelno>)
"
  (cond
    ((null form) nil)
    ((keywordp (first form))
     (cons
      (let ((frm (gethash (first form) *observed*)))
        (if frm
            (list* (first frm) orgelno (rest frm))
            (first form)))
      (replace-keywords (cdr form) orgelno)))
    ((consp (first form))
     (cons
      (if (gethash (caar form) *observed*)
          (first form)
          (replace-keywords (first form) orgelno))
      (replace-keywords (cdr form) orgelno)))
    (:else (cons (first form)
                 (replace-keywords (rest form) orgelno)))))

(defun orgel-dependency-form (form)
  (if (gethash (first form) *observed*)
      (list form)))

;;; -> (base-freq phase main min-amp max-amp ramp-up ramp-down exp-base bias-pos level delay q gain osc-level mlevel)

(defun parse-observed (form)
  "extract all forms containing observed slots from form, replacing
keywords with their expanded access functions."
  (cond
    ((null form) nil)
    ((consp (first form))
     (append
      (or (orgel-dependency-form (first form))
          (parse-observed (car form)))
      (parse-observed (cdr form))))
    (:else (or (orgel-dependency-form form)
               (parse-observed (cdr form))))))

;;; (parse-observed '(+ (mlevel 1 1) :delay01 (- (level 1 1) (gain 2 1) (* 3 (base-freq 2)))) 1)
;;; -> ((mlevel 1 1) (delay 1 1) (level 1 1) (gain 2 1) (base-freq 2))

(defun get-fn (target orgel form)
"return the compiled function for a given target in the preset
definition. A target can be
:level, but also :level01"
  (if (eql target :global)
      (eval `(lambda (&rest args) (declare (ignorable args))
               (set-global-faders ,(second form) ,(first form))))
      (let* ((call-spec (gethash target *orgel-preset-def-lookup*))
             (orgeltarget orgel))
        (eval `(lambda (&rest args) (declare (ignorable args))
;;;                 (format t "calling:" )
                 (,(first call-spec) ,orgeltarget
                  ,@(rest call-spec) ,form))))))

(defun register-responder (fn observed)
  "register fn in the responder registry of all targets supplied in the
observed arg. A target either starts with 'ccin, has length 3 for
a fader target, or length 2 for a global target."
  (cond
    ((eql (first observed) 'ccin)
     (push fn (aref (aref *midi-cc-responders* (or (third observed) *global-midi-channel*))
                    (second observed))))
    ((eql (first observed) 'notein)
     (push fn (aref (aref *midi-note-responders* (or (third observed) *global-midi-channel*))
                    (second observed))))
    ((= (length observed) 3)
     (push fn (aref (slot-value (aref *osc-responder-registry* (1- (second observed))) (first observed))
                    (1- (third observed)))))
    (:else
     (push fn (slot-value (aref *osc-responder-registry* (1- (second observed))) (first observed))))))

(defun register-responders (target orgel form reset)
  (let* ((registered-fns '())
         (new-form (replace-keywords form (orgel-nr orgel)))
         (fn (get-fn target orgel new-form)))
    (dolist (observed (parse-observed new-form))
      (register-responder fn observed)
      (when (and reset (not (member fn registered-fns)))
          (push fn registered-fns)
          (if reset (funcall fn))))))

(defun digest-route (orgel form reset)
  (loop
    for (target form) on form by #'cddr
    do (register-responders target orgel form reset)))

(defun digest-routes (form &key (reset nil))
  (clear-osc-responder-registry)
  (remove-all-cc-responders)
  (loop
    for (orgel form) on form by #'cddr
    do (digest-route orgel form reset)))
