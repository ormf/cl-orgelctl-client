;;; 
;;; orgel-accessor-fns.lisp
;;;
;;; definition of getter and setter functions for all slots of the
;;; papierorgel. the orgeltarget can be specified either as
;;; orgelnummer or as keyword.
;;;
;;; Examples:
;;;
;;; getter functions:
;;;
;;; (level :orgel01 2)
;;; (level 1 2)
;;; (base-freq :orgel02)
;;; (base-freq 2)
;;;
;;; setter functions:
;;; 
;;; (setf (level :orgel01 2) 0.3)
;;; (setf (level 1 2) 0.3)
;;; (setf (base-freq :orgel02) 271)
;;; (setf (base-freq 2) 271)
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

(defun val-orgel-access-fn (target)
  "retrieve function object by keyword of its name."
  (symbol-function (intern (string-upcase (format nil "val-orgel-~a" target)) :cl-orgelctl)))

;;; (orgel-access-fn :level) -> #<function orgel-level>

(defun orgel-slot-name (target)
  "convert keyword to symbol"
  (read-from-string (format nil "orgel-~a" target)))

;;; (orgel-slot-name :level) -> orgel-level 

;;; utility shorthand access fns for the organ slots in *curr-state*

(declaim (inline get-orgelidx))

;;; todo: use set-cell functions!!

(defmacro define-orgel-fader-access-fn (target)
  `(progn
     (defun ,(intern (string-upcase (format nil "~a" target))) (orgelnummer partial)
       ,(format nil "access function for the ~a slot with index <idx>
of orgel at <orgelnummer> in *curr-state*." target)
       (let ((orgelidx (gethash orgelnummer *orgeltargets*)))
         (val (aref (,(intern (string-upcase (format nil "orgel-~a" target)) :cl-orgelctl)
                     (aref *curr-state* orgelidx))
                    (1- partial)))))
     (defsetf ,(intern (string-upcase (format nil "~a" target))) (orgelnummer partial) (value)
       ,(format nil "access function for the ~a slot with index <idx>
of orgel at <orgelnummer> in *curr-state*." target)
       `(orgel-ctl-fader ,orgelnummer ,,target ,partial ,value))))

(defmacro define-all-orgel-fader-access-fns (targets)
  `(progn
     ,@(loop
         for target in (eval targets)
         collect (list 'define-orgel-fader-access-fn target))))

(define-all-orgel-fader-access-fns *orgel-fader-targets*)

;; (define-orgel-fader-access-fn :level)
;; (define-orgel-fader-access-fn :gain)
;; (define-orgel-fader-access-fn :delay)
;; (define-orgel-fader-access-fn :q)
;; (define-orgel-fader-access-fn :osc-level)

(defmacro define-orgel-global-access-fn (target)
  `(progn
     (defun ,(intern (string-upcase (format nil "~a" target))) (orgelnummer)
       ,(format nil "access function for the ~a slot of orgel at <orgelnummer> in *curr-state*." target)
       (let ((orgelidx (gethash orgelnummer *orgeltargets*)))
         (val (,(intern (string-upcase (format nil "orgel-~a" target)) :cl-orgelctl)
               (aref *curr-state* orgelidx)))))
     (defsetf ,(intern (string-upcase (format nil "~a" target))) (orgelnummer) (value)
       `(orgel-ctl ,orgelnummer ,,target ,value))))

(defmacro define-all-orgel-global-access-fns (targets)
  `(progn
     ,@(loop
         for target in (eval targets)
         collect (list 'define-orgel-global-access-fn target))))

;;; for the following target list we need to remove phase as this
;;; symbol is used and we call the access function "ophase":

(define-all-orgel-global-access-fns (remove :bias-type (remove :phase *orgel-global-targets*)))

(deftype orgel-phase () `(member -1 1 -1.0 1.0))
(deftype orgel-bias-type () `(member 0 1 0.0 1.0))

(defun bias-type (orgelnummer)
"access function for the phase slot with index <idx>
of orgel at <orgelnummer> in *curr-state*."
  (let ((orgelidx (gethash orgelnummer *orgeltargets*)))
    (val (orgel-bias-type (aref *curr-state* orgelidx)))))

(defun (setf bias-type) (value orgeltarget)
  (declare (type orgel-bias-type value))
  (orgel-ctl orgeltarget :bias-type value))

(defun ophase (orgelnummer)
"access function for the phase slot with index <idx>
of orgel at <orgelnummer> in *curr-state*."
  (let ((orgelidx (gethash orgelnummer *orgeltargets*)))
    (val (orgel-phase (aref *curr-state* orgelidx)))))

(defun (setf ophase) (value orgeltarget)
  (declare (type orgel-phase value))
  (orgel-ctl orgeltarget :phase value))

;;; another special case, as mlevel isn't part of a preset and
;;; therefore not stored in *curr-state*

(defun mlevel (orgelnummer partial)
"access function for the mlevel slot with index <idx>
of orgel at <orgelnummer> in *curr-state*."
  (let ((orgelidx (gethash orgelnummer *orgeltargets*)))
    (val (aref (aref *orgel-mlevel* orgelidx) (1- partial)))))

(defun (setf mlevel) (value orgelnummer partial)
  (let ((orgelidx (gethash orgelnummer *orgeltargets*)))
    (setf (val (aref (aref *orgel-mlevel* orgelidx) (1- partial))) value)))
