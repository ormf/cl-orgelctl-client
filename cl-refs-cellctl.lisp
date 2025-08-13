;;; 
;;; cl-refs-cellctl.lisp
;;;
;;; compatibility layer for cl-refs
;;;
;;; **********************************************************************
;;; Copyright (c) 2025 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cellctl)

(defmethod (setf val) (new-val (instance cl-refs:ref-object))
;;;  (format t "directly setting value-cell~%")
  (cl-refs:set-val instance new-val)
  new-val)

(defmethod (setf val) (new-val (instance cl-refs:bang-object))
;;;  (format t "directly setting value-cell~%")
  (cl-refs:set-val instance new-val)
  new-val)

(defmethod val ((instance cl-refs:ref-object))
;;;  (format t "directly setting value-cell~%")
  (cl-refs:get-val instance))

(defmethod val ((instance cl-refs:bang-object))
;;;  (format t "directly setting value-cell~%")
  (cl-refs:get-val instance))
