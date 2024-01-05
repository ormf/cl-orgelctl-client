;;; 
;;; structs.lisp
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

(defstruct val-orgel
  (base-freq 0.0 :type number)
  (phase 1 :type number)
  (bias-pos 0.0 :type number)
  (bias-bw 0.0 :type number)
  (bias-type 0 :type number)
  (main 0.0 :type number)
  (min-amp 0.0 :type number)
  (max-amp 0.0 :type number)
  (ramp-up 0.0 :type number)
  (ramp-down 0.0 :type number)
  (exp-base 0.0 :type number)
  (level (make-array 16 :initial-element 0.0) :type simple-array)
  (bias-level (make-array 16 :initial-element 0.0) :type simple-array)
  (delay (make-array 16 :initial-element 0.0) :type simple-array)
  (q (make-array 16 :initial-element 0.0) :type simple-array)
  (gain (make-array 16 :initial-element 0.0) :type simple-array)
  (osc-level (make-array 16 :initial-element 0.0) :type simple-array))


(defun copy-orgel (original)
  (make-val-orgel
   :base-freq (val-orgel-base-freq original)
   :phase (val-orgel-phase original)
   :bias-pos (val-orgel-bias-pos original)
   :bias-bw (val-orgel-bias-bw original)
   :bias-type (val-orgel-bias-type original)
   :main (val-orgel-main original)
   :min-amp (val-orgel-min-amp original)
   :max-amp (val-orgel-max-amp original)
   :ramp-up (val-orgel-ramp-up original)
   :ramp-down (val-orgel-ramp-down original)
   :exp-base (val-orgel-exp-base original)
   :level (copy-seq (val-orgel-level original))
   :delay (copy-seq (val-orgel-delay original))
   :q (copy-seq (val-orgel-q original))
   :gain (copy-seq (val-orgel-gain original))
   :osc-level (copy-seq (val-orgel-osc-level original))))

(defun copy-model-seq (model-array)
  (map 'vector #'val model-array))

(defun model-orgel->val-orgel (original)
  (make-val-orgel
   :base-freq (val (orgel-base-freq original))
   :phase (val (orgel-phase original))
   :bias-pos (val (orgel-bias-pos original))
   :bias-bw (val (orgel-bias-bw original))
   :bias-type (val (orgel-bias-type original))
   :main (val (orgel-main original))
   :min-amp (val (orgel-min-amp original))
   :max-amp (val (orgel-max-amp original))
   :ramp-up (val (orgel-ramp-up original))
   :ramp-down (val (orgel-ramp-down original))
   :exp-base (val (orgel-exp-base original))
   :level (copy-model-seq (orgel-level original))
   :delay (copy-model-seq (orgel-delay original))
   :q (copy-model-seq (orgel-q original))
   :gain (copy-model-seq (orgel-gain original))
   :osc-level (copy-model-seq (orgel-osc-level original))))

(defun val-orgel->model-orgel (src dest)
  (dolist (slot *orgel-global-target-syms*)
    (setf (val (slot-value dest slot)) (slot-value src slot)))
  (dolist (slot *orgel-fader-target-syms*)
    (dotimes (i 16)
      (setf (val (aref (slot-value dest slot) i)) (aref (slot-value src slot) i)))))

(defclass orgel-registry ()
  ((base-freq :initform nil)
   (phase :initform nil)
   (bias-pos :initform nil)
   (bias-bw :initform nil)
   (bias-type :initform nil)
   (main :initform nil)
   (min-amp :initform nil)
   (max-amp :initform nil)
   (ramp-up :initform nil)
   (ramp-down :initform nil)
   (exp-base :initform nil)
   (level :initform (make-array 16 :initial-element nil) :type simple-array)
   (bias-level :initform (make-array 16 :initial-element nil) :type simple-array)
   (delay :initform (make-array 16 :initial-element nil) :type simple-array)
   (q :initform (make-array 16 :initial-element nil) :type simple-array)
   (gain :initform (make-array 16 :initial-element nil) :type simple-array)
   (osc-level :initform (make-array 16 :initial-element nil) :type simple-array)
   (mlevel :initform (make-array 16 :initial-element nil) :type simple-array)))
