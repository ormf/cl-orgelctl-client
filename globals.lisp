;;; 
;;; globals.lisp
;;;
;;; global variables are defined here.
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

(defparameter *debug* t
   "flag for printing debugging info.")

(unless (boundp '*orgelcount*)
  (defvar *orgelcount* 8)
  "total num of organs")

(unless (fboundp 'make-orgel)
  (defstruct orgel
    (base-freq (make-instance 'model-slot) :type model-slot)
    (phase (make-instance 'model-slot :val 1) :type model-slot)
    (bias-pos (make-instance 'model-slot) :type model-slot)
    (bias-bw (make-instance 'model-slot) :type model-slot)
    (bias-type (make-instance 'model-slot :val 0) :type model-slot)
    (main (make-instance 'model-slot) :type model-slot)
    (min-amp (make-instance 'model-slot) :type model-slot)
    (max-amp (make-instance 'model-slot) :type model-slot)
    (ramp-up (make-instance 'model-slot) :type model-slot)
    (ramp-down (make-instance 'model-slot) :type model-slot)
    (exp-base (make-instance 'model-slot) :type model-slot)
    (level (make-array 16 :initial-contents (v-collect (n 16) (make-instance 'model-slot))) :type simple-array)
    (delay (make-array 16 :initial-contents (v-collect (n 16) (make-instance 'model-slot))) :type simple-array)
    (q (make-array 16 :initial-contents (v-collect (n 16) (make-instance 'model-slot))) :type simple-array)
    (gain (make-array 16 :initial-contents (v-collect (n 16) (make-instance 'model-slot))) :type simple-array)
    (osc-level (make-array 16 :initial-contents (v-collect (n 16) (make-instance 'model-slot))) :type simple-array)))

(defparameter *base-freqs*
  '(220.0 188.13 160.77 137.48 117.49 100.47 85.86 73.416)
  "all base frequencies of the orgel.")

;;; (mapcar #'base-freq (range 1 9))

(defparameter *orgel-freqs*
  (sort
      (loop
        for base-freq in *base-freqs*
        for orgeltarget from 1
        append (loop
                 for partial from 1 to 16
                 collect (list (dround (* base-freq partial))
                               (dround (ftom (* base-freq partial)))
                               orgeltarget partial)))
      #'<
      :key #'first)
  "all available frequencies in orgel. The entries contain frequency,
keynum, orgelno and partialno.")

(defparameter *orgel-freqs-vector*
  (coerce
   (sort
    (loop
      for base-freq in *base-freqs*
      for orgeltarget from 1
      append (loop
               for partial from 1 to 16
               collect (list (dround (* base-freq partial))
                             (dround (ftom (* base-freq partial)))
                             orgeltarget partial)))
    #'<
    :key #'first)
   'vector)
  "all available frequencies in orgel. The entries contain frequency,
keynum, orgelno and partialno.")


(defparameter *orgel-max-freq* (caar (last *orgel-freqs*)))
(defparameter *orgel-min-freq* (caar *orgel-freqs*))

(defparameter *orgel-partial-idx-lookup* (make-array '(9 17) :element-type 'integer))

(loop for (freq midict orgelno partial) in *orgel-freqs*
      for idx from 0
      do (setf (aref *orgel-partial-idx-lookup* orgelno partial) idx))


(defparameter *orgel-presets-file*
  (asdf:system-relative-pathname :cl-orgelctl-client "presets/orgel-presets.lisp"))

(defparameter *route-presets-file*
    (asdf:system-relative-pathname :cl-orgelctl-client "presets/route-presets.lisp"))

(defconstant +notch+ 1)
(defconstant +bandp+ 0)
(defconstant +phase+ 1)
(defconstant +invert+ -1)

;;; the current state of all orgel vars: In case cl-orgel-gui is used,
;;; this needs to be defined there so we only define it in case it
;;; hasn't already been defined.

(unless (boundp '*curr-state*)
  (defparameter *curr-state*
    (make-array
     *orgelcount*
     :element-type 'orgel
     :initial-contents (loop for i below *orgelcount* collect (make-orgel)))
    "State of all faders of the orgel on the pd side."))

(unless (boundp '*orgel-mlevel*)
  (defparameter *orgel-mlevel*
    (make-array *orgelcount*
                :element-type 'simple-array
                :initial-contents
                (loop
                  for i below *orgelcount*
                  collect (make-array 16 :element-type 'float
                                         :initial-contents (loop for x below 16 collect 0.0))))
    "all volume levels currently measured in pd (permanently updated)."))

(defparameter *orgeltargets* (make-hash-table)
  "lookup of orgelname (as keyword) or orgelnumber (starting from 1 to
zerobased index.")

(defparameter *global-targets* nil)
(defparameter *global-idx-amp-targets* nil)
(defparameter *global-amps* nil)

;;; setup of *orgeltargets*

(dotimes (i *orgelcount*)
  (setf (gethash (read-from-string (format nil ":orgel~2,'0d" (1+ i)))
                 *orgeltargets*)
        i
        (gethash (1+ i) *orgeltargets*)
        i))

(defparameter *orgel-global-targets*
  '(:base-freq :phase :bias-pos :bias-bw :bias-type :main :min-amp :max-amp
    :ramp-up :ramp-down :exp-base))

(defparameter *orgel-global-target-syms*
  (mapcar (lambda (key) (intern (symbol-name key)))
          *orgel-global-targets*))

(defparameter *orgel-fader-targets*
  '(:level :delay :q :gain :osc-level))

(defparameter *orgel-fader-target-syms*
  (mapcar (lambda (key) (intern (symbol-name key)))
          *orgel-fader-targets*))

;;; (mapcar #'cons *orgel-global-targets* *orgel-global-target-syms*)

(defun target-string->sym (key)
  (cdr
   (assoc key
          '(("level" . level) ("delay" . delay) ("q" . q) ("gain" . gain)
            ("osc-level" . osc-level) ("base-freq" . base-freq) ("phase" . phase)
            ("bias-pos" . bias-pos) ("bias-bw" . bias-bw) ("bias-type" . bias-type)
            ("main" . main) ("min-amp" . min-amp) ("max-amp" . max-amp)
            ("ramp-up" . ramp-up) ("ramp-down" . ramp-down) ("exp-base" . exp-base))
          :test #'string=)))

(defun target-key->sym (key)
  (cdr
   (assoc key
          '((:level . level) (:delay . delay) (:q . q) (:gain . gain) (:osc-level . osc-level)
            (:base-freq . base-freq) (:phase . phase) (:bias-pos . bias-pos) (:bias-bw . bias-bw)
            (:bias-type . bias-type) (:main . main) (:min-amp . min-amp) (:max-amp . max-amp)
            (:ramp-up . ramp-up) (:ramp-down . ramp-down) (:exp-base . exp-base)))))

;;; (target-key->sym :level)

(defparameter *orgel-level-meter-targets*
  '(:mlevel))

(defparameter *midi-targets*
  '(ccin notein))

(defparameter *orgel-nr-lookup* nil
  "property list of orgel names and their number.")

(defparameter *orgel-name-lookup* #()
    "vector associating orgel numbers with their name.")

(setf *orgel-nr-lookup*
      (loop for idx below *orgelcount*
            for name = (read-from-string (format nil ":orgel~2,'0d" (1+ idx)))
            append (list name (1+ idx)))

      *orgel-name-lookup*
      (coerce (cons nil (loop
                          for (name idx) on *orgel-nr-lookup* by #'cddr
                          collect name))
              'vector))
