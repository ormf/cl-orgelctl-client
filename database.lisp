;;; 
;;; database.lisp
;;;
;;; a database for lookup of the closest faders for given frequencies
;;; in all papierorgeln.
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

(defun build-database (organstate)
  "build a database of all frequencies possible with given base-freq of
all organs in *curr-state*. The database is a list of (freq orgelidx
partial-number) sorted by frequency."
  (sort
   (loop
     for orgelidx below (length organstate)
     for base-freq = (val (orgel-base-freq (aref *curr-state* orgelidx)))
     append (loop for partial from 1 to 16
                  collect (list (* base-freq partial) orgelidx partial)))
   #'<
   :key #'first))

(defparameter *orgel-db* (build-database *curr-state*))

(defun fv->ct (fv)
  "return midicent from frequency ratio <fv>."
  (* 12 (log fv 2)))

(defun get-fader (freq &key (db *orgel-db*))
  "return the orgelfader closest to <freq> according to db. The returned
list is

(freq-of-organ-partial orgelidx partialno deviation-in-midicent)."
  (loop
    for (last next) on db
    while (and next (> freq (first next)))
    finally (if next
                (return (if (< (- (first next) freq)
                               (- freq (first last)))
                            (append next (list (fv->ct (/ (first next) freq))))
                            (append last (list (fv->ct (/ freq (first last))))))))))

#|

;;; fill *curr-state* with random base-frequencies.

(dotimes (orgelidx (length *curr-state*))
  (setf (orgel-base-freq (aref *curr-state* orgelidx)) (+ 100 (random 400.0))))

;;, Lookup the closest fader for 1200 Hz:

(get-fader 1200) -> (1130.4521 2 5 1.0336133)

|#
