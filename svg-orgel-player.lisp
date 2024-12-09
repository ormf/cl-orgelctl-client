;;; 
;;; svg-orgel-player.lisp
;;;
;;; Play Papierrohrorgel using the ats browser interface.
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

(in-package :cm)

(defparameter *svg-orgel-play-unwatch* nil)
(defparameter *svg-orgel-play-toggle* (make-ref 0.0))

(defun pending-orgel-notes-off ()
  (funcall cl-orgelctl::*keymap-note-responder-fn* :clear))

(pushnew #'pending-orgel-notes-off cm.svgd:*stop-hooks*)

(in-package :cl-orgelctl)

(defun import-quantize-midifile (fname &key (quant-level 8) (transposition 0))
  (sort
   (let ((seq (import-events fname)))
     (loop for track in (cdr seq)
           append
           (progn
             (map-objects (lambda (m)
                            (setf (object-time m) (/ (round (* quant-level (object-time m))) quant-level))
                            (sv m cm::channel 5)
                            (sv+ m cm::keynum transposition)
                            (sv m cm::amplitude 1.0)
                            m)
                          (subobjects track)
                          :class 'midi)
             (subobjects track :class 'midi))))
   #'<
   :key #'object-time))

#|

(events (import-quantize-midifile "~/work/selmafile/orm-unterricht/24-sose/syntaktische-grundlagen/24-05-14/tristan-auszüge/robin-verklärung.mid" :transposition 18)
(svg-gui-path "robin-verklärung.svg"))

(events (import-quantize-midifile "~/work/selmafile/orm-unterricht/24-sose/syntaktische-grundlagen/24-05-14/tristan-auszüge/clemens-verklärung.mid" :transposition 18)
        (svg-gui-path "clemens-verklärung.svg"))

(set-val cm.svgd:timescale (* 3/2 1/32))
(svg->browser "robin-verklärung.svg")
|#



