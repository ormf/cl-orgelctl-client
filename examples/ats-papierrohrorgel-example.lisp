;;; 
;;; ats-papierrohrorgel-example.lisp
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

;;; wenn der orgel-server bereits gestartet ist:

(ql:quickload "cl-orgelctl-client")

(connect-to-server)

(in-package :ats-cuda)


;;; analyse a soundfile:

(defparameter *village01* nil)
;;; (defparameter village02a nil)
;;; (defparameter village02b nil)

(tracker (asdf:system-relative-pathname :cl-orgelctl-client "snd/village01.wav")
           '*village01*
           :start 0.0
           :hop-size 1/4
           :lowest-frequency 100.0
           :highest-frequency 20000.0
           :frequency-deviation 0.5
           :lowest-magnitude (db-amp -40)
           :SMR-continuity 0.7
           :track-length 6
           :min-segment-length 3
           :residual "/tmp/village01-res.snd"
           :verbose nil
           :debug nil)

(in-package :cl-orgelctl)

;;; Den ats-player starten (ats-cuda:browser-play-papierorge ats-cuda::village01:

(progn
  (set-orgel-freqs
   (mapcar (lambda (x) (* x 2))
           '(27.5 32.401794 38.49546 46.19711 56.132587
             69.28748 87.30706 113.156204 152.76933 220.0))
   2)
  (digest-route-preset
   15
   `(:preset nil
     :routes (:all (:main (main 1))
              :orgel01
              (:bias-pos (ccin 0) :bias-bw (ccin 1)
               :global ((apply-notch :bias-type
                                       (bias-cos :bias-pos :bias-bw :targets *global-targets*
                                                 :levels *global-amps*))
                          *global-targets*)))))
  (orgel-ctl :orgel01 :bias-bw 1)
  (ats-cuda:browser-play-papierorgel ats-cuda::*village01*)
  (play-browser 4))

;;; Im Browser die Datei
;;; ~/quicklisp/local-projects/cl-orgelctl-client/html/ats-display.html
;;; öffnen, um die Datei zu sehen und mit der Maus zu spielen.

;;; play currently loaded ats-sound in 4 seconds

(play-browser 4)


