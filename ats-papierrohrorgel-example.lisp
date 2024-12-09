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

(in-package :clamps)


;;; analyse a soundfile:

(defparameter village01 nil)
;;; (defparameter village02a nil)
;;; (defparameter village02b nil)

(setf village01
      (track-ats (asdf:system-relative-pathname :cl-orgelctl-client "snd/village01.wav")
                 :start 0.0
                 :hop-size 1/4
                 :lowest-frequency 100.0
                 :highest-frequency 20000.0
                 :frequency-deviation 0.5
                 :lowest-magnitude (db->amp -40)
                 :SMR-continuity 0.7
                 :track-length 6
                 :min-segment-length 3
                 :residual "/tmp/village01-res.snd"
                 :verbose nil
                 :debug nil))

;; send to browser:

(ats->browser village01)

(ats->browser (load-ats "cl.ats"))

(pushnew "/home/orm/work/selmafile/orm-unterricht/24-wise/yaohung/24-11-20/throatsinging/" *ats-file-path*)
(ats->browser (load-ats "yaohung02.ats"))

;; open http://localhost:54619/ats-orgel-display

(in-package :cl-orgelctl)

(with-open-file (out "/tmp/test.txt" :direction :output :if-exists :supersede)
  (format out "~{~a ~}"
          (let ((seq (append
                      '(0 0)
                      (loop
                        for i from 0
                        for cents in (mapcar (lambda (x) (round (* x 100))) (mapcar #'second *orgel-freqs*))
                        append (list cents i))
                      '(12700 127))))
            (loop for x below 12700
                  collect (round (interpl x seq))))))

(svg->browser "robin-verklärung.svg")

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
                                     (bias-cos
                                      :bias-pos :bias-bw
                                      :targets *global-targets*
                                      :levels *global-amps*))
                          *global-targets*)))))
  (orgel-ctl :orgel01 :bias-bw 1)
  (ats-cuda:browser-play-papierorgel ats-cuda::village01)
  (play-browser 4))

(ou:differentiate (mapcar #'second *orgel-freqs*))
(copy-orgel-preset *curr-state* (aref *orgel-presets* 1))
(save-orgel-presets)

(cd (asdf:system-relative-pathname :cl-orgelctl ""))

;;; play currently loaded ats-sound in 4 seconds

(mapcar #'wellenlaenge *base-freqs*)


(play-browser 4)

(ats-cuda::coords)

(coords)
(orgel-ctl-fader)

(cm:events
 (loop for base-freq in *base-freqs*
       for time from 0 by 0.1
       for i from 1
       append (loop for p from 1 to 16 collect (cm:new cm:midi :time time :keynum (ou:ftom (* base-freq p)) :duration 1 :channel i)))
 "/tmp/freqs.svg")



(cm:cd (asdf:system-relative-pathname :cl-orgelctl ""))
(save-orgel-presets (asdf:system-relative-pathname :cl-orgelctl "presets/orgel-presets.lisp"))

(load-orgel-presets (asdf:system-relative-pathname :cl-orgelctl "presets/orgel-presets.lisp"))

(copy-orgel-preset *curr-state* (aref *orgel-presets* 0))


