;;; 
;;; ats-player.lisp
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

(in-package :ats-cuda)

(defun browser-play-papierorgel (ats-sound &rest args)
  (let* ((ats-snd
           (if (or (stringp ats-sound) (typep ats-sound 'pathname))
               (symbol-value (ats-cuda::ats-load
                              ats-sound
                              (intern
                               (string-upcase
                                (pathname-name (pathname ats-sound)))
                               :ats-cuda)))
               ats-sound))
         (num-partials (ats-sound-partials ats-snd))
         (maxfreq (float (+ 100 (aref (ats-sound-frq-av ats-snd) (1- num-partials))) 1.0))
         (browser-player
           (make-browser-player
            :ats-sound ats-snd
            :amp-scale (getf args :amp-scale 1.0)
            :num-partials num-partials
            :partials (getf args :partials (range num-partials))
            :res-bal (getf args :res-bal 0.5)
            :maxfreq maxfreq
            :last-frame -1
            :amod (make-array num-partials :element-type 'incudine::sample :initial-element 1.0d0)
            :fmod (make-array num-partials :element-type 'incudine::sample :initial-element 1.0d0)
            :bw (getf args :bw 40000)
            :soundpos (getf args :soundpos 0)
            :mousefreq (* (max 0.0 (min (getf args :y 0) 1.0)) maxfreq))))
    (ats->svg ats-snd :brightness (getf args :brightness 20))
    (broadcast-message "reload")
    (if *curr-browser-player* (free (browser-player-id *curr-browser-player*)))
    (setf *curr-browser-player* browser-player)
    (recalc-amps)
    ;; (apply #'incudine::sin-noi-rtc-synth 0.0 ats-snd
    ;;        :amod (browser-player-amod browser-player)
    ;;        :fmod (browser-player-fmod browser-player) :id (getf args :id 2) args)
    (setf (browser-player-id *curr-browser-player*) (getf args :id 2))
    browser-player))


(defun get-freq-amps (frame ats-sound &key (dbthresh -40))
  (loop
    with ampthresh = (ou:db->amp dbthresh)
    for partial below (ats-sound-partials ats-sound)
    for amp = (aref (ats-sound-amp ats-sound) partial frame)
    if (> amp ampthresh)
      collect (list (aref (ats-sound-frq ats-sound) partial frame) (* 3 amp))))

(defun update-mute-faders (new-faders browser-player)
  (if new-faders
      (dolist (fader (browser-player-last-faders browser-player))
;;;        (break "~a ~a ~a" fader new-faders (member fader new-faders :test #'equal))
;;; unless (member fader new-faders :test #'equal)
        (destructuring-bind (target orgelidx partial) fader
          (cl-orgelctl::orgel-ctl-fader (aref cl-orgelctl::*orgel-name-lookup* orgelidx) target partial 0.0d0)            )))
  (setf (browser-player-last-faders browser-player) new-faders))

(defun coords (x y)
  (let* ((ats-sound (browser-player-ats-sound *curr-browser-player*))
         (frame (min (1- (ats-sound-frames ats-sound)) (round (* x (1- (ats-sound-frames ats-sound)))))))
    (if (/= frame (browser-player-last-frame *curr-browser-player*))
        (let ((fader-amps (cl-orgelctl::find-orgel-fader-amps
                           (get-freq-amps frame ats-sound)
                           :fader 'osc-level)))
          (setf (browser-player-last-frame *curr-browser-player*) frame)
;;;          (format t "~&faders: ~a~%" (first fader-amps))
          (if (member 0 (mapcar #'third (first fader-amps)))
              (format t "~a~%" (first fader-amps)))
          (setf cl-orgelctl::*global-targets* (first fader-amps))
          (setf cl-orgelctl::*global-amps* (coerce (second fader-amps) 'vector))
          (cl-orgelctl::oscillator-mute)
          (update-mute-faders (first fader-amps) *curr-browser-player*)
          (mapcar #'funcall (slot-value (aref cl-orgelctl::*osc-responder-registry* 0) 'cl-orgelctl::bias-bw))))
    (cl-orgelctl::orgel-ctl :orgel01 :bias-pos y))
  ;; (set-control 2 :soundpos x)
  ;; (setf (browser-player-soundpos *curr-browser-player*) x)
  ;; (setf (browser-player-mousefreq *curr-browser-player*)
  ;;       (* (max 0.0 (min y 1.0)) (browser-player-maxfreq *curr-browser-player*)))
  ;; (recalc-amps)
  )

;;; (aref)

(export '(browser-play-papierorgel play-browser) 'ats-cuda)

(in-package :cl-orgelctl)

(defun play-browser (dur)
  (let* ((start (cm:now))
         (end (+ start dur)))
    (labels ((inner (time)
               (when (< time end)
                 (let ((next (+ time 0.05)))
                   (ats-cuda::coords (/ (- time start) dur) 0.0)
                   (cm:at next #'inner next)))))
      (inner start))))

#|
(ats-cuda:browser-play-papierorgel ats-cuda::village01)



ats-cuda::*curr-browser-player*
(coords)
(ats-cuda:browser-play-papierorgel ats-cuda::village01)

ats-cuda::*curr-browser-player*
|#
