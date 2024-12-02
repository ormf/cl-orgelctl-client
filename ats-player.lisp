;;; 
;;; ats-player.lisp
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

(in-package :ats-cuda-display)

(defparameter atsd.orgel-target (make-ref :level))

(defparameter atsd.orgel-play (make-ref 0.0))

(defun ats-orgel-display (body)
  "On-new-window handler."
  (let (controls atsd.svg atsd.orgel-play-toggle atsd.play-toggle atsd.bw-slider atsd.contrast-slider
        atsd.res-bal-slider atsd.pitchbox atsd.freqbox atsd.timebox
        atsd.osc-play-toggle atsd.osc-amp-slider)
    (setf (title (clog::html-document body)) "ATS Orgel Player")
    (setf (style body :background-color) "#c0b65c")
    (set-val atsd.scale 0.3)
    (setf atsd.svg
          (create-o-svg
           body (bind-refs-to-attrs atsd.width "width" atsd.x "cursor-pos" atsd.shift-x "shift-x" atsd.data "svg-file"
                                    atsd.scale "scale" atsd.crosshairs "crosshairs" atsd.mousepos "mousepos"
                                    atsd.bw "bandwidth" atsd.contrast "ats-contrast")))
    ;; (create-o-radio body (bind-refs-to-attrs idx "value") :css '(:width "6em") :labels (list (loop for idx from 1 to 6 collect idx)) :num 6)
    (setf controls (create-div body :style "display: flex; height: 3em; margin-top: 0.5em"))
    (setf atsd.orgel-play-toggle
          (create-o-toggle controls (bind-refs-to-attrs atsd.orgel-play "value")
                           :css '(:font-size "2em" :width "3em" :display "block") :label '("off" "on") :background '("transparent" "#8f8")))

    (setf atsd.play-toggle
          (create-o-toggle controls (bind-refs-to-attrs atsd.play "value")
                           :css '(:font-size "2em" :width "3em" :display "block") :label '("off" "on") :background '("transparent" "#8f8")))
    (setf atsd.contrast-slider
          (create-o-slider controls (bind-refs-to-attrs atsd.contrast "value")
                           :width "4em" :css '(:margin-left "0.5em") :height "88%" :direction :right :min 0 :max 1))
    (setf atsd.bw-slider
          (create-o-slider controls (bind-refs-to-attrs atsd.bw "value")
                           :width "4em" :css '(:margin-left "0.5em") :height "88%" :direction :right :min 0.01 :max 1))
    (setf atsd.res-bal-slider
          (create-o-slider controls (bind-refs-to-attrs atsd.res-balance "value")
                           :width "4em" :css '(:margin-left "0.5em") :height "88%" :direction :right :min 0 :max 1))
    (setf atsd.timebox
          (create-o-numbox controls (bind-refs-to-attrs atsd.time "value") :min 0 :max 10
                           :css '(:margin-left "0.5em")))
    (setf atsd.pitchbox
          (create-o-numbox controls (bind-refs-to-attrs atsd.pitch "value") :min 0 :max 127
                           :css '(:margin-left "0.5em")))
    (setf atsd.freqbox
          (create-o-numbox controls (bind-refs-to-attrs atsd.freq "value") :min 0 :max 10000
                           :css '(:margin-left "0.5em")))
    (setf atsd.osc-play-toggle
          (create-o-toggle controls (bind-refs-to-attrs atsd.osc-play "value")
                           :css '(:font-size "2em" :margin-left "0.5em" :width "3em" :display "block") :label '("off" "on") :background '("transparent" "#8f8")))
    (setf atsd.osc-amp-slider
          (create-o-slider controls (bind-refs-to-attrs atsd.osc-amp "value")
                           :width "4em" :css '(:margin-left "0.5em") :height "88%" :direction :right :min 0 :max 0.01))
    (atsd-set-keyboard-mouse-shortcuts body atsd.svg atsd.orgel-play-toggle atsd.bw-slider atsd.contrast-slider atsd.res-bal-slider)))

(defun on-new-ats-orgel-window (body)
  (ats-orgel-display body))

(set-on-new-window #'on-new-ats-orgel-window :path "/ats-orgel-display")

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
         (maxfreq (float (+ 100 (aref (ats-sound-frq-av ats-snd)
                                      (1- num-partials)))
                         1.0))
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
    (clamps:ats->svg ats-snd :brightness (getf args :brightness 20) :fname "/tmp/ats.svg")
    (broadcast-message "reload")
    (if *curr-browser-player* (free (browser-player-id *curr-browser-player*)))
    (setf *curr-browser-player* browser-player)
    (recalc-amps)
    ;; (apply #'incudine::sin-noi-rtc-synth 0.0 ats-snd
    ;;        :amod (browser-player-amod browser-player)
    ;;        :fmod (browser-player-fmod browser-player) :id (getf args :id 2) args)
    (setf (browser-player-id *curr-browser-player*) (getf args :id 2))
    browser-player))

(defun coords (x y)
  (cl-orgelctl::orgelctl-coords x y))


;;; (aref)

(export '(browser-play-papierorgel play-browser) 'ats-cuda)

(in-package :cl-orgelctl)

(defun update-mute-faders (new-faders new-amps last-faders)
  "update last-faders given the new faders and new amps."
  (dolist (fader last-faders)
;;;        (break "~a ~a ~a" fader new-faders (member fader new-faders :test #'equal))
;;; unless (member fader new-faders :test #'equal)
    (unless (member fader new-faders :test #'equal)
      (destructuring-bind (target orgelidx partial) fader
;;        (clamps::msg :warn "muting target: ~a orgelno: ~a partial: ~a" target (1+ orgelidx) partial)
        (orgel-ctl-fader
         (aref *orgel-name-lookup* orgelidx)
         (intern (string-upcase (symbol-name target)) 'cl-orgelctl) partial 0.0d0))))
  (loop for fader in new-faders
        for amp in new-amps
;;;        (break "~a ~a ~a" fader new-faders (member fader new-faders :test #'equal))
;;; unless (member fader new-faders :test #'equal)
        do (destructuring-bind (target orgelidx partial) fader
             (orgel-ctl-fader
              (aref *orgel-name-lookup* orgelidx)
              (intern (string-upcase (symbol-name target)) 'cl-orgelctl) partial amp)))
  
  new-faders)

(defun get-freq-amps (frame ats-sound &key (dbthresh -40) (gainfac 1))
  (loop
    with ampthresh = (ou:db->amp dbthresh)
    for partial below (ats-cuda::ats-sound-partials ats-sound)
    for amp = (aref (ats-cuda::ats-sound-amp ats-sound) partial frame)
    if (> amp ampthresh)
      collect (list (aref (ats-cuda::ats-sound-frq ats-sound) partial frame) (min 1.0d0 (* gainfac amp)))))

(defun orgelctl-coords (x y &key (fader 'osc-level))
  (let* ((ats-sound (ats-cuda::browser-player-ats-sound ats-cuda::*curr-browser-player*))
         (frame (min (1- (ats-cuda::ats-sound-frames ats-sound))
                     (round (* x (1- (ats-cuda::ats-sound-frames ats-sound)))))))
    (if (/= frame (ats-cuda::browser-player-last-frame ats-cuda::*curr-browser-player*))
        (let ((fader-amps (find-orgel-fader-amps
                           (get-freq-amps frame ats-sound :gainfac 8)
                           :fader fader)))
          (setf (ats-cuda::browser-player-last-frame ats-cuda::*curr-browser-player*) frame)
;;;          (format t "~&faders: ~a~%" (first fader-amps))
          (if (member 0 (mapcar #'third (first fader-amps)))
              (format t "~a~%" (first fader-amps)))
          (setf *global-targets* (first fader-amps))
          (setf *global-amps* (coerce (second fader-amps) 'vector))
;;;          (oscillator-mute)
          (update-mute-faders (first fader-amps) (second fader-amps) ats-cuda::*curr-browser-player*)))
    (orgel-ctl :orgel01 :bias-bw y))
  ;; (set-control 2 :soundpos x)
  ;; (setf (browser-player-soundpos *curr-browser-player*) x)
  ;; (setf (browser-player-mousefreq *curr-browser-player*)
  ;;       (* (max 0.0 (min y 1.0)) (browser-player-maxfreq *curr-browser-player*)))
  ;; (recalc-amps)
  )

(defparameter *atsd-orgel-last-frame* 0)
(defparameter *atsd-orgel-last-faders* nil)

(set-val ats-cuda-display::atsd.orgel-target :level)

(defun start-orgel-play ()
  (dolist (fn *orgel-play-watch*) (funcall fn))
  (clamps::msg :warn "starting ats-orgel-player")
  (setf *atsd-orgel-last-faders* nil)
  (setf *orgel-play-watch*
        (list
         (let ((last-frame -1)
;;;               (last-faders nil)
;;;               (fader target)
               )
           (watch (lambda ()
                    (destructuring-bind (x y) (get-val atsd.mousepos)
                      (let* ((ats-sound atsd.sound)
                             (frame (min (1- (ats-cuda::ats-sound-frames ats-sound))
                                         (round (* x (1- (ats-cuda::ats-sound-frames ats-sound)))))))
                        (if (/= frame last-frame)
                            (let ((fader-amps (find-orgel-fader-amps
                                               (get-freq-amps frame ats-sound :gainfac 8)
                                               :fader (get-val ats-cuda-display::atsd.orgel-target))))
                              (setf last-frame frame)
;;;          (format t "~&faders: ~a~%" (first fader-amps))
                              (if (member 0 (mapcar #'third (first fader-amps)))
                                  (format t "~a~%" (first fader-amps)))
                              (setf *global-targets* (first fader-amps))
                              (setf *global-amps* (coerce (second fader-amps) 'vector))
;;;          (oscillator-mute)
                              (setf *atsd-orgel-last-faders*
                                    (update-mute-faders (first fader-amps) (second fader-amps) *atsd-orgel-last-faders*))))
                        (orgel-ctl :orgel01 :bias-bw (get-val atsd.bw))
                        (orgel-ctl :orgel01 :bias-pos y)))))))))

;;; (start-orgel-play :target :level)

(defun stop-orgel-play ()
  (clamps::msg :warn "stopping ats-orgel-player")
  (dolist (fn *orgel-play-watch*) (funcall fn))
  (setf *orgel-play-watch* nil)
  (setf *atsd-orgel-last-faders*
        (update-mute-faders nil nil *atsd-orgel-last-faders*)))

;;; (stop-orgel-play)

(defparameter *orgel-play-watch* nil
  "the unwatch function of start/stop-orgel-play (observing the mouse coords, bw, etc.)")

(defparameter *ats-orgel-play-toggle-unwatch* nil
  "the unwatch function of the atsd.orgel-play-button (observing just the play button)")


(defun init-ats-orgel-play-watch ()
  "connect state changes in ats-cuda-display::atsd.orgel-play
to starting/stopping of the ats-orgel-player."
  (dolist (fn *ats-orgel-play-toggle-unwatch*) (funcall fn))
  (setf *ats-orgel-play-toggle-unwatch*
        (list
         (watch (lambda () (if (zerop (get-val ats-cuda-display::atsd.orgel-play))
                          (stop-orgel-play)
                          (start-orgel-play)))))))

(init-ats-orgel-play-watch)

(defun orgelctl-coords-clamps (x y &key (fader 'osc-level))
  (let* ((ats-sound ats-cuda-display:atsd.sound)
         (frame (min (1- (ats-cuda::ats-sound-frames ats-sound))
                     (round (* x (1- (ats-cuda::ats-sound-frames ats-sound)))))))
    (if (/= frame *atsd-orgel-last-frame*)
        (let ((fader-amps (find-orgel-fader-amps
                           (get-freq-amps frame ats-sound :gainfac 8)
                           :fader fader)))
          (setf *atsd-orgel-last-frame* frame)
;;;          (format t "~&faders: ~a~%" (first fader-amps))
          (if (member 0 (mapcar #'third (first fader-amps)))
              (format t "~a~%" (first fader-amps)))
          (setf *global-targets* (first fader-amps))
          (setf *global-amps* (coerce (second fader-amps) 'vector))
;;;          (oscillator-mute)
          (update-mute-faders (first fader-amps) (second fader-amps))))
    (orgel-ctl :orgel01 :bias-bw y))
  ;; (set-control 2 :soundpos x)
  ;; (setf (browser-player-soundpos *curr-browser-player*) x)
  ;; (setf (browser-player-mousefreq *curr-browser-player*)
  ;;       (* (max 0.0 (min y 1.0)) (browser-player-maxfreq *curr-browser-player*)))
  ;; (recalc-amps)
  )

;;; (oscillator-mute)

(defun play-browser (dur &key (fader 'osc-level))
  (let* ((start (cm:now))
         (end (+ start dur)))
    (labels ((inner (time)
               (when (< time end)
                 (let ((next (+ time 0.05)))
                   (orgelctl-coords (/ (- time start) dur) 0.0 :fader fader)
                   (cm:at next #'inner next)))))
      (inner start))))

;;; (shadowing-import 'coords 'cl-orgelctl)


#|

(oscillator-mute)

|#
