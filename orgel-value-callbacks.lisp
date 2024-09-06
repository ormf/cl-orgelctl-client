;;; 
;;; orgel-value-callbacks.lisp
;;;
;;; callback functions when a value changes (triggered by osc messages
;;; from the lisp server or by midi-controllers or calling of value
;;; changing functions).
;;;
;;; the callback does 3 things:
;;;
;;; 1. update the respective slot of *curr-state*
;;; 2. send this to the lisp server.
;;;
;;; the src argument is nil if the values are received from osc and
;;; therefore the callbacks don't send to pd (another mechanism has to
;;; be implemented if the patch should also receive osc messages from
;;; other sources than the pd patch).
;;;
;;; the src argument otherwise contains the html element which caused
;;; the value change to prevent a loop in the value changes to the
;;; connected html gui instances.
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

;;; all the callback functions are installed in:
;;;
;;; - osc.lisp (define-orgel-fader-responder...)
;;;
;;; - orgel-gui-redefs.lisp (make-orgel-val-receiver, etc., used in
;;;   the init routines of the gui widgets (init-vslider...) in
;;;   utils.lisp). They simply set the cell of the model-slots in
;;;   *curr-state*, triggering gui-redraw and behaviour defined in
;;;   route-presets (like controller actions and such with
;;;   digest-route-preset).

#|

(defun orgel-global-value-callback (orgelidx target value src)
  "callback function if a global value changes (either through gui
interaction or by responding to osc events)."
  (set-cell (slot-value (aref *curr-state* orgelidx) target) value :src src))

(defun orgel-fader-value-callback (orgelidx target faderidx value src)
  "callback function if a fader value changes (either through gui
interaction or by responding to osc events)."
  (let ((f-idx (round (1- faderidx))))
    (set-cell (aref (slot-value (aref *curr-state* orgelidx) target) f-idx) value :src src)))

(defun orgel-mlevel-value-callback (orgelidx faderno value src)
  "callback function if a meter level value changes (by responding to osc
events from the dsp engine)."
  (let ((f-idx (round (1- faderidx))))
    (set-cell (aref (aref *orgel-mlevel* orgelidx) f-idx) value :src src)))
|#

(defun setup-ref-cell-hooks ()
  "Set up propagating changes in the model-slots of *curr-state* and
*orgel-mlevel* to all connected clients (gui and pd via osc). This
 also takes care of calling the route functions defined in presets, so
 triggering all necessary respond actions to gui, cc or osc messages
 boils down to simply setting the ref-cell in *curr-state*."
  (dotimes (orgelidx *orgelcount*)
    (let ((global-orgel (aref *curr-state* orgelidx)))
      (map
       nil
       (lambda (slot-sym slot-key)
         (let ((orgelidx orgelidx))
           (setf (set-cell-hook (slot-value global-orgel slot-sym))
                 (lambda (val &key src)
                   (incudine.util:msg :info "setup-ref-cell-hooks, slot-sym: ~a, val: ~a src: ~a to-server: ~a" slot-sym val src (and val (not (equal src "osc"))))
                   (if (and val (not (equal src "osc")))
                       (global-to-server (orgel-name (1+ orgelidx)) slot-key val))
 ;;; call the defined route functions
                   (dolist (fn (slot-value (aref *osc-responder-registry* orgelidx) slot-sym))
                     (funcall fn val))))))
           *orgel-global-target-syms*
           *orgel-global-targets*)
      (map nil (lambda (slot-sym slot-key)
                 (let ((orgelidx orgelidx))
                   (dotimes (faderidx 16)
                     (let* ((faderidx faderidx)
                            (partial (1+ faderidx)))
                       (setf (set-cell-hook (aref (slot-value global-orgel slot-sym) faderidx))
                             (lambda (val &key src)
                               (incudine.util:msg :info "setup-ref-cell-hooks, slot-sym: ~a, partial: val: ~a src: ~a" slot-sym partial val src)
                               (if (and val (not (equal src "osc")))
                                   (fader-to-server (orgel-name (1+ orgelidx)) slot-key partial val))
;;;                               (format t "setting: ~a ~a ~a ~a~%" (orgel-name (1+ orgelidx)) slot-key (1+ faderidx) val)
 ;;; call the defined route functions
                               (dolist (fn (aref (slot-value (aref *osc-responder-registry* orgelidx) slot-sym) faderidx))
                                 (funcall fn val))))))))
            *orgel-fader-target-syms*
            *orgel-fader-targets*)
      ))
  (register-cc-ref-cell-hooks)
  (register-notein-ref-cell-hooks)
  )

;;; (setup-ref-cell-hooks)
;;; *curr-state*

