;;; 
;;; presets.lisp
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

;;; *orgel-presets* currently just contains the static positions of
;;; all faders/numboxes, but no routing/relation of faders!

(defparameter *orgel-presets*
  (make-array
   128
   :initial-contents
   (loop
     for i below 128
     collect (make-array
              *orgelcount*
              :initial-contents
              (loop
                for x below *orgelcount*
                collect (make-val-orgel))))))

(defun copy-orgel-preset (src target)
  (dotimes (i *orgelcount*)
    (setf (aref target i)
          (copy-orgel (aref src i)))))

;;; (copy-orgel-preset *curr-state* (aref *orgel-presets* 0))
;;;(aref *curr-state* 1)
#|
(defun recall-orgel (orgelidx num &optional next interp)
  "recall the complete state of orgel at orgelidx from preset <num> by
sending the values using osc. <interp> is a value between 0 and 1
interpolating all values between presets <num> and <next>."
  (dolist (slot '(:level :delay :q :gain :osc-level)) ;;; sliders
    (dotimes (i 16) ;;; iterate over all 16 sliders
      (let* (;; (g-accessor (cl-orgel-gui::slot->function "g-orgel" slot))
             (val (if next
                      (+
                       (* (if interp (- 1 interp) 0.5)
                          (aref (funcall (val-orgel-access-fn slot)
                                         (aref (aref *orgel-presets* num) orgelidx))
                                i))
                       (* (or interp 0.5)
                          (aref (funcall (val-orgel-access-fn slot)
                                         (aref (aref *orgel-presets* next) orgelidx))
                                i)))
                      (aref (funcall (val-orgel-access-fn slot)
                                     (aref (aref *orgel-presets* num) orgelidx))
                            i))))
        (orgel-ctl-fader (orgel-name (1+ orgelidx)) slot (1+ i) val))))

  (dolist (slot *orgel-global-targets*) ;;; global slots
    (let* (;;; (slot-symbol (intern (format nil "~:@(~a~)" slot) 'cl-orgel-gui))
           (val (if next
                    (+
                     (* (if interp (- 1 interp) 0.5)
                        (funcall (val-orgel-access-fn slot)
                                 (aref (aref *orgel-presets* num) orgelidx)))
                     (* (or interp 0.5)
                        (funcall (val-orgel-access-fn slot)
                                 (aref (aref *orgel-presets* next) orgelidx))))
                    (funcall (val-orgel-access-fn slot) (aref (aref *orgel-presets* num) orgelidx)))))
      (if *debug* (format t "sending: orgel~2,'0d: ~a ~a~%" (1+ orgelidx) slot val))
      (orgel-ctl (orgel-name (1+ orgelidx)) slot val))))

;;; (recall-orgel 0 2)

(defun recall-orgel-preset (num &optional next interp)
  "recall preset /num/ of /*orgel-presets*/ into *curr-state*".
  (when num
    (loop for orgel below *orgelcount*
          for time from 0 by 0.02
          do (let ((orgel orgel))
               (cm::at (+ (cm:now) time)
                       (lambda () (recall-orgel orgel num next interp)))))
    (let ((preset (elt *orgel-presets* num)))
      (dotimes (idx *orgelcount*)
        (val-orgel->model-orgel (aref preset idx) (aref *curr-state* idx) )))))

;;; (recall-orgel-preset 0)

(defun store-orgel-preset (num &key (presets *orgel-presets*))
  "store *curr-state* into preset /num/ of /*orgel-presets*/".
  (let ((preset (aref presets num)))
    (dotimes (idx *orgelcount*)
      (format t "~a, " idx)
      (setf (aref preset idx) (model-orgel->val-orgel (aref *curr-state* idx) )))))
|#

(defun save-orgel-presets (&optional (file *orgel-presets-file*))
  "save all presets from /*orgel-presets*/ to /file/."
  (with-open-file (out file :direction :output :if-exists :supersede)
    (format out "(in-package :cl-orgelctl)~%(setf *orgel-presets* ~%~a)" *orgel-presets*)))

;;; (save-orgel-presets)
(defun load-orgel-presets (&optional (file *orgel-presets-file*))
  "load all presets from /file/ into /*orgel-presets*/."  
  (load file))

(defun map-curr-state (result-type fn &key (orgelnos (mapcar #'1+ (range *orgelcount*)))
                                        (targets (append *orgel-global-targets* *orgel-fader-targets*)))
  "map fn over all slots of *curr-state*. Filter by /orgelnos/ and
/targets/. The function is called with 3 (global targets) or 4 (fader
targets) arguments: (slot orgelnos target &optional faderno). If
result-type is non-nil, return the results of fn in a sequence of type
/result-type/, similar to common lisp's #'map."
  (let ((result nil))
    (dolist (target targets)
      (dolist (orgelno orgelnos)
        (if (member target *orgel-global-targets*)
            (if result-type
                (push (funcall fn (slot-value (aref *curr-state* (1- orgelno)) (intern (symbol-name target))) orgelno target) result)
                (funcall fn (slot-value (aref *curr-state* (1- orgelno)) (intern (symbol-name target))) orgelno target))
            (dotimes (fader-idx 16)
              (if result-type
                  (push (funcall fn (slot-value (aref *curr-state* (1- orgelno)) (intern (symbol-name target)))
                                 orgelno target (1+ fader-idx))
                        result)
                  (funcall fn (slot-value (aref *curr-state* (1- orgelno)) (intern (symbol-name target)))
                           orgelno target (1+ fader-idx)))))))
    (if result-type (coerce (reverse result) result-type))))

;;; (map-curr-state '() (lambda (slot &rest args) (list args (setf (val slot) 0))) :targets '(:level))
;;; (map-curr-state 'list (lambda (slot orgelidx target &optional faderidx) (list orgelidx target faderidx (setf (val slot) 0))) :targets '(:level))

(defun cp-preset (srcno destno &key (orgelnos (mapcar #'1+ (range *orgelcount*)))
                             (targets (append *orgel-global-targets* *orgel-fader-targets*)))
  "cp the values of /targets/ of orgeln /orgelnos/ from index /srcno/ in *orgel-presets* to /destno/ of *orgel-presets*."
  (let ((src (aref *orgel-presets* srcno))
        (dest (aref *orgel-presets* destno)))
    (dolist (orgelno orgelnos)
      (dolist (target targets)
        (if (member target *orgel-global-targets*)
            (setf (slot-value (aref dest (1- orgelno)) (intern (symbol-name target)))
                  (slot-value (aref src (1- orgelno)) (intern (symbol-name target))))
            (dotimes (fader-idx 16)
              (setf (aref (slot-value (aref dest (1- orgelno)) (intern (symbol-name target))) fader-idx)
                    (aref (slot-value (aref src (1- orgelno)) (intern (symbol-name target))) fader-idx))))))))

(defun store-preset (destno &key (orgelnos (mapcar #'1+ (range *orgelcount*)))
                              (targets (append *orgel-global-targets* *orgel-fader-targets*)))
  "store the values of *curr-state* into preset number /destno/ of
*orgel-presets*. Filter by /orgelno/ and /targets/."
  (let ((dest (aref *orgel-presets* destno)))
    (map-curr-state
     nil
     (lambda (slot orgelno target &optional partial)
       (if (member target *orgel-global-targets*)
           (setf (slot-value (aref dest (1- orgelno)) (intern (symbol-name target)))
                 (val slot))
           (setf (aref (slot-value (aref dest (1- orgelno)) (intern (symbol-name target))) (1- partial))
                 (val (aref slot (1- partial))))))
     :orgelnos orgelnos
     :targets targets)))

;;; (store-preset 2)


(defun recall-preset (srcno &key (orgelnos (mapcar #'1+ (range *orgelcount*)))
                              (targets (append *orgel-global-targets* *orgel-fader-targets*)))
  "recall the values of preset number /srcno/ of *orgel-presets* into
*curr-state*. Filter by /orgelno/ and /targets/."
  (let ((src (aref *orgel-presets* srcno)))
    (map-curr-state
     nil
     (lambda (slot orgelno target &optional partial)
       (if (member target *orgel-global-targets*)
           (setf (val slot)
                 (slot-value (aref src (1- orgelno)) (intern (symbol-name target))))
           (setf (val (aref slot (1- partial)))
                 (aref (slot-value (aref src (1- orgelno)) (intern (symbol-name target))) (1- partial)))))
     :orgelnos orgelnos
     :targets targets)))

;;; (recall-preset 2)



;;; (orgel-ctl 1 :level 1 (random 128))

(defparameter *route-presets* (make-array 128 :initial-element nil :element-type 'list))

(defun digest-route-preset (preset-num form &key (reset t))
  (setf (aref *route-presets* preset-num) form)
  (recall-orgel-preset (getf form :preset))
  (digest-routes (getf form :routes) :reset reset))

(defun save-route-presets (&optional (file *route-presets-file*))
  (with-open-file (out file :direction :output :if-exists :supersede)
    (format out "(in-package :cl-orgelctl)~%~%(setf *route-presets*~%~S)" *route-presets*)))

(defun load-route-presets (&optional (file *route-presets-file*))
  (load file))

(defun recall-route-preset (num)
  (let ((form (aref *route-presets* num)))
    (recall-orgel-preset (getf form :preset))
    (digest-routes (getf form :routes))
))

;;; (recall-route-preset 0)
;;; (save-route-presets)

#+swank
(defparameter *emcs-conn* swank::*emacs-connection*)

#+swank
(defun define-elisp-code ()
  (let ((swank::*emacs-connection* *emcs-conn*))
    (swank::eval-in-emacs
     `(progn
        (setq orgel-preset-file ,(namestring (merge-pathnames "curr-preset.lisp" (asdf:system-source-directory :cl-orgelctl-client))))
        (find-file orgel-preset-file)
        (set-window-dedicated-p (get-buffer-window "curr-preset.lisp" t) t)
        (load ,(namestring (merge-pathnames "edit-orgel-presets.el" (asdf:system-source-directory :cl-orgelctl-client))))
        ) t)))

#+slynk
(defun define-elisp-code ()
  (slynk::eval-in-emacs
   `(progn
      (setq orgel-preset-file ,(namestring (merge-pathnames "curr-preset.lisp" (asdf:system-source-directory :cl-orgelctl-client))))
      (find-file orgel-preset-file)
      (set-window-dedicated-p (get-buffer-window "curr-preset.lisp" t) t)
      (load ,(namestring (merge-pathnames "sly-edit-orgel-presets.el" (asdf:system-source-directory :cl-orgelctl-client))))
      ) t))


(defun preset->string (preset-form ref)
  (format nil "(digest-route-preset~%~d~%`(~S ~d~%~S ~S))"
          ref
          (first preset-form)
          (second preset-form)
          (third preset-form)
          (fourth preset-form)))

#+swank
(defun edit-preset-in-emacs (ref &key (presets *route-presets*))
  "send the preset form referenced by <ref> to emacs for display in the
curr-preset.lisp buffer."
  (let ((swank::*emacs-connection* *emcs-conn*))
    (if (numberp ref)
        (swank::eval-in-emacs
         `(edit-orgelctl-preset
           ,(progn
              (in-package :cl-orgelctl)
              (defparameter swank::*send-counter* 0)
              (preset->string (aref presets ref) ref))
           ,ref) t)
        (swank::eval-in-emacs
         `(save-excursion
           (switch-to-buffer (get-buffer "curr-preset.lisp"))) t))))

#+slynk
(defun edit-preset-in-emacs (ref &key (presets *route-presets*))
  "send the preset form referenced by <ref> to emacs for display in the
curr-preset.lisp buffer."
  (if (numberp ref)
      (slynk::eval-in-emacs
       `(edit-orgelctl-preset
         ,(progn
            (in-package :cl-orgelctl)
            (preset->string (aref presets ref) ref))
         ,ref) t)
      (slynk::eval-in-emacs
       `(save-excursion
         (switch-to-buffer (get-buffer "curr-preset.lisp"))) t)))

;;; (define-elisp-code)

(defparameter *curr-orgel-preset-nr* 0)
(defparameter *max-orgel-preset-nr* 127)
(defparameter *curr-route-preset-nr* 0)
(defparameter *max-route-preset-nr* 127)

(defun next-orgel-preset ()
  (if (< *curr-orgel-preset-nr* *max-orgel-preset-nr*)
      (incudine.osc:message *oscout* "/preset-ctl/preset-no" "i" (incf *curr-orgel-preset-nr*))))

(defun previous-orgel-preset ()
  (if (> *curr-orgel-preset-nr* 0)
      (incudine.osc:message *oscout* "/preset-ctl/preset-no" "i" (decf *curr-orgel-preset-nr*))))

(defun next-route-preset ()
  (if (< *curr-route-preset-nr* *max-route-preset-nr*)
      (edit-preset-in-emacs (incf *curr-route-preset-nr*))))

(defun previous-route-preset ()
  (if (> *curr-route-preset-nr* 0)
      (edit-preset-in-emacs (decf *curr-route-preset-nr*))))

;;; (previous-route-preset)
;;; (next-route-preset)


