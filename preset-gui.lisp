;;; 
;;; preset-gui.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2025 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

;;; (shadowing-import '(create-br title html-document) 'clog)

(defparameter ocpg.targets (append *orgel-global-targets* *orgel-fader-targets*) "Current targets of orgelclient gui.")
(defparameter ocpg.orgelnos (range 1 (1+ *orgelcount*)) "Current orgelnos of orgelclient gui.")


(defparameter ocpg.curr-preset (make-ref 0) "Current preset number of orgelclient gui.")

#|

;;; helper functions:

(defmacro make-ocpg-target-cells ()
  `(progn
     ,@(loop
         for target in ocpg.targets
         collect `(defparameter ,(intern (format nil "~:@(ocpg.~a-btn~)" target)) (make-ref 0)))))

(make-ocpg-target-cells)

(loop for target in (append *orgel-global-targets* *orgel-fader-targets*)
      do (format t "(push (watch (lambda () (if (zerop (get-val ocpg.~a-btn))
                         (setf ocpg.targets (delete :~a ocpg.targets))
                         (pushnew :~a ocpg.targets))))
        ocpg.unwatch)~%" target target target))


(defmacro make-ocpg-target-gui ()
  `(progn
     ,@(loop
         for target in ocpg.targets
         collect `(create-o-toggle body (bind-refs-to-attrs ,(intern (format nil "~:@(ocpg.~a-btn~)" target)) "value")
                      :label '(,(format nil "~a" target)) :css '(:display "inline-block" :height "1.2em" :width "3em")))))

;;; (make-ocpg-target-gui)
|#

(defparameter ocpg.unwatch nil)

(defparameter ocpg.prev-preset-btn (make-bang))
(defparameter ocpg.next-preset-btn (make-bang))
(defparameter ocpg.store-btn (make-bang))
(defparameter ocpg.recall-btn (make-bang))
(defparameter ocpg.save-btn (make-bang))
(defparameter ocpg.load-btn (make-bang))
(defparameter ocpg.prev-preset-btn (make-bang))
(defparameter ocpg.next-preset-btn (make-bang))
(defparameter ocpg.all-targets-btn (make-bang))
(defparameter ocpg.no-target-btn (make-bang))
(defparameter ocpg.all-orgeln-btn (make-bang))
(defparameter ocpg.no-orgel-btn (make-bang))

(defparameter ocpg.base-freq-btn (make-ref 1))
(defparameter ocpg.phase-btn (make-ref 1))
(defparameter ocpg.bias-pos-btn (make-ref 1))
(defparameter ocpg.bias-bw-btn (make-ref 1))
(defparameter ocpg.bias-type-btn (make-ref 1))
(defparameter ocpg.main-btn (make-ref 1))
(defparameter ocpg.min-amp-btn (make-ref 1))
(defparameter ocpg.max-amp-btn (make-ref 1))
(defparameter ocpg.ramp-up-btn (make-ref 1))
(defparameter ocpg.ramp-down-btn (make-ref 1))
(defparameter ocpg.exp-base-btn (make-ref 1))
(defparameter ocpg.level-btn (make-ref 1))
(defparameter ocpg.delay-btn (make-ref 1))
(defparameter ocpg.q-btn (make-ref 1))
(defparameter ocpg.gain-btn (make-ref 1))
(defparameter ocpg.osc-level-btn (make-ref 1))

(defparameter ocpg.orgel1-btn (make-ref 1))
(defparameter ocpg.orgel2-btn (make-ref 1))
(defparameter ocpg.orgel3-btn (make-ref 1))
(defparameter ocpg.orgel4-btn (make-ref 1))
(defparameter ocpg.orgel5-btn (make-ref 1))
(defparameter ocpg.orgel6-btn (make-ref 1))
(defparameter ocpg.orgel7-btn (make-ref 1))
(defparameter ocpg.orgel8-btn (make-ref 1))



(defun ocpg.prev-preset ()
  "decrement ocpg.curr-preset. Protect against loops if used in a watch
context."
  (with-unwatched ((v (get-val ocpg.curr-preset)))
    (if (> v 0)
        (set-val ocpg.curr-preset (1- v)))))

;;; (ocpg.prev-preset)

(defun ocpg.next-preset ()
  "increment ocpg.curr-preset. Protect against loops if used in a watch
context."
  (with-unwatched ((v (get-val ocpg.curr-preset)))
    (if (< v 127)
        (set-val ocpg.curr-preset (1+ v)))))

;;; (ocpg.next-preset)

(defun preset-display (body)
  "On-new-window handler."
  (let ()
    (setf (clog:title (clog:html-document body)) "Orgelclient Presets")
    (clog:create-div body :content "Orgelclient Preset Gui"
                     :css '(:font-size "5em" :height "1.5em"))
    (clog:create-br body)
    (create-o-bang body (bind-refs-to-attrs ocpg.prev-preset-btn "bang")
                   :background '("#bbb" "#888")
                   :label '("prev")
                   :css '(:display "inline-block" :height "1.2em" :width "8em" :margin-bottom "0.5em"))
    (create-o-numbox body (bind-refs-to-attrs ocpg.curr-preset "value")
                   :min "0" :max "127"
                   :css '(:display "inline-block" :height "1.2em" :width "8em"))
    (create-o-bang body (bind-refs-to-attrs ocpg.next-preset-btn "bang")
                   :background '("#bbb" "#888")
                   :label '("next")
                   :css '(:display "inline-block" :height "1.2em" :width "8em"))
    (clog:create-br body)
    (create-o-bang body (bind-refs-to-attrs ocpg.store-btn "bang")
                   :background '("#f99" "#f44")
                   :label '("store")
                   :css '(:display "inline-block" :height "1.2em" :width "8em"  :margin-bottom "0.5em"))
    (create-o-bang body (bind-refs-to-attrs ocpg.recall-btn "bang")
                   :background '("#9f9" "#090")
                   :label '("recall")
                   :css '(:display "inline-block" :height "1.2em" :width "8em"))
    (clog:create-br body)
    (create-o-bang body (bind-refs-to-attrs ocpg.save-btn "bang")
                   :background '("#f99" "#f44")
                   :label '("save")
                   :css '(:display "inline-block" :height "1.2em" :width "8em" :margin-bottom "0.5em"))
    (create-o-bang body (bind-refs-to-attrs ocpg.load-btn "bang")
                   :background '("#9f9" "#090")
                   :label '("load")
                   :css '(:display "inline-block" :height "1.2em" :width "8em"))

    (clog:create-br body)
    (clog:create-div body :content "Targets"
                          :css '(:font-size "2em" :height "1.5em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.base-freq-btn "value") :label
                     '("base-freq") :css
                     '(:display "inline-block" :height "1.2em" :width "8em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.phase-btn "value") :label
                     '("phase") :css
                     '(:display "inline-block" :height "1.2em" :width "8em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.bias-pos-btn "value") :label
                     '("bias-pos") :css
                     '(:display "inline-block" :height "1.2em" :width "8em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.bias-bw-btn "value") :label
                     '("bias-bw") :css
                     '(:display "inline-block" :height "1.2em" :width "8em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.bias-type-btn "value") :label
                     '("bias-type") :css
                     '(:display "inline-block" :height "1.2em" :width "8em"))
    (clog:create-br body)
    (create-o-toggle body (bind-refs-to-attrs ocpg.main-btn "value") :label
                     '("main") :css
                     '(:display "inline-block" :height "1.2em" :width "8em"))
    (clog:create-br body)
    (create-o-toggle body (bind-refs-to-attrs ocpg.min-amp-btn "value") :label
                     '("min-amp") :css
                     '(:display "inline-block" :height "1.2em" :width "8em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.max-amp-btn "value") :label
                     '("max-amp") :css
                     '(:display "inline-block" :height "1.2em" :width "8em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.ramp-up-btn "value") :label
                     '("ramp-up") :css
                     '(:display "inline-block" :height "1.2em" :width "8em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.ramp-down-btn "value") :label
                     '("ramp-down") :css
                     '(:display "inline-block" :height "1.2em" :width "8em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.exp-base-btn "value") :label
                     '("exp-base") :css
                     '(:display "inline-block" :height "1.2em" :width "8em"))
    (clog:create-br body)
    (create-o-toggle body (bind-refs-to-attrs ocpg.level-btn "value") :label
                     '("level") :css
                     '(:display "inline-block" :height "1.2em" :width "8em" :margin-bottom "0.5em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.delay-btn "value") :label
                     '("delay") :css
                     '(:display "inline-block" :height "1.2em" :width "8em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.q-btn "value")
                     :label '("q")
                     :css '(:display "inline-block" :height "1.2em" :width "8em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.gain-btn "value") :label
                     '("gain") :css
                     '(:display "inline-block" :height "1.2em" :width "8em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.osc-level-btn "value") :label
                     '("osc-level") :css
                     '(:display "inline-block" :height "1.2em" :width "8em"))
    (clog:create-br body)
    (create-o-bang body (bind-refs-to-attrs ocpg.all-targets-btn "bang")
                   :background '("#bbb" "#888")
                   :label '("all targets")
                   :css '(:display "inline-block" :height "1.2em" :width "8em"
                          :margin-bottom "0.5em"))
    (create-o-bang body (bind-refs-to-attrs ocpg.no-target-btn "bang")
                   :label '("no target")
                   :background '("#bbb" "#888")
                   :css '(:display "inline-block" :height "1.2em" :width "8em"))
    (clog:create-br body)
    (clog:create-div body :content "Orgeln"
                          :css '(:font-size "2em" :height "1.5em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.orgel1-btn "value") :label
                     '("Orgel01") :css
                     '(:display "inline-block" :height "1.2em" :width "8em" :margin-bottom "0.5em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.orgel2-btn "value") :label
                     '("Orgel02") :css
                     '(:display "inline-block" :height "1.2em" :width "8em" :margin-bottom "0.5em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.orgel3-btn "value")
                     :label '("Orgel03")
                     :css '(:display "inline-block" :height "1.2em" :width "8em" :margin-bottom "0.5em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.orgel4-btn "value") :label
                     '("Orgel04") :css
                     '(:display "inline-block" :height "1.2em" :width "8em" :margin-bottom "0.5em"))
    (clog:create-br body)
    (create-o-toggle body (bind-refs-to-attrs ocpg.orgel5-btn "value") :label
                     '("Orgel05") :css
                     '(:display "inline-block" :height "1.2em" :width "8em" :margin-bottom "0.5em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.orgel6-btn "value") :label
                     '("Orgel06") :css
                     '(:display "inline-block" :height "1.2em" :width "8em" :margin-bottom "0.5em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.orgel7-btn "value") :label
                     '("Orgel07") :css
                     '(:display "inline-block" :height "1.2em" :width "8em" :margin-bottom "0.5em"))
    (create-o-toggle body (bind-refs-to-attrs ocpg.orgel8-btn "value") :label
                     '("Orgel08") :css
                     '(:display "inline-block" :height "1.2em" :width "8em" :margin-bottom "0.5em"))

    (clog:create-br body)
    (create-o-bang body (bind-refs-to-attrs ocpg.all-orgeln-btn "bang")
                   :background '("#bbb" "#888")
                   :label '("all orgeln")
                   :css '(:display "inline-block" :height "1.2em" :width "8em"))
    (create-o-bang body (bind-refs-to-attrs ocpg.no-orgel-btn "bang")
                   :label '("no orgel")
                   :background '("#bbb" "#888")
                   :css '(:display "inline-block" :height "1.2em" :width "8em"))
    ))

(clog:set-on-new-window 'preset-display :path "/presets")

(defun setup-ocpg-connections ()
  (dolist (fn ocpg.unwatch) (funcall fn))
  (push (watch (lambda () (get-val ocpg.prev-preset-btn) (ocpg.prev-preset))) ocpg.unwatch)
  (push (watch (lambda () (get-val ocpg.next-preset-btn) (ocpg.next-preset))) ocpg.unwatch)
  (push (watch (lambda () (get-val ocpg.store-btn)
                 (with-unwatched ((num (round (get-val ocpg.curr-preset))))
                   (store-preset num :targets ocpg.targets :orgelnos ocpg.orgelnos))))
        ocpg.unwatch)
  (push (watch (lambda () (get-val ocpg.recall-btn)
                 (with-unwatched ((num (round (get-val ocpg.curr-preset))))
                   (recall-preset num :targets ocpg.targets :orgelnos ocpg.orgelnos))))
        ocpg.unwatch)
  (push (watch (lambda () (get-val ocpg.save-btn) (save-orgel-presets))) ocpg.unwatch)
  (push (watch (lambda () (get-val ocpg.load-btn) (load-orgel-presets))) ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.base-freq-btn))
                         (setf ocpg.targets (delete :base-freq ocpg.targets))
                         (pushnew :base-freq ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.phase-btn))
                         (setf ocpg.targets (delete :phase ocpg.targets))
                         (pushnew :phase ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.bias-pos-btn))
                         (setf ocpg.targets (delete :bias-pos ocpg.targets))
                         (pushnew :bias-pos ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.bias-bw-btn))
                         (setf ocpg.targets (delete :bias-bw ocpg.targets))
                         (pushnew :bias-bw ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.bias-type-btn))
                         (setf ocpg.targets (delete :bias-type ocpg.targets))
                         (pushnew :bias-type ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.main-btn))
                         (setf ocpg.targets (delete :main ocpg.targets))
                         (pushnew :main ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.min-amp-btn))
                         (setf ocpg.targets (delete :min-amp ocpg.targets))
                         (pushnew :min-amp ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.max-amp-btn))
                         (setf ocpg.targets (delete :max-amp ocpg.targets))
                         (pushnew :max-amp ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.ramp-up-btn))
                         (setf ocpg.targets (delete :ramp-up ocpg.targets))
                         (pushnew :ramp-up ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.ramp-down-btn))
                         (setf ocpg.targets (delete :ramp-down ocpg.targets))
                         (pushnew :ramp-down ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.exp-base-btn))
                         (setf ocpg.targets (delete :exp-base ocpg.targets))
                         (pushnew :exp-base ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.level-btn))
                         (setf ocpg.targets (delete :level ocpg.targets))
                         (pushnew :level ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.delay-btn))
                         (setf ocpg.targets (delete :delay ocpg.targets))
                         (pushnew :delay ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.q-btn))
                         (setf ocpg.targets (delete :q ocpg.targets))
                         (pushnew :q ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.gain-btn))
                         (setf ocpg.targets (delete :gain ocpg.targets))
                         (pushnew :gain ocpg.targets))))
        ocpg.unwatch)
  (push (watch (lambda ()
                 (get-val ocpg.all-targets-btn)
                 (dolist  (btn
                           (list ocpg.base-freq-btn
                                 ocpg.phase-btn ocpg.bias-pos-btn
                                 ocpg.bias-bw-btn ocpg.bias-type-btn
                                 ocpg.main-btn ocpg.min-amp-btn
                                 ocpg.max-amp-btn ocpg.ramp-up-btn
                                 ocpg.ramp-down-btn ocpg.exp-base-btn
                                 ocpg.level-btn
                                 ocpg.delay-btn
                                 ocpg.q-btn
                                 ocpg.gain-btn
                                 ocpg.osc-level-btn))
                   (set-val btn 1))))
        ocpg.unwatch)
  (push (watch (lambda ()
                 (get-val ocpg.no-target-btn)
                 (dolist  (btn
                           (list ocpg.base-freq-btn
                                 ocpg.phase-btn ocpg.bias-pos-btn
                                 ocpg.bias-bw-btn ocpg.bias-type-btn
                                 ocpg.main-btn ocpg.min-amp-btn
                                 ocpg.max-amp-btn ocpg.ramp-up-btn
                                 ocpg.ramp-down-btn ocpg.exp-base-btn
                                 ocpg.level-btn
                                 ocpg.delay-btn
                                 ocpg.q-btn
                                 ocpg.gain-btn
                                 ocpg.osc-level-btn))
                   (set-val btn 0))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.orgel1-btn))
                         (setf ocpg.orgelnos (delete 1 ocpg.orgelnos))
                         (pushnew 1 ocpg.orgelnos))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.orgel2-btn))
                         (setf ocpg.orgelnos (delete 2 ocpg.orgelnos))
                         (pushnew 2 ocpg.orgelnos))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.orgel3-btn))
                         (setf ocpg.orgelnos (delete 3 ocpg.orgelnos))
                         (pushnew 3 ocpg.orgelnos))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.orgel4-btn))
                         (setf ocpg.orgelnos (delete 4 ocpg.orgelnos))
                         (pushnew 4 ocpg.orgelnos))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.orgel5-btn))
                         (setf ocpg.orgelnos (delete 5 ocpg.orgelnos))
                         (pushnew 5 ocpg.orgelnos))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.orgel6-btn))
                         (setf ocpg.orgelnos (delete 6 ocpg.orgelnos))
                         (pushnew 6 ocpg.orgelnos))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.orgel7-btn))
                         (setf ocpg.orgelnos (delete 7 ocpg.orgelnos))
                         (pushnew 7 ocpg.orgelnos))))
        ocpg.unwatch)
  (push (watch (lambda () (if (zerop (get-val ocpg.orgel8-btn))
                         (setf ocpg.orgelnos (delete 8 ocpg.orgelnos))
                         (pushnew 8 ocpg.orgelnos))))
        ocpg.unwatch)

  (push (watch (lambda ()
                 (get-val ocpg.all-orgeln-btn)
                 (dolist  (btn (list
                                ocpg.orgel1-btn
                                ocpg.orgel2-btn
                                ocpg.orgel3-btn
                                ocpg.orgel4-btn
                                ocpg.orgel5-btn
                                ocpg.orgel6-btn
                                ocpg.orgel7-btn
                                ocpg.orgel8-btn
                                ))
                   (set-val btn 1))))
        ocpg.unwatch)
  (push (watch (lambda ()
                 (get-val ocpg.no-orgel-btn)
                 (dolist  (btn (list
                                ocpg.orgel1-btn
                                ocpg.orgel2-btn
                                ocpg.orgel3-btn
                                ocpg.orgel4-btn
                                ocpg.orgel5-btn
                                ocpg.orgel6-btn
                                ocpg.orgel7-btn
                                ocpg.orgel8-btn
                                ))
                   (set-val btn 0))))
        ocpg.unwatch))

(setup-ocpg-connections)
(trigger ocpg.all-targets-btn)
(trigger ocpg.all-orgeln-btn)
