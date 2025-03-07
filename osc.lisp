;;; 
;;; osc.lisp
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

(defparameter *local-host* "127.0.0.1")
(defparameter *remote-host* "127.0.0.1")
(defparameter *local-port* 3016)
(defparameter *remote-port* 3011)
(defparameter *client-id* nil)

(defparameter *oscout* nil)
(defparameter *oscin* nil)

(defun connect-to-server (&key (remote-host *remote-host*)
                            (remote-port *remote-port*)
                            (local-host *local-host*)
                            (local-port *local-port*)
                            (protocol :udp))
  (when *oscin* (incudine.osc:close *oscin*))
  (setf *oscin* (incudine.osc:open :host local-host :port local-port :protocol protocol))
  (make-all-responders)
  (incudine:recv-start *oscin*)
  (when *oscout* (incudine.osc:close *oscout*))
  (setf *oscout* (incudine.osc:open :host remote-host :port remote-port :protocol protocol :direction :output))
  (unless (eq (incudine:rt-status) :started)
    (error "incudine rt not running! Please start first!"))
  (incudine:at (incudine:now) #'incudine.osc:message *oscout*
               "/connect-client" "si" local-host local-port))

  (incudine:at (incudine:now) #'incudine.osc:message *oscout*
    "/connect-client" "si" *local-host* *local-port*)

(defun disconnect-from-server ()
  (incudine:at (incudine:now) #'incudine.osc:message *oscout*
               (format nil "/disconnect-client") "s" *client-id*)
  (setf *client-id* nil))

(defparameter *orgel-osc-responder* (make-hash-table)) ;;; a hashtable with the handles of all orgel responders


;;; (incudine.osc:close *oscin*)
;; (progn
;;   (incudine.osc:close *oscout*)
;;   (incudine.osc:close *oscin*))

;;; registry for route functions to be called on incoming osc messages

(defparameter *osc-responder-registry*
  (make-array *orgelcount*
              :initial-contents (loop
                                  for i below *orgelcount*
                                  collect (make-instance 'orgel-registry))))
#|
(loop for target in *orgel-global-targets*
collect `(setf (,(read-from-string (format nil "orgel-registry-~a" target)) (aref *orgel-responder-registry* orgelidx)) nil))

|#

;;; The funcalls are spelled out for speed. Too lazy to do it with a macro...

(defmacro %clear-osc-responder-registry ()
  "clear all function calls to be called on osc message receive."
  `(loop
     for orgelidx below *orgelcount*
     do (progn
          ,@(mapcar (lambda (sym) `(setf (slot-value (aref *osc-responder-registry* orgelidx) ',sym) nil)) *orgel-global-target-syms*)
          ,@(mapcar (lambda (sym) `(dotimes (idx 16)
                                (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) ',sym) idx) nil)))
                    (append '(bias-level mlevel) *orgel-fader-target-syms*)))))

;;; (clear-osc-responder-registry)

(defun clear-osc-responder-registry ()
  (%clear-osc-responder-registry))
#|
(defun clear-osc-responder-registry ()
  "clear all function calls to be called on osc message receive."
  (loop
    for orgelidx below *orgelcount*
    do (progn
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'base-freq) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'phase) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'main) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'bias-pos) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'bias-bw) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'bias-type) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'min-amp) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'max-amp) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'ramp-up) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'ramp-down) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'exp-base) nil)
         (dotimes (idx 16)
           (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'level) idx) nil))
         (dotimes (idx 16)
           (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'bias-level) idx) nil))
         (dotimes (idx 16)
           (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'mlevel) idx) nil))
         (dotimes (idx 16)
           (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'delay) idx) nil))
         (dotimes (idx 16)
           (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'q) idx) nil))
         (dotimes (idx 16)
           (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'gain) idx) nil))
         (dotimes (idx 16)
(setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'osc-level) idx) nil)))))
|#


#|
(defmacro struct-accessor (struct-name slot instance)
  `(,(intern (string-upcase (format nil "~a-~a" struct-name slot))) ,instance))

;;; (struct-accessor :orgel :bias '*test*)

(defun struct-accessor (struct-name slot instance)
  (list (intern (string-upcase (format nil "~a-~a" struct-name slot))) instance))

(let ((orgelidx 1))
  (mapcar #'funcall (orgel-registry-base-freq
                     (aref *osc-responder-registry* (1- orgelidx)))))
|#

;;; registry of orgel-responder functions:

;;; We define generic responders for all incoming osc messages. They
;;; simply map over all functions of the respective entry of the
;;; incoming message in *osc-responder-registry*.
;;;
;;; To make this more efficient, macros are used to expand all the
;;; function definition code in order to reduce indirection/lookup
;;; overhead. On intialisation of the package, the macro
;;; make-all-responders should be called once with the number of
;;; organs to observe.
;;;
;;; Installing functions to be called boils down to pushing them to
;;; the lists of the respective entries in *osc-responder-registry*.

#|
(defmacro define-orgel-fader-responder (stream orgelidx target)
  "responder for the fader controllers of the 16 partials (level, delay,
bp, gain, osc-level)."
  `(list ,target
         (let ((target-sym ',(read-from-string (format nil "~a" (symbol-value target)))))
           (declare (ignorable target-sym))
           (incudine:make-osc-responder
            ,stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) (symbol-value target)) "ff"
            (lambda (faderno value)
              (incudine.util:msg :info "osc-in: /orgel~2,'0d/~a ~a ~a~%" ,(1+ orgelidx) ,target faderno value)
              (set-cell (aref (slot-value (aref *curr-state* ,orgelidx) target-sym) (round (1- (round faderno)))) value :src "osc"))))))

;;; (define-orgel-fader-responder 'osc-stream 0 :base-freq)


(defmacro get-orgel-fader-responders (stream orgelidx targets)
  `(append
     ,@(loop for target in (symbol-value targets)
             collect `(define-orgel-fader-responder ,stream ,orgelidx ,target))))

(defmacro define-orgel-global-responder (stream orgelidx target)
  "responder for the global parameters of the organ (ramps, base-freq,
amps, etc.)"
  `(list ,target
         (let ((target-sym ',(read-from-string (format nil "~a" (symbol-value target)))))
           (incudine:make-osc-responder
            ,stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) (symbol-value target)) "f"
            (lambda (value)
              (incudine.util:msg :info "osc-in: /orgel~2,'0d/~a ~a~%" ,(1+ orgelidx) ,target value)
              (set-cell (slot-value (aref *curr-state* ,orgelidx) target-sym) value :src "osc"))))))

;;; (define-orgel-global-responder 'osc-stream 0 :base-freq)

(defmacro get-orgel-global-responders (stream orgelidx targets)
  `(append
     ,@(loop for target in (symbol-value targets)
             collect `(define-orgel-global-responder ,stream ,orgelidx ,target))))

;;; (define-orgel-global-responder 'osc-stream 0 :base-freq)

;;; (get-orgel-global-responders *oscin* 0 *orgel-global-targets*)
|#

(defmacro define-orgel-level-meter-responder (stream orgelidx)
  "responder for the 16 output level meters."
  `(list :mlevel
         (incudine:make-osc-responder
          ,stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) 'mlevel) "ff"
          (lambda (faderno value)
            (let ((f-idx (round (1- faderno))))
              (set-cell (aref (aref *orgel-mlevel* ,orgelidx) f-idx) value :src "osc"))
            (incudine.util:msg :info "orgel~2,'0d: ~a ~a ~a~%" ,(1+ orgelidx) :mlevel (round faderno) value)))))

;;; (define-orgel-level-meter-responder *oscin* 0 :mlevel)

(defmacro define-orgel-ccin-responder (stream)
  "responder for external ccin."
  `(list :ccin
         (incudine:make-osc-responder
          ,stream "/ccin"  "fff"
          (lambda (ccval ccnum channel)
            (setf (val (aref (aref *midi-cc-state* (round channel)) (round ccnum))) (round ccval))
            (incudine.util:msg :info "ccin: ~a ~a ~a~%" ccnum ccval channel)))))

(defmacro define-orgel-notein-responder (stream)
  "responder for external notein."
  `(list :notein
         (incudine:make-osc-responder
          ,stream "/notein"  "fff"
          (lambda (keynum velo channel)
            (incudine.util:msg :info "notein: ~a ~a ~a~%" keynum velo channel)
            (setf (val (aref (aref *midi-note-state* (round channel)) (round keynum))) (round velo))))))

(defmacro define-preset-responder (stream path fn)
  `(incudine:make-osc-responder
    ,stream
    ,(format nil "/preset-ctl/~a" path)
    ""
    (lambda ()
      ,fn
      (incudine.util:msg :info "preset-ctl: ~a~%" ,path))))

(defmacro get-preset-responders (stream)
  `(progn
     (define-preset-responder ,stream "prev-preset" '(previous-orgel-preset))
     (define-preset-responder ,stream "next-preset" '(next-orgel-preset))
     (define-preset-responder ,stream "recall-preset" '(recall-orgel-preset *curr-orgel-preset-nr*))
     (define-preset-responder ,stream "store-preset" '(store-orgel-preset *curr-orgel-preset-nr*))
     (define-preset-responder ,stream "load-presets" '(load-orgel-presets))
     (define-preset-responder ,stream "save-presets" '(save-orgel-presets))
     (incudine:make-osc-responder
      ,stream
      "/preset-ctl/preset-no" "f"
      (lambda (f)
        (setf *curr-orgel-preset-nr* (round f))
        (incudine.util:msg :info "preset-ctl: preset-no ~a~%" (round f))))))

;; (get-preset-responders *oscin*)

(defmacro define-orgel-plist-responders (stream)
  `(let ((curr-plist nil) (trigger nil))
     (incudine.util:msg :info "defining plist responders on ~a." ,stream)
     (incudine:make-osc-responder
      ,stream "/plist-ctl/start" ""
      (lambda ()
        (setf curr-plist nil)
        (incudine.util:msg :info "plist-ctl: start")))
     (incudine:make-osc-responder
      ,stream "/plist-ctl/stop" ""
      (lambda ()
        (incudine.util:msg :info "plist: ~a" curr-plist)
        (switch-targets (reverse curr-plist) :old '*global-targets* :trigger trigger)
        (setf curr-plist nil)
        (incudine.util:msg :info "plist-ctl: stop~%")))
     (incudine:make-osc-responder
      ,stream "/plist-ctl/fader" "sfff"
      (lambda (target orgelno partial value)
        (push (list (my-make-symbol (string-upcase target)) (round orgelno) (round partial) value) curr-plist)
        (incudine.util:msg :info "plist-ctl/fader: ~a" (list (my-make-symbol (string-upcase target)) (round orgelno) (round partial) value) curr-plist orgelno partial value)))))

(defmacro define-orgel-flist-responders (stream)
  `(let ((curr-flist nil)
         (process-flist-fn (make-process-flist-fn)))
     (incudine.util:msg :info "defining flist responders on ~a." ,stream)
     (incudine:make-osc-responder
      ,stream "/flist-ctl/start" ""
      (lambda ()
        (setf curr-flist nil)
        (incudine.util:msg :info "flist-ctl: start")))
     (incudine:make-osc-responder
      ,stream "/flist-ctl/stop" ""
      (lambda ()
        (incudine.util:msg :info "flist: ~a" curr-flist)
        (funcall process-flist-fn (reverse curr-flist))
        (setf curr-flist nil)
        (incudine.util:msg :info "flist-ctl: stop~%")))
     (incudine:make-osc-responder
      ,stream "/flist-ctl/freq-amp" "ff"
      (lambda (freq amp)
        (push (list freq amp) curr-flist)
        (incudine.util:msg :info "flist-ctl/freq-amp: ~a" curr-flist)))))

(defparameter *count* 0)

(defmacro %make-all-responders (&optional (stream '*oscin*))
  (let ((maxorgel (symbol-value '*orgelcount*)))
    `(progn
       (incudine:remove-all-responders ,stream)
       (incudine.util:with-logger (:level :info)
         (incudine.util:msg :info "making osc-responders"))
       (get-preset-responders ,stream)
       (define-orgel-plist-responders ,stream)
       (define-orgel-flist-responders ,stream)
       (incudine::make-osc-responder
        ,stream "/orgelctlfader" "sisif"
        (lambda (src orgelno target faderno value)
          (incf *count*)
          (incudine.util:msg :info "orgelctlfader in: ~S ~a ~S ~a ~a" src orgelno  target faderno value)
          (set-cell (aref (slot-value (aref *curr-state* (1- orgelno)) (target-string->sym target)) (1- faderno)) value :src src)))
       (incudine::make-osc-responder
        ,stream "/orgelctl" "sisf"
        (lambda (src orgelno target value)
          (incf *count*)
          (incudine.util:msg :info "orgelctl in: ~S ~a ~S ~a"  src orgelno target value)
          (set-cell (slot-value (aref *curr-state* (1- orgelno)) (target-string->sym target)) value :src src)))       
       ,@(loop for orgelidx below maxorgel
               collect `(define-orgel-level-meter-responder ,stream ,orgelidx))
       (define-orgel-ccin-responder ,stream)
       (define-orgel-notein-responder ,stream)
       (incudine:make-osc-responder
        ,stream "/client-id" "s"
        (lambda (id)
          (incudine.util:with-logger (:level :info)
            (incudine.util:msg :info "connected to server, client-id: ~S" id))
          (setf *client-id* id)))
       nil)))

(defun make-all-responders ()
  (%make-all-responders))

;;; (make-all-responders)

(defparameter *orgel-target-props*
  (append
   (apply #'append
           (mapcar #'list *orgel-global-targets* *orgel-global-target-syms*))
   (apply #'append
           (mapcar #'list *orgel-fader-targets* *orgel-fader-target-syms*))))

(defun target->target-sym (target)
  (if (keywordp target)
      (getf *orgel-target-props* target)
      target))

;;; (getf *orgel-target-props* :level)

(defun orgel-ctl-fader (orgel target partial val)
  (let ((orgelidx (gethash orgel *orgeltargets*)))
    (unless orgelidx (error "Orgel \"~S\" doesn't exist" orgel))
;;;  (break "orgel: ~a ~a ~a ~a" orgel target idx val)
    (set-cell
     (aref
      (slot-value (aref *curr-state* orgelidx)
                  (target->target-sym target))
      (1- partial))
     (float val 1.0))))

;;; (orgel-ctl-fader :orgel04 (target->target-sym :level) 2 0.5)

(declaim (inline target-key))
(defun target-key (target)
  (if (keywordp target) target
      (format nil "orgel~2,'0d" target)))

(defun orgel-ctl (orgel target val)
  (if (eql orgel :all)
      (dotimes (orgelidx *orgelcount*)
        (set-cell (slot-value (aref *curr-state* orgelidx)
                          (target-key->sym target))
              (float val 1.0)))
  (let ((orgelidx (gethash orgel *orgeltargets*)))
    (unless orgelidx (error "Orgel \"~S\" doesn't exist" orgel))
;;;  (break "orgel: ~a ~a ~a ~a" orgel target idx val)
    (set-cell (slot-value (aref *curr-state* orgelidx)
                          (target-key->sym target))
              (float val 1.0)))))

;;; (orgel-ctl :orgel01 :base-freq 431)

(defun global-to-server (orgelno target val)
  "send global orgel slot values to the lisp server via osc."
  (let ((form (cond ((listp target) target)
                    ((keywordp target) (gethash target *observed*))
                    (t (list target)))))
    (unless form (error "target ~S doesn't exist" target))
    (unless (eq (incudine:rt-status) :started) (error "Incudine dsp engine not started!"))
    (unless *client-id* (error "not connected to server, please start connection first!"))
    (if (cdr form)
        (progn
          (incudine.util:msg :info "osc-out: /orgelctlfader \"sisif\" ~a ~a ~S ~a ~a"
                             *client-id* (float orgelno 1.0) (format nil "~a" (first form)) (float (second form) 1.0) (float val 1.0))
          (incudine.osc:message *oscout* "/orgelctlfader" "sisif"
                                *client-id* orgelno (format nil "~a" (first form)) (second form) (float val 1.0)))
        (progn
          (incudine.util:msg :info "osc-out: /orgelctl \"sisf\" ~a ~a ~S ~a"
                             *client-id* orgelno (format nil "~a" (first form)) (float val 1.0))
          (incudine.osc:message *oscout* "/orgelctl" "sisf"
                                *client-id* orgelno (format nil "~a" (first form)) (float val 1.0))))))

(defun fader-to-server (orgelno target partialno val)
  "send orgel fader slot values to the lisp server via osc."
  (unless *client-id* (error "not connected to server, please start connection first!"))
  (incudine.util:msg :info "osc-out: /orgelctlfader ~a \"sisif\" ~a ~S ~a ~a"
                     *client-id* orgelno (format nil "~a" target) partialno (float val 1.0))
  (incudine.osc:message *oscout* "/orgelctlfader" "sisif"
                        *client-id* orgelno (format nil "~a" target) partialno (float val 1.0)))
