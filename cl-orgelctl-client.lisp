;;;; cl-orgelctl-client.lisp

(in-package #:cl-orgelctl)

(setf *debug* nil)

;;(cm:cd "/home/orm/work/programmieren/lisp/cl-orgelctl-remote")
(uiop:chdir (asdf:system-source-directory :cl-orgelctl-client))

;;; (permute)

(setup-ref-cell-hooks)

(defun start-orgelctl-client (&key (local-host "127.0.0.1")
                                (remote-host "127.0.0.1"))
;;;  (ats-cuda::start-ats-cuda-server)
  (clamps)
  (define-elisp-code)
  (load-orgel-presets)
  (load-route-presets)
  (setf *local-host* local-host)
  (setf *local-port* 3016)
  (setf *remote-host* remote-host)
  (setf *remote-port* 3011)
  (if *oscout* (incudine.osc:close *oscout*))
  (if *oscin* (incudine.osc:close *oscin*))
  (setf *oscout* (incudine.osc:open :port *remote-port* :host *remote-host* :direction :output :protocol :udp))
  (setf *oscin* (incudine.osc:open :port *local-port* :host *local-host* :direction :input :protocol :udp))
  (setf (incudine.util:logger-level) :warn)
  (incudine:rt-start)

;;; (start-osc-midi-receive)

  (unless cm:*midi-in1* (cm:midi-open-default :direction :input))
  (unless cm:*midi-out1* (cm:midi-open-default :direction :output))
  (let ((midi-in (if (typep cm:*midi-in1* 'cm:incudine-stream)
                     (cm:incudine-input cm:*midi-in1*) cm:*midi-in1*)))
    (incudine:recv-start midi-in)
    (incudine:remove-all-responders midi-in))
  (make-orgel-cc-responder)
  (make-orgel-note-responder)
  (init-orgel-keymaps)
  (start-keymap-note-responder)
  (incudine:recv-start *oscin*)
  (connect-to-server)
  (setf *package* (find-package :cl-orgelctl)))

;;; (init-orgel-keymaps)
;;; (stop-keymap-note-responder)
;;; (start-keymap-note-responder)
;;; (print-pending-keymap-responders)
;;; (clear-keymap-responders



#|
(dotimes (idx *orgelcount*)
  (make-c(make-all-responders)responders idx))

(let ((test (make-orgel)))
  (slot-value test 'ramp-up))
|#


;;; (incudine.osc:close *oscout*)
;;; (incudine.osc:close *oscin*)

;;; (incudine:rt-start)
