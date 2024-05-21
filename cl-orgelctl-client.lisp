;;;; cl-orgelctl-client.lisp

(in-package #:cl-orgelctl)

(setf *debug* nil)

;;(cm:cd "/home/orm/work/programmieren/lisp/cl-orgelctl-remote")
(uiop:chdir (asdf:system-relative-pathname :cl-orgelctl-client ""))

;;; (permute)

(setup-ref-cell-hooks)

(defun start-orgelctl-client (&key (local-host "127.0.0.1")
                                (remote-host "127.0.0.1"))
  (ats-cuda::start-ats-cuda-server)
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

  (cm:midi-open-default :direction :input)
  (cm:midi-open-default :direction :output)
  (incudine:recv-start cm:*midi-in1*)
  (incudine:remove-all-responders cm:*midi-in1*)
  (make-orgel-cc-responder)
  (make-orgel-note-responder)
  (init-orgel-keymaps)
  (start-keymap-note-responder)
  (incudine:recv-start *oscin*)
  (connect-to-server))

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
