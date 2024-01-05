;;;; cl-orgelctl.lisp

(in-package #:cl-orgelctl)

(setf *debug* nil)


;;(cm:cd "/home/orm/work/unterricht/frankfurt/ws_22_23/musikinformatik/papierorgel/lisp/cl-orgelctl")
(uiop:chdir (asdf:system-relative-pathname :cl-orgelctl ""))
(load-orgel-presets)
(load-route-presets)

;;; (permute)

(incudine:remove-all-responders *oscin*)
(make-all-responders *orgelcount* *oscin*)
(start-osc-midi-receive)

(cm:midi-open-default :direction :input)
(cm:midi-open-default :direction :output)
(incudine:recv-start cm:*midi-in1*)
(incudine:remove-all-responders cm:*midi-in1*)
(make-orgel-cc-responder)
(make-orgel-note-responder)
(init-orgel-keymaps)
(start-keymap-note-responder)

;;; (init-orgel-keymaps)
;;; (stop-keymap-note-responder)
;;; (start-keymap-note-responder)
;;; (print-pending-keymap-responders)
;;; (clear-keymap-responders)

#|
(dotimes (idx *orgelcount*)
  (make-responders idx))

(let ((test (make-orgel)))
  (slot-value test 'ramp-up))
|#

(incudine:recv-start *oscin*)

;;; (incudine.osc:close *oscout*)
;;; (incudine.osc:close *oscin*)

(setup-ref-cell-hooks)
(incudine:rt-start)

