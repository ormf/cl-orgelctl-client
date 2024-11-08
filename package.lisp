;;;; package.lisp

(defpackage #:cl-orgelctl
  (:use #:cl #:clamps #:cellctl)
  (:shadowing-import-from :cl-refs #:trigger)
;;   (:shadowing-import-from :cl-orgel-gui #:orgel #:make-orgel #:orgel-ramp-up #:orgel-ramp-down #:orgel-exp-base #:orgel-base-freq #:orgel-min-amp #:orgel-max-amp #:orgel-phase #:orgel-bias-type #:orgel-level #:orgel-delay #:orgel-q #:orgel-gain #:orgel-osc-level
;; ;;; #:orgel-meters
;;                           #:orgel-main #:orgel-bias-bw #:orgel-bias-pos #:start-orgel-gui)

  )
