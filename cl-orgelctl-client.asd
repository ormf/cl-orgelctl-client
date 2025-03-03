;;;; cl-orgelctl.asd

(when (find-package :slynk) (pushnew :slynk *features*))
(when (find-package :swank) (pushnew :swank *features*))

(asdf:defsystem #:cl-orgelctl-client
  :description "Remote Controller für HfMDK Orgelprojekt 2022-24"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :depends-on (#:clamps #:cellctl)
  :license  "GPL 2.0 or later"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
;;;               (:file "svg-export")
               (:file "websocket-server")
               (:file "structs")
               (:file "globals")
               (:file "orgel-accessor-fns")
               (:file "orgel-value-callbacks")
               (:file "preset-parser")
               (:file "presets")
               (:file "utils")
;;               (:file "cl-orgel-gui-redefs")
               (:file "database")
               (:file "midi-cc-handler")
               (:file "midi-notein-handler")
               (:file "midi-keymap-handler")
               (:file "osc")
               (:file "osc-midi")
               (:file "ats-player")
               (:file "svg-orgel-player")
               (:file "osc-pd-ctl")
               (:file "orgel-synth")
               (:file "preset-gui")
               (:file "cl-orgelctl-client")))
