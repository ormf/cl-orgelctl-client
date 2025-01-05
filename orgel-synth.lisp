;;; 
;;; orgel-synth.lisp
;;;
;;; definition of a sine simulation synth for the papierrohrorgel.
;;;
;;; The synth is automatically defined on startup, but needs to be
;;; explicitely started with teh mechanism explained at the bottom of
;;; this file. The levels will be in sync with the orgel-server.
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

(defun make-orgelsynth ()
  (let ((orgelsynth-unwatch nil)
        (orgelsynth-ids (make-array 128 :element-type 'integer))
        (orgelsynth-freqs ;;; calculate orgel-freqs in orgel order:
          (coerce
           (loop
             for base-freq in *base-freqs*
             append (loop
                      for partialidx from 1 to 16
                      collect (dround (* base-freq partialidx))))
           'vector)))
    (labels
        ((setup-orgelsynth-watch ()
;;;           (break "setup-orgelsynth-watch")
           (map nil #'funcall orgelsynth-unwatch)
           (setf orgelsynth-unwatch nil)
           (dotimes (x 128)
             ;; definition of a #'watch in cellctl fashion:
             (let* ((x x)
                    (orgelidx (floor x 16))
                    (partialidx (mod x 16))
                    (fn (lambda (val) (set-control (aref orgelsynth-ids x) :amp val))))
               ;; pushing fn to the list of the fader's slot in
               ;; *osc-responder-registry* will call it whenever the
               ;; value in *curr-state* changes (this gets set up
               ;; in #'setup-ref-cell-hooks):
               (push fn (aref (slot-value (aref *osc-responder-registry* orgelidx) 'cl-orgelctl::level) partialidx))
               ;; definition of an unwatch function in cellctl
               ;; fashion: As all fns in the list of the fader's
               ;; slot in *osc-responder-registry* will get called
               ;; if a value in *curr-state* is changed, we simply
               ;; remove fn from this list.
               (push
                (lambda () 
                  (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'cl-orgelctl::level) partialidx)
                        (remove
                         fn
                         (aref (slot-value (aref *osc-responder-registry* orgelidx) 'cl-orgelctl::level)
                               partialidx))))
                orgelsynth-unwatch))))
         (unwatch-orgelsynth ()
           (map nil #'funcall orgelsynth-unwatch)
           (setf orgelsynth-unwatch nil)))
      (lambda (cmd)
        (case cmd
          (:start
           (dotimes (x 128)
             (let ((orgelidx (floor x 16))
                   (partialidx (mod x 16)))
;;;               (break "start ~a ~a" x orgelsynth-freqs)
               (osc~
                :freq (aref orgelsynth-freqs x)
                :amp (val (aref (slot-value (aref *curr-state* orgelidx) 'cl-orgelctl::level) partialidx))
                :action (let ((x x))
                          (lambda (n)
                            (setf (aref orgelsynth-ids x) (node-id n))
                            (if (= x 127)
                                (setup-orgelsynth-watch))))
                :tail 200))))
          (:stop
           (map nil #'free orgelsynth-ids)
           (dotimes (i 128) (setf (aref orgelsynth-ids i) -1))
           (unwatch-orgelsynth))
          (:unwatch
           orgelsynth-unwatch)
          (:ids
           orgelsynth-ids)
          (:freqs
           orgelsynth-freqs))))))

(dotimes (x 128)
  (let ((orgelidx (floor x 16))
        (partialidx (mod x 16)))
    (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'cl-orgelctl::level)
                partialidx)
          nil)))

;;; Orgelsynth Instanz erzeugen: Hinweis: Der Orgelsynth läuft erst,
;;; wenn er explizit mit :start gestartet wird (s.u.)!

(defparameter *orgelsynth* (make-orgelsynth))

(defun orgelsynth (cmd)
  (funcall *orgelsynth* cmd))

#|

;; Bedienung:

(funcall *orgelsynth* :start)
(funcall *orgelsynth* :stop)

;; oder direkt mit der Funktion #'orgelsynth:

(orgelsynth :start)
(orgelsynth :stop)

;; state in der repl ausgeben:

(funcall *orgelsynth* :unwatch)
(funcall *orgelsynth* :ids)
(funcall *orgelsynth* :freqs)

|#
