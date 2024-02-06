;;; 
;;; midi-keymap-handler.lisp
;;;
;;; definition of different keymaps and their notein handlers
;;;
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

(in-package :incudine)

;;; shadow incudine's wrapper to make *keymap-note-responder-fn* work

(defun midi-responder-wrapper (function)
    (let* ((lambda-list (incudine.util::function-lambda-list function))
           (len (length lambda-list))
           (key-pos (position '&key lambda-list)))
      (if (and key-pos (<= key-pos 2))
          ;; Optional keywords are ignored with one argument `(stream &key ...)'
          ;; but an error is signaled with more arguments.
          (setf len key-pos))
      ;; &optional makes sense only with three mandatory arguments.
      (case (position '&optional lambda-list)
        ((nil) (case len
                 (1 ;; (func stream)
                  (lambda (status data1 data2 stream)
                    (declare (ignore status data1 data2))
                    (funcall function stream)))
                 (3 ;; (func status data1 data2)
                  (lambda (status data1 data2 stream)
                    (declare (ignore stream))
                    (funcall function status data1 data2)))
                 (4 ;; No wrapper:
                    ;; (func status data1 data2 stream)
                  function)
                 (otherwise
                  (if (eq (first lambda-list) '&rest)
                      ;; No wrapper:
                      ;; (func &rest arguments)
                      function
                      (error 'alexandria:simple-program-error
                        :format-control "MIDI responder function with invalid ~
                                         number of arguments: ~D"
                        :format-arguments (list len))))))
        ;; No wrapper:
        ;; (func status data1 data2 &optional stream)
        (t function))))

(in-package :cl-orgelctl)

(defparameter *orgel-keymaps* nil)
(defparameter *orgel-keymap-note-responder* nil)
(defparameter *sticky* nil)

(defun toggle-sticky ()
  (setf *sticky* (not *sticky*)))

(defun keys-panic ()
  (dotimes (orgel 10)
    (dotimes (key 16)
      (orgel-ctl-fader (orgel-name (1+ orgel)) :osc-level (1+ key) 0.0))))

(defun init-orgel-keymaps ()
  "setup of different keymaps, each using a different midi channel:

  channels 0 and 1: sorted by orgel/partial, keynums 24-103.
                    orgel 1-5 in chan 0, orgel 6-10 in chan 2.

  channels 2 and 3: sorted by frequency, keynums 24-103.
                    first 80 freqs in chan 1, second 80 freqs in chan 2.

  channel 4: sorted by tempered pitch, using the closest orgelpartial
             for each keynum.

  channel 5: sorted by tempered pitch, using a random orgelpartial of
             all partials within [keynum-0.5..keynum+0.5], or -if none
             exists- the closest of all available partials."
  (setf *orgel-keymaps*
        (make-array 16 :element-type 'vector
                       :initial-contents
                       (loop repeat 16 collect
                                       (make-array 128 :element-type 'list
                                                       :initial-element nil))))
  (loop ;;; Keynums 24->103 for organs 1-5 in organ order
        with keymap = (aref *orgel-keymaps* 0)
        for keynum from 24 to 103
        with offset = -24
        do (setf (aref keymap keynum)
                 (list
                  (find-entry (1+ (floor (+ keynum offset) 16))
                              (1+ (mod (+ keynum offset) 16))))))
  (loop ;;; Keynums 24->103 for organs 6-10 in organ order
        with keymap = (aref *orgel-keymaps* 1)
        for keynum from 24 to 103
        with offset = (- 80 24)
        do (setf (aref keymap keynum)
                 (list
                  (find-entry (1+ (floor (+ keynum offset) 16))
                              (1+ (mod (+ keynum offset) 16))))))  
  (loop ;;; Keynums 24->103 for organ freqs in sorted order
        with keymap = (aref *orgel-keymaps* 2) 
        for keynum from 24 to 103
        with offset = -24
        do (setf (aref keymap keynum)
                 (list
                  (elt *orgel-freqs* (+ keynum offset)))))
  (loop ;;; Keynums 24->103 for second part of freqs in sorted order
        with keymap = (aref *orgel-keymaps* 3) 
        for keynum from 24 to 103
        with offset = (- 80 24)
        do (setf (aref keymap keynum)
                 (list
                  (elt *orgel-freqs* (+ keynum offset)))))  

  (loop ;;; one partial per key, closest partial in tempered scale.
        with keymap = (aref *orgel-keymaps* 4) 
        for entry across keymap
        for keynum from 0
        do (setf (aref keymap keynum) (list (find-orgel-partial (mtof keynum)))))

  (loop ;;; multiple partials per key, closest partials in tempered scale.
        with keymap = (aref *orgel-keymaps* 5)
        for entry in *orgel-freqs*
        do (push entry (aref keymap (round (second entry)))))
  (loop ;;; fill in gaps, push number of partials for keynums with
;;; more than one partial.
        with keymap = (aref *orgel-keymaps* 5)
        for entry across keymap
        for keynum from 0
        if (null entry) ;;; fill in gaps
          do (setf (aref keymap keynum) (list (find-orgel-partial (mtof keynum))))
        else do (let ((len (length entry))) ;;; cons length for entries
;;; with more than one partial
                  (if (> len 1) (setf (aref keymap keynum)
                                      (cons len (sort entry #'< :key #'first))))))

  (loop ;;; multiple partials per key, closest partials in tempered scale.
        with keymap = (aref *orgel-keymaps* 5)
        for entry in *orgel-freqs*
        do (push entry (aref keymap (round (second entry)))))
;;; channel 6: Francesco keymap mit 4 Tonhöhen pro Taste in cm:heap Selektion.
  (loop
    with keymap = (aref *orgel-keymaps* 6)
    for entry in (ou:group *orgel-freqs* 4)
    for idx from 24
    do (setf (aref keymap idx) (cm:new cm:heap :of entry)))
;;; channel 7: Francesco keymap mit 4 Tonhöhen pro Taste als cluster
  (loop
    with keymap = (aref *orgel-keymaps* 7)
    for entry in (ou:group *orgel-freqs* 4)
    for idx from 24
    do (setf (aref keymap idx) (cm:new cm:line :of (list entry)))))


;;; (cm:next (aref (aref *orgel-keymaps* 6) 24))

;;; (init-orgel-keymaps)

(defun find-entry (orgelno faderno &key (freqs *orgel-freqs*))
  "retrieve the entry of orgelno and faderno from *orgel-freqs*."
    (find (list orgelno faderno) freqs :key #'cddr :test #'equal))

;;; (find-entry 1 2) (55.0 33.0 1 2)

(defun get-closest (keynum entry)
  "search a list of (freq keynum orgelno faderno) sorted by keynum and
return the element closest to the supplied keynum."
  (loop
    for (partial1 partial2) on entry while partial2
    for keynum1 = (second partial1)
    for keynum2 = (second partial2)
    until (<= keynum1 keynum keynum2)
    finally (return (if (< (- keynum keynum1) (- keynum2 keynum))
                        partial1 partial2))))

(defun get-keymap-entry (keynum chan &key (select :random))
  "get one entry from the keymap at chan according to keynum. The
keymaps contain lists in the form (freq cent orgelno faderno). If more
than one partial is associated with keynum, the first element of the
list at keynum is the length of the following lists to be used to
retrieve a random element."
  (let ((entry (aref (aref *orgel-keymaps* chan) keynum)))
    (cond ((typep entry 'cm::pattern)
           (cm:next entry))
          ((numberp (first entry)) ;;; more than one list in entry
           (case select
             (:random (elt (cdr entry) (random (first entry))))
             (:first (second entry))
             (:last (first (last entry)))
             (:closest (get-closest keynum (cdr entry)))))
          (t (first entry)))))

;;; (get-keymap-entry 60 6)

(defmacro remove-1 (elem list &key test (from-end t))
"destructively remove 1 occurence of elem from list, starting at the
end and reset the list to the result."
  `(setf ,list (delete ,elem ,list :test (or ,test #'equal) :count 1 :from-end ,from-end)))

;;; We use the parameter *keymap-note-responder-fn* defined below as a
;;; responder function for midi-note-events because we create and
;;; maintain a "pending" list which contains all pending note
;;; events. Using the parameter we can access the function for
;;; inspecting and clearing the list by calling it with a keyword as
;;; first element instead of a status byte like this:
;;;
;;; (funcall *keymap-note-responder-fn* :clear)
;;;
;;; As d1 and d2 are not used in such cases they have to be declared
;;; &optional.

(defparameter *keymap-note-responder-fn*
  (let ((pending nil))
    (lambda (&optional st d1 d2 &rest args)
      (incudine.util:msg :info "keymap-note-responder: ~S ~a ~a ~a~%" st d1 d2 args)
      (if (keywordp st)
          (incudine.util:msg :info "keymap-note-responder: ~S~%" st)
          (incudine.util:msg :info "keymap-note-responder: ~S ~a ~a ~%"
                             (cm:status->opcode st) d1 d2
                             (cm:status->channel st)))
      (case (if (numberp st) (cm:status->opcode st) st)
        ;;; Midi messages
        (:note-on (let* ((chan (cm:status->channel st))
                         (val (float (/ d2 127) 1.0))
                         (entry (get-keymap-entry d1 chan)))
                    (when (numberp (first entry)) (setf entry (list entry)))
                    (push (list* d1 entry) pending)
                    (incudine.util:msg :info "entry: ~a~%pending: ~a" entry pending)
                    (dolist (e entry)
                      (destructuring-bind
                          (&optional freq keynum orgelno faderno) e
                        (declare (ignore freq keynum))
                        (when orgelno
                          (orgel-ctl-fader (orgel-name orgelno) :osc-level faderno val)
                           ;;; register entry
                          )))))
        (:note-off
         (incudine.util:msg :info "note-off: ~a ~a ~%" d1 d2)
         (unless *sticky* (let ((entry (cdr (assoc d1 pending))))
                            (when entry
                              (dolist (e entry)
                                (orgel-ctl-fader (orgel-name (third e))
                                                 :osc-level (fourth e) 0.0)
                                (remove-1 e pending)) ;;; unregister entry
                              ))))
        ;;; additional messages
        (:clear
         (dolist (entry pending)
           (dolist (e entry)
             (orgel-ctl-fader (orgel-name (third e))
                              :osc-level (fourth e) 0.0)))
         (setf pending nil))
        (:print
         (if pending
             (format t "pending: ~{~a~^, ~}~%" pending)
             (format t "no pending notes~%")))))))

(defun clear-keymap-responders ()
  (funcall *keymap-note-responder-fn* :clear))

;;; (clear-keymap-responders)

(defun print-pending-keymap-responders ()
  (funcall *keymap-note-responder-fn* :print))

;;; (print-pending-keymap-responders)


;;; (funcall *keymap-note-responder-fn* (+ 4 (ash cm::+ml-note-on-opcode+ 4)) 60 64)
;;; (funcall *keymap-note-responder-fn* (+ 4 (ash cm::+ml-note-on-opcode+ 4)) 62 64)

(defun start-keymap-note-responder ()
  (if *orgel-keymap-note-responder*
      (incudine::remove-responder *orgel-keymap-note-responder*))
  (setf *orgel-keymap-note-responder*
        (incudine:make-responder cm:*midi-in1* *keymap-note-responder-fn*))
  :started)

;;; (start-keymap-note-responder)

(defun stop-keymap-note-responder ()
  (if *orgel-keymap-note-responder*
      (incudine::remove-responder *orgel-keymap-note-responder*))
  :stopped)

;;; (funcall *keymap-note-responder-fn* 144 60 0)


;;; (funcall *keymap-note-responder-fn* :clear)

#|
(incudine::remove-all-responders cm:*midi-in1*)

(dolist (responder (gethash cm:*midi-in1* incudine::*responders*))
  (format t "~a~%" (incudine::responder-function responder))
 )

|#
