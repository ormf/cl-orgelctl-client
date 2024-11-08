;;; 
;;; create-papierrohrorgel-image.lisp
;;;
;;; lisp script to generate an image starting the orgelctl server.
;;;
;;; invoke with "sbcl --load create-papierrohrorgel-image.lisp"
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-user)

(ql:quickload :slynk)
(ql:quickload :cl-orgelctl-client)

(setf *init-hooks*
      (append *init-hooks* (list #'cl-orgelctl::start-orgelctl-client)))

(sb-ext:save-lisp-and-die
 (asdf:system-relative-pathname :cl-orgelctl-client "cl-papierrohrorgel-client")
 :purify t
 :executable t)

