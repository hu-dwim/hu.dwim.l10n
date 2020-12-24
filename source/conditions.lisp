;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:hu.dwim.l10n)

(define-condition l10n-error (error)
  ())

(define-condition locale-not-found-error (l10n-error)
  ((locale-name :initarg :locale-name :accessor locale-name-of))
  (:report (lambda (condition stream)
             (cl:format stream "Could not find locale definition for ~S among the CLDR files. (Hint: did you run 'hu.dwim.l10n/bin/update-cldr.sh' to download the CLDR files?)"
                        (locale-name-of condition)))))

(defun locale-not-found-error (locale-name)
  (error 'locale-not-found-error :locale-name locale-name))

(define-condition cldr-warning (simple-warning)
  ())

(defun cldr-warning (message &rest args)
  (warn 'cldr-warning :format-control message :format-arguments args))
