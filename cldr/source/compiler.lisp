;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :hu.dwim.cldr-compiler)

(def constant +generated-file-header+ (format nil "~
;;; This file is generated by the :hu.dwim.cldr-compiler package
;;; from the locale definitions found in the http://cldr.org
;;; repository. It is a part of the :hu.dwim.l10n suite.
;;;
;;; A friendly reminder: editing generated files is not wise.

"))

(defun compile-cldr-files ()
  (with-standard-io-syntax
    (bind ((output-file-name (project-relative-pathname (make-pathname :directory '(:relative "generated")
                                                                       :name "main"
                                                                       :type "lisp")))
           (cldr-directory (project-relative-pathname "xml/common/main/")))
      (format t "Compiling CLDR xml's from dir ~S into ~S~%" cldr-directory output-file-name)
      (bind ((locale-factories (make-hash-table :test 'equal))
             (*package* (load-time-value (find-package :hu.dwim.l10n/ldml))))
        (dolist (file (uiop:directory-files cldr-directory))
          (bind ((locale-name (pathname-name file)))
            (format t "Processing ~S~%" locale-name)
            (bind ((locale (parse-cldr-file locale-name)))
              (setf (gethash locale-name locale-factories)
                    (make-local-factory-form locale)))))
        ;; emit the file contents
        (with-output-to-file (out output-file-name :if-exists :supersede)
          (flet ((emit (form)
                   (write form
                          :stream out
                          :readably t)
                   (terpri out)
                   (terpri out)))
            (format t "Emitting forms~%")
            (write-string +generated-file-header+ out)
            (emit `(cl:in-package ,(make-keyword (package-name *package*))))
            (emit `(defparameter ldml::*locale-registry* (make-hash-table :test 'equal)))
            (emit `(defun ldml::%register-locale (ldml::locale-name factory)
                     (setf (gethash ldml::locale-name ldml::*locale-registry*) factory)))
            (dolist (locale-name (alexandria:hash-table-keys locale-factories))
              (emit `(ldml::%register-locale ,locale-name (lambda () ,(gethash locale-name locale-factories)))))
            (format t "Done~%")))))))
