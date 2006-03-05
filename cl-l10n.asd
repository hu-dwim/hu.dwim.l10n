;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-user)

(defpackage #:cl-l10n.system
  (:use #:cl #:asdf))

(in-package #:cl-l10n.system)

(defsystem cl-l10n
  :name "CL-L10N"
  :author "Sean Ross <sdr@jhb.ucs.co.za>"
  :maintainer "Sean Ross <sdr@jhb.ucs.co.za>"
  :version "0.3.4"
  :description "Portable CL Locale Support"
  :long-description "Portable CL Package to support localization"
  :licence "MIT"
  :components ((:file "package")
               (:file "parse-number" :depends-on ("package"))
               (:file "utils" :depends-on ("package"))
               (:file "locale" :depends-on ("utils"))
               (:file "load-locale" :depends-on ("locale"))
               (:file "printers" :depends-on ("load-locale"))
               (:file "parsers" :depends-on ("printers" "parse-number"))
               (:file "parse-time" :depends-on ("load-locale"))
               (:file "i18n" :depends-on ("printers")))
  :depends-on (:cl-ppcre :cl-fad))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl-l10n))))
  (funcall (find-symbol "LOAD-DEFAULT-LOCALE" "CL-L10N"))
  (provide 'cl-l10n))
      

(defmethod perform ((op test-op) (sys (eql (find-system :cl-l10n))))
  (oos 'load-op :cl-l10n-tests)
  (oos 'test-op :cl-l10n-tests))

(defsystem cl-l10n-tests
  :depends-on (rt cl-l10n)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-l10n-tests))))
  (funcall (find-symbol "DO-TESTS" "REGRESSION-TEST")))

;; EOF
