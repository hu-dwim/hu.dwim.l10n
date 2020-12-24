;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :hu.dwim.l10n )

;; Classes
(defclass locale ()
  ((language
    :initform (required-arg :language)
    :initarg :language
    :accessor language-of)
   (script
    :initform nil
    :initarg :script
    :accessor script-of)
   (territory
    :initform nil
    :initarg :territory
    :accessor territory-of)
   (variant
    :initform nil
    :initarg :variant
    :accessor variant-of)
   (precedence-list
    :initform nil
    :initarg :precedence-list
    :accessor precedence-list-of)
   (version-info
    :initform nil
    :initarg :version-info
    :accessor version-info-of)
   (number-symbols
    :initform (list)
    :accessor number-symbols-of)
   (currencies
    :initform (make-hash-table :test #'eq)
    :accessor currencies-of)
   (currency-formatter
    :initform nil
    :initarg :currency-formatter
    :accessor currency-formatter-of)
   (languages
    :initform (make-hash-table :test #'eq)
    :accessor languages-of)
   (scripts
    :initform (make-hash-table :test #'eq)
    :accessor scripts-of)
   (territories
    :initform (make-hash-table :test #'eq)
    :accessor territories-of)
   (variants
    :initform (make-hash-table :test #'eq)
    :accessor variants-of)
   (gregorian-calendar
    :initform nil
    :initarg :gregorian-calendar
    :accessor gregorian-calendar-of)
   (resources
    :initform (make-hash-table :test #'equal)
    :accessor resources-of)
   (initialized
    :initform nil
    :accessor initialized-p)
   (decimal-formatters
    :initform nil
    :accessor decimal-formatters-of)
   (percent-formatters
    :initform nil
    :accessor percent-formatters-of)))

(defmethod print-object ((obj locale) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (locale-name obj) stream)))

(defun make-local-factory-form (%locale)
  ;; MAKE-LOAD-FORM-SAVING-SLOTS cannot be used here because we need
  ;; need a form that can be written into a .lisp file.
  (labels
      ((recurse (object)
         (etypecase object
           ((or string number symbol)
            object)
           (standard-object
            (bind ((class (class-of object)))
              `(make-instance ',(class-name class)
                              ,@(loop
                                  :for slot :in (closer-mop:class-slots class)
                                  :collect (first (closer-mop:slot-definition-initargs slot))
                                  :collect (recurse (closer-mop:slot-value-using-class class object slot))))))
           (list
            `(make-list ,@(mapcar #'recurse object)))
           (hash-table
            `(plist-hash-table ,(loop
                                  :for (key value) :on (hash-table-plist object) :by #'cddr
                                  :nconc (list (recurse key) (recurse value)))
                               :test ,(hash-table-test object)))
           (array
            (assert (= 1 (length (array-dimensions object))))
            (assert (not (array-has-fill-pointer-p object)))
            (assert (not (array-displacement object)))
            `(make-array ,(array-dimension object 0)
                         :element-type ,(array-element-type object)
                         :initial-contents (list ,@(map 'list #'recurse object)))))))
    (recurse %locale)))

(defclass currency ()
  ((code
    :initform (required-arg :code)
    :initarg :code
    :accessor code-of)
   (symbol
    :initform nil
    :initarg :symbol
    :accessor symbol-of)
   (long-name
    :initform nil
    :initarg :long-name
    :accessor long-name-of)))

(defgeneric locale-name (locale &key ignore-script ignore-territory ignore-variant)
  (:method ((locale locale) &key ignore-variant ignore-territory ignore-script)
    (let ((*print-pretty* nil))
      (with-output-to-string (*standard-output*)
        (write-string (language-of locale))
        (unless ignore-script
          (awhen (script-of locale)
            (write-char #\_)
            (write-string it)))
        (unless ignore-territory
          (awhen (territory-of locale)
            (write-char #\_)
            (write-string it)))
        (unless ignore-variant
          (awhen (variant-of locale)
            (write-char #\_)
            (write-string it)))))))

(defun clear-locale-cache ()
  (prog1
      (hash-table-count *locale-cache*)
    (clrhash *locale-cache*)
    (makunbound '*root-locale*)
    (load-root-locale)
    (load-default-locale)))
