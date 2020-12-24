;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :hu.dwim.cldr-compiler)

;;; see http://unicode.org/cldr/

;; TODO use alists instead of plists...

(defvar *parser*)

(define-condition cldr-parser-warning (cldr-warning)
  ((parser :initform *parser* :initarg :parser :accessor parser-of))
  (:report (lambda (c stream)
             (bind ((source-xml-file (source-xml-file-of (parser-of c))))
               (apply #'cl:format stream (concatenate 'string "~A.~A: " (simple-condition-format-control c))
                      (pathname-name source-xml-file)
                      (pathname-type source-xml-file)
                      (simple-condition-format-arguments c))))))

(defun cldr-parser-warning (message &rest args)
  (warn 'cldr-parser-warning :format-control message :format-arguments args))

(defclass cldr-parser (flexml:flexml-builder)
  ((source-xml-file :initarg :source-xml-file :accessor source-xml-file-of)))

(defun make-cldr-parser ()
  (make-instance 'cldr-parser :default-package (string '#:hu.dwim.l10n/ldml)))

(defun cldr-pathname-for (locale-name)
  (merge-pathnames (concatenate 'string locale-name ".xml")
                   (project-relative-pathname "xml/common/main/")))

(defun compute-locale-precedence-list (locale)
  "Calculate the precedence list for a locale that should be searched for definitions. For example: (locale-precedence-list (locale \"en_US_POSIX\")) => (en_US_POSIX en_US en root)"
  (let ((result (list locale-name)))
    (flet ((try (locale-name)
             (awhen (or (cached-locale locale-name)
                        (parse-cldr-file locale-name))
               (push it result))))
      (when (variant-of locale)
        (try (locale-name locale
                          :ignore-variant t)))
      (when (territory-of locale)
        (try (locale-name locale
                          :ignore-territory t
                          :ignore-variant t)))
      (when (script-of locale)
        (try (locale-name locale
                          :ignore-script t
                          :ignore-territory t
                          :ignore-variant t))))
    (when (boundp '*root-locale*)
      ;; this is how we deal with the situation when loading the root locale itself
      (push *root-locale* result))
    (nreverse result)))

(defun parse-cldr-file (locale-name)
  (bind ((*package* (find-package :hu.dwim.l10n))
         (parser (make-cldr-parser))
         (source-xml-file (cldr-pathname-for locale-name))
         (*locale* nil))
    (bind ((*parser* parser))
      (setf (source-xml-file-of *parser*) source-xml-file)
      (cxml:parse source-xml-file *parser* :entity-resolver 'cldr-entity-resolver)
      (process-ldml-node nil (flexml:root-of *parser*))
      ;;(setf (precedence-list-of *locale*) (compute-locale-precedence-list *locale*))
      )
    (assert *locale*)
    (values *locale* parser)))

(defun cldr-entity-resolver (public-id system-id)
  (declare (ignore public-id))
  (check-type system-id puri:uri)
  (bind ((file (cond
                 ((puri:uri= system-id (load-time-value
                                        (puri:parse-uri "http://www.unicode.org/cldr/dtd/1.8.0/ldml.dtd")))
                  "cldr/common/dtd/ldml.dtd")
                 ((puri:uri= system-id (load-time-value
                                        (puri:parse-uri "http://www.unicode.org/cldr/dtd/1.8.0/cldrTest.dtd")))
                  "cldr/common/dtd/cldrTest.dtd"))))
    (when file
      (open (project-relative-pathname file) :element-type '(unsigned-byte 8) :direction :input))))

(defmethod flexml:class-name-for-node-name ((parser cldr-parser) namespace-uri package (local-name string) qualified-name)
  (let ((class-name (find-symbol (string-upcase (camel-case-to-hyphened local-name)) :ldml)))
    (if (find-class class-name nil)
        class-name
        'ldml:node)))

(defclass ldml:node (flexml:flexml-node)
  ())

(defmethod print-object ((self ldml:node) stream)
  (let ((*print-circle* nil))
    (print-unreadable-object (self stream :type t :identity t)
      (princ (flexml::local-name-of self) stream))))

(macrolet ((define (&body entries)
             `(progn
                ,@(iter (for entry :in entries)
                        (destructuring-bind (name &optional supers &body slots)
                            (ensure-list entry)
                          (unless supers
                            (setf supers '(ldml:node)))
                          (collect `(defclass ,name ,supers
                                      (,@slots))))))))
  (define
   ldml:ldml
   ldml:identity
   ldml:version
   ldml:language
   ldml:languages
   ldml:script
   ldml:scripts
   ldml:territory
   ldml:territories
   ldml:variant
   ldml:variants
   ldml:numbers
   ldml:symbols
   ldml:currencies
   ldml:currency
   ldml:currency-formats
   ldml:currency-format-length
   ldml:currency-format
   ldml:currency-spacing
   ldml:unit-pattern
   ldml:display-name
   ldml:calendar
   ldml:calendars
   ldml:month-width
   ldml:month
   ldml:day-width
   ldml:day
   ldml:quarter-width
   ldml:quarter
   ldml:am
   ldml:pm
   ldml:date-formats
   ldml:date-format-length
   ldml:time-formats
   ldml:time-format-length
   ldml:era-names
   ldml:era-abbr
   ldml:era-narrow
   ldml:era
   ldml:decimal-formats
   ldml:decimal-format-length
   ldml:decimal-format
   ldml:percent-formats
   ldml:percent-format-length
   ldml:percent-format
   ))

(defmethod sax:characters ((parser cldr-parser) data)
  (unless (every (lambda (char)
                   (member char '(#\Space #\Tab #\Return #\Linefeed) :test #'char=))
                 data)
    (call-next-method)))

(defun ldml-intern (name &key hyphenize)
  (check-type name string)
  (when hyphenize
    (setf name (camel-case-to-hyphened name)))
  (let* ((ldml-package (find-package '#:hu.dwim.l10n/ldml))
         (result (intern (string-upcase name) ldml-package)))
    (export result ldml-package)
    result))

(defgeneric process-ldml-node (parent node)
  (:method (parent (node flexml:flexml-node))
    (iter (for child :in-sequence (flexml:children-of node))
          (process-ldml-node node child)))

  (:method ((parent flexml:flexml-node) (node string))
    ;; nop
    )

  (:method ((parent ldml:ldml) (node ldml:identity))
    (flet ((lookup (type &optional (attribute-name 'ldml::type))
             (let* ((child-node (flexml:first-child-with-type node type))
                    (value (when child-node
                             (slot-value child-node attribute-name))))
               (assert (or (null value)
                           (stringp value)))
               value))
           (drop-wrapper-string (value prefix postfix)
             (when (and (starts-with-subseq prefix value)
                        (ends-with-subseq postfix value))
               (subseq value
                       (length prefix)
                       (- (length value)
                          (length postfix))))))
      (let ((language  (lookup 'ldml:language))
            (script    (lookup 'ldml:script))
            (territory (lookup 'ldml:territory))
            (variant   (lookup 'ldml:variant))
            (version   (lookup 'ldml:version 'ldml:number)))
        (assert language)
        (setf version (drop-wrapper-string version "$Revision: " " $"))
        (setf *locale*
              (make-instance 'locale
                             :language language
                             :script script
                             :territory territory
                             :variant variant
                             :version-info version)))))

  (:method ((parent ldml:numbers) (node ldml:symbols))
    ;; FIXME
    #+nil
    (iter (for symbol-node :in-sequence (flexml:children-of node))
          (for value = (flexml:string-content-of symbol-node))
          (for name = (ldml-intern (flexml:local-name-of symbol-node) :hyphenize t))
          (push (cons name value) (number-symbols-of *locale*))))

  (:method ((parent ldml:currencies) (node ldml:currency))
    (flet ((ensure-currency (locale code)
             (bind ((currency (gethash code (currencies-of locale))))
               (unless currency
                 (setf currency (make-instance 'currency :code code))
                 (setf (gethash code (currencies-of locale)) currency))
               currency)))
      (bind ((currency-code (slot-value node 'ldml::type)))
        (assert (every #'upper-case-p currency-code))
        (setf currency-code (ldml-intern currency-code))
        (when-bind symbol-node (flexml:first-child-with-local-name node "symbol")
          (bind ((symbol (flexml:string-content-of symbol-node)))
            (setf (symbol-of (ensure-currency *locale* currency-code)) symbol)))
        (when-bind display-name-node (flexml:first-child-with-local-name node "displayName")
          (bind ((long-name (flexml:string-content-of display-name-node)))
            (setf (long-name-of (ensure-currency *locale* currency-code)) long-name))))))

  (:method ((parent ldml:numbers) (node ldml:currency-formats))
    (setf (currency-formatter-of *locale*) (make-instance 'currency-formatter))
    (call-next-method))

  (:method ((parent ldml:currency-spacing) node)
    ;; FIXME
    #+nil
    (bind ((currency-slot-reader (symbolicate (string-upcase (camel-case-to-hyphened (flexml:local-name-of node))) '#:-of))
           (slot-value (funcall currency-slot-reader (currency-formatter-of *locale*))))
      (iter (for child :in-sequence (flexml:children-of node))
            (setf (getf slot-value (ldml-intern (flexml:local-name-of child) :hyphenize t)) (flexml:string-content-of child)))
      (funcall (fdefinition `(setf ,currency-slot-reader)) slot-value (currency-formatter-of *locale*))))

  (:method ((parent ldml:currency-formats) (node ldml:currency-format-length))
    (bind ((ldml-type (slot-value node 'ldml::type))
           (name (and ldml-type (ldml-intern ldml-type)))
           ;; FIXME flexml:first-child
           (inbetween-node (flexml:first-child node)))
      (unless (length= 1 (flexml:children-of inbetween-node))
        (cldr-parser-warning "LDML node ~A has multiple children, using the first one" inbetween-node))
      (bind ((pattern (flexml:string-content-of (flexml:first-child inbetween-node))))
        (setf (assoc-value (formatters-of (currency-formatter-of *locale*)) name)
              (list :pattern pattern)))))

  (:method ((parent ldml:currency-formats) (node ldml:unit-pattern))
    (bind ((ldml-count (slot-value node 'ldml::count))
           (name (and ldml-count (ldml-intern ldml-count)))
           (unit-pattern (flexml:string-content-of node)))
      (setf (getf (unit-pattern-of (currency-formatter-of *locale*)) name) unit-pattern)))

  (:method ((parent ldml:languages) (node ldml:language))
    (process-langauge-list-like-ldml-node node 'languages-of))

  (:method ((parent ldml:scripts) (node ldml:script))
    (process-langauge-list-like-ldml-node node 'scripts-of))

  (:method ((parent ldml:territories) (node ldml:territory))
    (process-langauge-list-like-ldml-node node 'territories-of))

  (:method ((parent ldml:variants) (node ldml:variant))
    (process-langauge-list-like-ldml-node node 'variants-of))

  (:method ((parent ldml:calendars) (node ldml:calendar))
    (if (string= (slot-value node 'ldml::type) "gregorian")
        (progn
          (setf (gregorian-calendar-of *locale*) (make-instance 'gregorian-calendar))
          (process-ldml-gregorian-calendar-node parent node))
        (call-next-method)))
  (:method ((parent ldml:decimal-formats) (node ldml:decimal-format-length))
    ;; FIXME
    #+nil
    (push (process-simple-number-formatter-node node)
          (decimal-formatters-of *locale*)))

  (:method ((parent ldml:percent-formats) (node ldml:percent-format-length))
    (push (process-simple-number-formatter-node node)
          (percent-formatters-of *locale*))))

(defun process-simple-number-formatter-node (node)
  (bind ((ldml-type (slot-value node 'ldml::type))
         (name (and ldml-type (ldml-intern ldml-type)))
         (inbetween-node (flexml:the-only-child node)))
      (unless (length= 1 (flexml:children-of inbetween-node))
        (cldr-parser-warning "LDML node ~A has multiple children, using the first one" inbetween-node))
      (bind ((pattern (flexml:string-content-of (flexml:first-child inbetween-node))))
        (list name :pattern pattern))))

(defun process-langauge-list-like-ldml-node (node accessor)
  (let* ((name (string-upcase (slot-value node 'ldml::type))))
    (aif (parse-integer name :junk-allowed t)
         (setf name it)
         (setf name (ldml-intern name)))
    (let* ((display-name (flexml:string-content-of node)))
      (setf (gethash name (funcall accessor *locale*)) display-name))))

(defgeneric process-ldml-gregorian-calendar-node (prent node)
  (:method (parent (node flexml:flexml-node))
    (iter (for child :in-sequence (flexml:children-of node))
          (process-ldml-gregorian-calendar-node node child)))

  (:method ((parent flexml:flexml-node) (node string))
    ;; nop
    )

  (:method ((parent ldml:calendar) (node ldml:am))
    (setf (am-of (gregorian-calendar-of *locale*)) (flexml:string-content-of node)))

  (:method ((parent ldml:calendar) (node ldml:pm))
    (setf (pm-of (gregorian-calendar-of *locale*)) (flexml:string-content-of node)))

  (:method ((parent ldml:month-width) (node ldml:month))
    (process-month-list-like-ldml-node
     parent node 12 '(("narrow"      . narrow-month-names-of)
                      ("abbreviated" . abbreviated-month-names-of)
                      ("wide"        . month-names-of))))

  (:method ((parent ldml:day-width) (node ldml:day))
    (process-month-list-like-ldml-node
     parent node 7 '(("narrow"      . narrow-day-names-of)
                     ("abbreviated" . abbreviated-day-names-of)
                     ("wide"        . day-names-of))
     '("sun" "mon" "tue" "wed" "thu" "fri" "sat")))

  (:method ((parent ldml:quarter-width) (node ldml:quarter))
    (process-month-list-like-ldml-node
     parent node 4 '(("abbreviated" . abbreviated-quarter-names-of)
                     ("wide"        . quarter-names-of))))

  (:method ((parent ldml:era-names) (node ldml:era))
    (parse-era-ldml-node node 'era-names-of))

  (:method ((parent ldml:era-abbr) (node ldml:era))
    (parse-era-ldml-node node 'abbreviated-era-names-of))

  (:method ((parent ldml:era-narrow) (node ldml:era))
    (parse-era-ldml-node node 'narrow-era-names-of))

  (:method ((parent ldml:date-formats) (node ldml:date-format-length))
    (bind ((name (ldml-intern (slot-value node 'ldml::type)))
           (inbetween-node (flexml:the-only-child node)))
      (unless (length= 1 (flexml:children-of inbetween-node))
        (cldr-parser-warning "LDML node ~A has multiple children, using the first one" inbetween-node))
      (bind ((pattern (flexml:string-content-of (flexml:first-child inbetween-node))))
        (setf (getf (date-formatters-of (gregorian-calendar-of *locale*)) name)
              (list :formatter 'dummy-formatter :pattern pattern)))))

  (:method ((parent ldml:time-formats) (node ldml:time-format-length))
    (bind ((name (ldml-intern (slot-value node 'ldml::type)))
           (inbetween-node (flexml:the-only-child node)))
      (unless (length= 1 (flexml:children-of inbetween-node))
        (cldr-parser-warning "LDML node ~A has multiple children, using the first one" inbetween-node))
      (bind ((pattern (flexml:string-content-of (flexml:first-child inbetween-node))))
        (setf (getf (time-formatters-of (gregorian-calendar-of *locale*)) name)
              (list :formatter 'dummy-formatter :pattern pattern))))))

(defun parse-era-ldml-node (node reader)
  (bind ((calendar (gregorian-calendar-of *locale*))
         (index-designator (slot-value node 'ldml::type))
         (index (parse-integer index-designator))
         (writer (fdefinition `(setf ,reader)))
         (vector (funcall reader calendar)))
    (assert (<= 0 index 1))
    (unless vector
      (setf vector (make-array 2 :initial-element nil))
      (funcall writer vector calendar))
    (setf (aref vector index) (flexml:string-content-of node))))

(defun process-month-list-like-ldml-node (parent node max-count reader-map &optional index-designators)
  (bind ((calendar (gregorian-calendar-of *locale*))
         (reader (awhen (assoc (slot-value parent 'ldml::type) reader-map :test #'string=)
                   (cdr it)))
         (index-designator (slot-value node 'ldml::type))
         (index (aif (parse-integer index-designator :junk-allowed t)
                     (1- it)
                     (position index-designator index-designators :test #'string=))))
    (assert (<= 0 index (1- max-count)))
    (when reader
      (bind ((writer (fdefinition `(setf ,reader)))
             (vector (funcall reader calendar)))
        (unless vector
          (setf vector (make-array max-count :initial-element nil))
          (funcall writer vector calendar))
        (setf (aref vector index) (flexml:string-content-of node))))))
