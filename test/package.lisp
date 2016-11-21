(cl:defpackage :hu.dwim.l10n/test
  (:use :common-lisp
        :hu.dwim.l10n
        :alexandria
        :iter
        :hu.dwim.stefil
        :metabang-bind
        )
  (:shadowing-import-from :hu.dwim.l10n
   #:format
   #:formatter
   #:*locale-cache*
   #:*locale*
   #:precedence-list-of
   ))

(in-package :hu.dwim.l10n/test)

(defsuite* (test :in root-suite))

;; import all the internal symbol of WUI
(let ((l10n-package (find-package :hu.dwim.l10n))
      (test-package (find-package :hu.dwim.l10n/test)))
  (iter (for symbol :in-package l10n-package :external-only nil)
        (when (and (eq (symbol-package symbol) l10n-package)
                   (not (find-symbol (symbol-name symbol) test-package)))
          (import symbol test-package))))
