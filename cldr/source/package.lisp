;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :hu.dwim.def)

(def package #:hu.dwim.cldr-compiler
  (:use :alexandria
        :anaphora
        :common-lisp
        :iterate
        :hu.dwim.def
        :hu.dwim.l10n
        :metabang-bind)
  (:local-nicknames (#:flexml :hu.dwim.flexml)
                    (#:ldml :hu.dwim.l10n/ldml))
  #+nil
  (:import-from :hu.dwim.l10n
                #:*locale*
                #:*root-locale*
                #:languages-of
                #:time-formatters-of
                #:precedence-list-of
                #:variant-of
                #:territory-of
                #:territories-of
                #:script-of
                #:scripts-of
                #:number-symbols-of
                #:symbol-of
                #:currencies-of
                #:long-name-of
                #:currency-formatter-of
                #:unit-pattern-of
                #:formatters-of
                #:gregorian-calendar
                #:gregorian-calendar-of
                #:decimal-formatters-of
                #:percent-formatters-of
                #:am-of
                #:pm-of
                #:date-formatters-of

                #:when-bind
                ))

(hu.dwim.common:import-all-owned-symbols :hu.dwim.l10n
                                         :hu.dwim.cldr-compiler
                                         :overwrite t)
