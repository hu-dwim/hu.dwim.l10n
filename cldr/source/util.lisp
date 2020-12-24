;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :hu.dwim.cldr-compiler)

(defun project-relative-pathname (path)
  (asdf:system-relative-pathname :hu.dwim.cldr-compiler path))

(defun camel-case-to-hyphened (input)
  (if (> (length input) 0)
      (string-downcase
       (with-output-to-string (*standard-output*)
         (iter (with in-uppercase? = (upper-case-p (elt input 0)))
               (for run-length :upfrom 0)
               (for hyphen-distance :upfrom 0)
               (for char :in-vector input)
               (for previous-char :previous char :initially #\ )
               (let ((new-in-uppercase? (if (alpha-char-p char)
                                            (upper-case-p char)
                                            (if (alpha-char-p previous-char)
                                                (not in-uppercase?)
                                                in-uppercase?))))
                 (unless (eq in-uppercase? new-in-uppercase?)
                   ;;(break "~A ~A ~A ~A" previous-char char run-length hyphen-distance)
                   (when (and (alphanumericp char)
                              (alphanumericp previous-char)
                              (or (> run-length 1)
                                  (> hyphen-distance 1)))
                     (write-char #\-)
                     (setf hyphen-distance 0))
                   (setf run-length 0)
                   (setf in-uppercase? new-in-uppercase?)))
               (write-char char))))
      input))

#+nil
(defmacro slot-value-unless-nil (instance slot-name)
  (once-only (instance)
    `(when ,instance
       (slot-value ,instance ,slot-name))))
