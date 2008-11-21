;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10; encoding: utf-8 -*-
;; See the file LICENCE for licence information.

(in-package #:cl-l10n)

(defparameter *consonants* (iter (with result = (make-hash-table :test #'eq))
                                 (for char in '(#\b #\c #\d #\f #\g #\h #\j #\k #\l #\m #\n #\p #\q #\r #\s #\t #\v #\w #\x #\z #\y))
                                 (setf (gethash char result) t)
                                 (finally (return result))))

(defstruct (vowel-entry (:conc-name vowel-))
  (low-p nil :type boolean)
  (acute-p nil :type boolean)
  (double-acute-p nil :type boolean)
  (diaresis-p nil :type boolean)
  (normal-variant #\x :type character)
  (acute-variant nil :type (or null character))
  (double-acute-variant nil :type (or null character))
  (diaresis-variant nil :type (or null character)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun unicode-char-or-nil (code)
    (when (< code char-code-limit)
      (code-char code))))

(defparameter *vowels*
  (let ((result (make-hash-table :test #'eq)))
    (flet ((make-entry (entry)
             (make-vowel-entry :low-p (first entry)
                               :normal-variant (second entry)
                               :acute-variant (third entry)
                               :double-acute-variant (fourth entry)
                               :diaresis-variant (fifth entry))))
      (iter (for entry in
                 ;; low-p char acute double-acute diaresis
                 '((t #\a (code-char 225))
                   (t #\o (code-char 243) #.(unicode-char-or-nil 337) (code-char 246))
                   (t #\u (code-char 250) #.(unicode-char-or-nil 369) (code-char 252))
                   (nil #\e (code-char 233))
                   (nil #\i (code-char 237))
                   ))
            (for char = (second entry))
            (setf entry (mapcar #'eval entry))
            (setf (gethash char result) (make-entry entry))
            (awhen (third entry)
              (let ((acute-entry (make-entry entry)))
                (setf (vowel-acute-p acute-entry) t)
                (setf (gethash it result) acute-entry)))
            (awhen (fourth entry)
              (let ((double-acute-entry (make-entry entry)))
                (setf (vowel-double-acute-p double-acute-entry) t)
                (setf (gethash it result) double-acute-entry)))
            (awhen (fifth entry)
              (let ((diaresis-entry (make-entry entry)))
                (setf (vowel-diaresis-p diaresis-entry) t)
                (setf (gethash it result) diaresis-entry)))))
    result))

(defun vowel-entry-for (char)
  (declare (type character char)
           (optimize (speed 3) (debug 0))
           (inline))
  (gethash (char-downcase char) *vowels*))

(iter (for char in (append (list (code-char 243) (code-char 246) (code-char 250) (code-char 252))
                           #+unicode(list (code-char 337) (code-char 369))))
      (setf (vowel-low-p (vowel-entry-for char)) nil))

(defun consonantp (char)
  (declare (type character char)
           (optimize (speed 3) (debug 0))
           (inline))
  (gethash (char-downcase char) *consonants*))

(defun vowelp (char)
  (declare (type character char)
           (optimize (speed 3) (debug 0))
           (inline))
  (vowel-entry-for char))

(defun high-vowel-p (char)
  (declare (type character char)
           (optimize (speed 3) (debug 0))
           (inline))
  (aif (vowel-entry-for char)
       (not (vowel-low-p it))))

(defun low-vowel-p (char)
  (declare (type character char)
           (optimize (speed 3) (debug 0))
           (inline))
  (aif (vowel-entry-for char)
       (vowel-low-p it)))

(defun last-vowel-of (str)
  (declare (type string str)
           (optimize (speed 3) (debug 0)))
  (iter (for i from (1- (length str)) downto 0)
        (for char = (elt str i))
        (finding char :such-that (vowelp char))))

(macrolet ((define (variant)
               `(defun ,(symbolicate '#:vowel- variant '#:-variant-of) (char)
                 (declare (type character char)
                  (optimize (speed 3) (debug 0)))
                 (awhen (vowel-entry-for char)
                   (,(symbolicate '#:vowel- variant '#:-variant) it)))))
  (define normal)
  (define acute)
  (define double-acute))

(defun starts-with-consonant-p (str)
  (declare (type string str)
           (optimize (speed 3) (debug 0))
           (inline))
  (unless (zerop (length str))
    (consonantp (elt str 0))))

(defun starts-with-vowel-p (str)
  (declare (type string str)
           (optimize (speed 3) (debug 0))
           (inline))
  (unless (zerop (length str))
    (vowelp (elt str 0))))

