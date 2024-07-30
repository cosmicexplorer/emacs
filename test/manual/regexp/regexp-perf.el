;; -*- lexical-binding:t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'rx)


;;; helpers
(defconst perf-buffer-base-len 100
  "Length multiple of temp buffer to generate for search tests.")

(defconst perf-string-base-len 100
  "Length multiple of string to generate for search tests.")

(defmacro perf-with-random-buffer (size &rest body)
  "Generate a temp buffer with SIZE random ASCII chars, executing BODY."
  (declare (indent 1) (debug (fixnum &rest form)))
  `(with-temp-buffer
     (perf-insert-random ,size)
     (goto-char (point-min))
     ,@body))

(defconst perf-regexp-short-base-len 2
  "Length of components for short regexps to generate for search tests.")

(defconst perf-regexp-long-base-len 40
  "Length of components for long regexps to generate for search tests.")

(defun perf-generate-random-alternation-pattern (case-len)
  (rx-to-string
   `(|
     (literal ,(perf-random-string case-len))
     (literal ,(perf-random-string case-len)))
   t))

(defun perf-generate-random-grouped-pattern (component-len)
  (rx-to-string
   `(:
     (literal ,(perf-random-string component-len))
     (? (group (| (literal ,(perf-random-string component-len))
                  (literal ,(perf-random-string component-len)))))
     (literal ,(perf-random-string component-len)))
   t))

(defun perf-generate-regexp-strings (n base-len kind)
  (cl-loop
   with v = (make-vector n nil)
   with pattern-fun =
   (cl-ecase kind
     (alternation #'perf-generate-random-alternation-pattern)
     (grouped #'perf-generate-random-grouped-pattern))
   for el across-ref v
   for r = (funcall pattern-fun base-len)
   do (setf el r)
   finally return v))

(defun perf-compile-regexps (regexp-strings)
  (cl-loop with v = (make-vector (length regexp-strings) nil)
           for el across-ref v
           for r across regexp-strings
           do (setf el (make-regexp r))
           finally return v))

(defconst perf-num-few-regexp-patterns-to-generate 4
  "A small number of regexp patterns to try one after another.")

(defconst perf-num-many-regexp-patterns-to-generate 30
  "A large number of regexp patterns to try one after another.")

(defconst perf-num-regexp-match-loops 60
  "Number of times to try matching each regexp in a search test.")

(defmacro perf-define-parameterized-regexp-strings-test
    (base-name args &optional doc &rest body)
  "Define a set of test cases with varying types of generated
regexp patterns."
  (declare (indent 2) (debug defun))
  (unless (and (consp args)
               (= (length args) 2))
    (error "Base function %s should accept exactly two arguments."
           base-name))
  (let ((all-variants
         '((short-patterns/few-patterns/alternation perf-regexp-short-base-len perf-num-few-regexp-patterns-to-generate 'alternation)
           (short-patterns/few-patterns/grouped perf-regexp-short-base-len perf-num-few-regexp-patterns-to-generate 'grouped)
           (short-patterns/many-patterns/alternation perf-regexp-short-base-len perf-num-many-regexp-patterns-to-generate 'alternation)
           (short-patterns/many-patterns/grouped perf-regexp-short-base-len perf-num-many-regexp-patterns-to-generate 'grouped)
           (long-patterns/few-patterns/alternation perf-regexp-long-base-len perf-num-few-regexp-patterns-to-generate 'alternation)
           (long-patterns/few-patterns/grouped perf-regexp-long-base-len perf-num-few-regexp-patterns-to-generate 'grouped)
           (long-patterns/many-patterns/alternation perf-regexp-long-base-len perf-num-many-regexp-patterns-to-generate 'alternation)
           (long-patterns/many-patterns/grouped perf-regexp-long-base-len perf-num-many-regexp-patterns-to-generate 'grouped))))
    `(progn
       ,@(cl-loop
          with base-str = (symbol-name base-name)
          with (rx-str-arg n-arg) = args
          for (ext-name regexp-len-sym num-patterns-sym kind) in all-variants
          for ext-str = (symbol-name ext-name)
          for full-name = (intern (format "%s/%s" base-str ext-str))
          collect `(perf-define-variable-test
                       ,full-name (,n-arg) ,doc
                       (let ((,rx-str-arg
                              (perf-generate-regexp-strings
                               ,num-patterns-sym
                               ,regexp-len-sym
                               ,kind)))
                         ,@body))))))

(defmacro perf-define-parameterized-compile-regexp-test
    (base-name args &optional doc &rest body)
  "Define a pair of test cases with pre-compiled regexp patterns as well
as raw strings (which get compiled and cached on the fly).

NB: compilation time via `perf-compile-regexps' ('compile-fun' in the
implementation) is *not* tracked in these generated benchmark tests,
while any just-in-time regex compilation from pattern strings *is*
tracked in these benchmark timings. This is intentional."
  (declare (indent 2) (debug defun))
  (unless (and (consp args)
               (= (length args) 2))
    (error "Base function %s should accept exactly two arguments."
           base-name))
  (let ((all-variants
         '((compiled . perf-compile-regexps)
           (no-compile . nil))))
    `(progn
       ,@(cl-loop
          with base-str = (symbol-name base-name)
          with (rx-str-arg n-arg) = args
          for (ext-name . maybe-compile-fun) in all-variants
          for ext-str = (symbol-name ext-name)
          for full-name = (intern (format "%s/%s" base-str ext-str))
          collect `(perf-define-parameterized-regexp-strings-test
                       ,full-name (,rx-str-arg ,n-arg) ,doc
                       ,@(if maybe-compile-fun
                             `((let ((,rx-str-arg
                                      (,maybe-compile-fun ,rx-str-arg)))
                                 ,@body))
                           body))))))


;; +============================================================+
;; | Matching performance tests without recording any match data.
;; +============================================================+

(perf-define-parameterized-compile-regexp-test
    perf-match/no-match-data/buffer (regexps n)
  "Generate random regexps and repeatedly regex search a random buffer."
  (perf-with-random-buffer (* n perf-buffer-base-len)
    (benchmark-run perf-num-regexp-match-loops
      (cl-loop for r across regexps
               do (save-excursion
                    (re-search-forward r nil t nil t))))))

(perf-define-parameterized-compile-regexp-test
    perf-match/no-match-data/string (regexps n)
  "Generate random regexps and repeatedly regex search a random string."
  (let ((haystack (perf-random-string (* n perf-string-base-len))))
    (benchmark-run perf-num-regexp-match-loops
      (cl-loop for r across regexps
               do (string-match r haystack nil t)))))


;; +============================================================+
;; | Match data manipulation.
;; +============================================================+
(defconst perf-num-match-data-loops 600
  "Number of times to extract and reset match data in a test.")

(defun perf-generate-simple-consecutive-groups-pattern (num-groups)
  "Create a regex pattern with NUM-GROUPS subexpressions, each matching
a single '.' (any character except newline)."
  (rx-to-string
   (cl-loop for i from 1 upto num-groups
            collecting '(group not-newline) into r
            finally return `(: ,@r))
   t))

(perf-define-variable-test perf-match/match-data/string/legacy (n)
  (let* ((haystack (perf-random-string (* n perf-string-base-len)))
         (num-groups (max n 4))
         (r (perf-generate-simple-consecutive-groups-pattern num-groups))
         (m '(0 0)))
    (benchmark-run perf-num-match-data-loops
      (cl-assert (string-match r haystack nil nil))
      (match-data t m nil)
      (cl-assert (and (= (cl-first m) 0)
                      (= (cl-second m) (1- (length haystack)))
                      (= (cl-third m) 0)
                      (= (cl-fourth m) 1)))
      (cl-assert (string-match "" haystack))
      (match-data t m nil)
      (cl-assert (and (= (cl-first m) 0)
                      (= (cl-second m) 0)
                      (null (cl-third m)))))))

(perf-define-variable-test perf-match/match-data/string/match-vectors (n)
  (let* ((haystack (perf-random-string (* n perf-string-base-len)))
         (num-groups (max n 4))
         (r (make-regexp
             (perf-generate-simple-consecutive-groups-pattern num-groups)))
         (r-blank (make-regexp ""))
         (starts (match-allocate-results r))
         (ends (match-allocate-results r)))
    (benchmark-run perf-num-match-data-loops
      (cl-assert (string-match r haystack nil nil))
      (match-extract-starts r starts)
      (match-extract-ends r ends)
      (cl-assert (= (length starts) (match-num-registers r)))
      (cl-assert (= (length ends) (match-num-registers r)))
      (cl-assert (and (= (aref starts 0) 0)
                      (> (aref ends 0) 0)
                      (> (aref starts 1) (aref ends 0))))
      (cl-assert (string-match r-blank haystack nil nil))
      (match-extract-starts r-blank starts)
      (match-extract-ends r-blank ends)
      (cl-assert (and (= (aref starts 0) 0)
                      (= (aref ends 0) 0))))))
