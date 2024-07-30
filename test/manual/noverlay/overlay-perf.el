;; -*- lexical-binding:t -*-

;; Copyright (C) 2015-2025 Free Software Foundation, Inc.

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


;; +===================================================================================+
;; | Basic performance tests
;; +===================================================================================+

(perf-define-variable-test perf-make-overlay (n)
  (with-temp-buffer
    (overlay-recenter (point-min))
    (benchmark-run 1
      (dotimes (_ n)
        (make-overlay 1 1)))))

(perf-define-variable-test perf-make-overlay-continuous (n)
  (with-temp-buffer
    (perf-insert-text n)
    (overlay-recenter (point-max))
    (benchmark-run 1
      (dotimes (i n)
        (make-overlay i (1+ i))))))

(perf-define-variable-test perf-make-overlay-scatter (n)
  (with-temp-buffer
    (perf-insert-text n)
    (benchmark-run 1
      (perf-insert-overlays-scattered n))))

(perf-define-variable-test perf-delete-overlay (n)
  (with-temp-buffer
    (let ((ovls (cl-loop for i from 1 to n
                        collect (make-overlay 1 1))))
      (overlay-recenter (point-min))
      (benchmark-run 1
        (mapc #'delete-overlay ovls)))))

(perf-define-variable-test perf-delete-overlay-continuous (n)
  (with-temp-buffer
    (perf-insert-text n)
    (let ((ovls (cl-loop for i from 1 to n
                         collect (make-overlay i (1+ i)))))
      (overlay-recenter (point-min))
      (benchmark-run 1
        (mapc #'delete-overlay ovls)))))

(perf-define-variable-test perf-delete-overlay-scatter (n)
  (with-temp-buffer
    (perf-insert-text n)
    (let ((ovls (progn (perf-insert-overlays-scattered n)
                       (overlays-in (point-min) (point-max)))))
      (benchmark-run 1
        (mapc #'delete-overlay ovls)))))

(perf-define-variable-test perf-overlays-at (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (benchmark-run 1
      (dotimes (i (point-max))
        (overlays-at i)))))

(perf-define-variable-test perf-overlays-in (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (let ((len perf-insert-overlays-default-length))
      (benchmark-run 1
        (dotimes (i (- (point-max) len))
          (overlays-in i (+ i len)))))))

(perf-define-variable-test perf-insert-before (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (goto-char 1)
    (overlay-recenter (point-min))
    (benchmark-run 1
      (dotimes (_ (/ n 2))
        (insert ?X)))))

(perf-define-variable-test perf-insert-before-empty (n)
  (let ((perf-insert-overlays-default-length 0))
    (perf-insert-before n)))
(perf-define-variable-test perf-insert-after-empty (n)
  (let ((perf-insert-overlays-default-length 0))
    (perf-insert-after n)))
(perf-define-variable-test perf-insert-scatter-empty (n)
  (let ((perf-insert-overlays-default-length 0))
    (perf-insert-scatter n)))
(perf-define-variable-test perf-delete-before-empty (n)
  (let ((perf-insert-overlays-default-length 0))
    (perf-delete-before n)))
(perf-define-variable-test perf-delete-after-empty (n)
  (let ((perf-insert-overlays-default-length 0))
    (perf-delete-after n)))
(perf-define-variable-test perf-delete-scatter-empty (n)
  (let ((perf-insert-overlays-default-length 0))
    (perf-delete-scatter n)))

(perf-define-test-suite perf-marker-suite
  (perf-define-marker-test insert before)
  (perf-define-marker-test insert after)
  (perf-define-marker-test insert scatter)
  (perf-define-marker-test delete before)
  (perf-define-marker-test delete after)
  (perf-define-marker-test delete scatter))

(perf-define-variable-test perf-insert-after (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (goto-char (point-max))
    (overlay-recenter (point-max))
    (benchmark-run 1
      (dotimes (_ (/ n 2))
        (insert ?X)))))

(perf-define-variable-test perf-insert-scatter (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (goto-char (point-max))
    (benchmark-run 1
      (dotimes (_ (/ n 2))
        (goto-char (1+ (random (point-max))))
        (insert ?X)))))

(perf-define-variable-test perf-delete-before (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (goto-char 1)
    (overlay-recenter (point-min))
    (benchmark-run 1
      (dotimes (_ (/ n 2))
        (delete-char 1)))))

(perf-define-variable-test perf-delete-after (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (goto-char (point-max))
    (overlay-recenter (point-max))
    (benchmark-run 1
      (dotimes (_ (/ n 2))
        (delete-char -1)))))

(perf-define-variable-test perf-delete-scatter (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (goto-char (point-max))
    (benchmark-run 1
      (dotimes (_ (/ n 2))
        (goto-char (max 1 (random (point-max))))
        (delete-char 1)))))

(perf-define-test-suite perf-insert-delete-suite
  'perf-insert-before
  'perf-insert-after
  'perf-insert-scatter
  'perf-delete-before
  'perf-delete-after
  'perf-delete-scatter
  )


;; +===================================================================================+
;; | Redisplay (new)
;; +===================================================================================+

;; 5000
;; 25000
;; 75000

;; Number of Overlays =  N / 2
;;
;; (except for the hierarchical case, where it is divided by 10.)

  ;; . scrolling through a buffer with lots of overlays that affect faces
  ;;   of characters in the buffer text
  ;; . scrolling through a buffer with lots of overlays that define
  ;;   'display' properties which are strings
  ;; . scrolling through a buffer with lots of overlays that define
  ;;   'invisible' properties

(perf-define-test-suite perf-display-suite
  (perf-define-display-test sequential display scroll)
  (perf-define-display-test sequential display random)
  (perf-define-display-test sequential face scroll)
  (perf-define-display-test sequential face random)
  (perf-define-display-test sequential invisible scroll)
  (perf-define-display-test sequential invisible random)
  (perf-define-display-test random display scroll)
  (perf-define-display-test random display random)
  (perf-define-display-test random face scroll)
  (perf-define-display-test random face random)
  (perf-define-display-test random invisible scroll)
  (perf-define-display-test random invisible random))

;; |------------|
;;   |--------|
;;     |----|
(perf-define-display-test hierarchical face scroll)




;; +===================================================================================+
;; | Real World
;; +===================================================================================+

(require 'python)

(defconst perf-many-errors-file
  (expand-file-name "many-errors.py"
                    (and load-file-name (file-name-directory load-file-name))))

(perf-define-constant-test perf-realworld-flycheck
  (interactive)
  (package-initialize)
  (when (and (require 'flycheck nil t)
             (file-exists-p perf-many-errors-file)
             (or (executable-find "pylint")
                 (executable-find "flake8")))
    (setq flycheck-python-pylint-executable
          (executable-find "pylint"))
    (setq flycheck-python-flake8-executable
          (executable-find "flake8"))
    (setq python-indent-guess-indent-offset-verbose nil)
    (setq flycheck-check-syntax-automatically nil)
    (setq flycheck-checker-error-threshold nil)
    (setq flycheck-display-errors-function nil)
    (with-current-buffer (find-file-noselect perf-many-errors-file)
      (let* ((done)
             (flycheck-after-syntax-check-hook
              (list (lambda () (setq done t)))))
        (flycheck-mode 1)
        (flycheck-buffer)
        (benchmark-run 1
          (while (not done)
            (accept-process-output))
          (perf-switch-to-buffer-scroll-up-and-down)
          (flycheck-mode -1))))))

;; https://lists.gnu.org/archive/html/emacs-devel/2009-04/msg00242.html
(defun make-lines-invisible (regexp &optional arg)
  "Make all lines matching a regexp invisible and intangible.
With a prefix arg, make it visible again.  It is not necessary
that REGEXP matches the whole line; if a hit is found, the
affected line gets automatically selected.

This command affects the whole buffer."
  (interactive "MRegexp: \nP")
  (let (ov
        ovs
        count)
    (cond
     ((equal arg '(4))
      (setq ovs (overlays-in (point-min) (point-max)))
      (mapc (lambda (o)
              (if (overlay-get o 'make-lines-invisible)
                  (delete-overlay o)))
            ovs))
     (t
      (save-excursion
        (goto-char (point-min))
        (setq count 0)
        (while (re-search-forward regexp nil t)
          (setq count (1+ count))
          (if (= (% count 100) 0)
              (message "%d" count))
          (setq ov (make-overlay (line-beginning-position)
                                 (1+ (line-end-position))))
          (overlay-put ov 'make-lines-invisible t)
          (overlay-put ov 'invisible t)
          (overlay-put ov 'intangible t)
          (goto-char (line-end-position))))))))

(perf-define-constant-test perf-realworld-make-lines-invisible
  (with-temp-buffer
    (insert-file-contents "/usr/share/dict/words")
    (set-window-buffer nil (current-buffer))
    (redisplay t)
    (overlay-recenter (point-max))
    (benchmark-run 1
      (make-lines-invisible "a"))))

(perf-define-constant-test perf-realworld-line-numbering
  (interactive)
  (with-temp-buffer
    (insert-file-contents "/usr/share/dict/words")
    (overlay-recenter (point-max))
    (goto-char (point-min))
    (let* ((nlines (count-lines (point-min) (point-max)))
           (line 1)
           (width 0))
      (dotimes (i nlines) ;;-with-progress-reporter "Creating overlays"
        (let ((ov (make-overlay (point) (point)))
              (str (propertize (format "%04d" line) 'face 'shadow)))
          (overlay-put ov 'before-string
                       (propertize " " 'display `((margin left-margin) ,str)))
          (setq width (max width (length str)))
          (cl-incf line)
          (forward-line)))
      (benchmark-run 1
        (let ((left-margin-width width))
          (perf-switch-to-buffer-scroll-up-and-down))))))

(perf-define-test-suite perf-realworld-suite
  'perf-realworld-flycheck
  'perf-realworld-make-lines-invisible
  'perf-realworld-line-numbering)


;; +===================================================================================+
;; | next-overlay-change
;; +===================================================================================+

(perf-define-variable-test perf-noc-hierarchical/forward/linear (n)
  "Search linear for the next change on every line."
  (with-temp-buffer
    (perf-insert-lines (* 3 n))
    (perf-insert-overlays-hierarchical n)
    (goto-char (point-min))
    (benchmark-run 1
      (while (not (eobp))
        (next-overlay-change (point))
        (forward-line)))))

(perf-define-variable-test perf-noc-sequential/forward/linear (n)
  "Search linear for the next change on every line."
  (with-temp-buffer
    (perf-insert-lines (* 3 n))
    (perf-insert-overlays-sequential n)
    (goto-char (point-min))
    (benchmark-run 1
      (while (not (eobp))
        (next-overlay-change (point))
        (forward-line)))))

(perf-define-variable-test perf-noc-hierarchical/forward/backnforth (n)
  "Search back and forth for the next change from `point-min' to `point-max'."
  (with-temp-buffer
    (perf-insert-lines (* 3 n))
    (overlay-recenter (point-max))
    (perf-insert-overlays-hierarchical n)
    (goto-char (point-min))
    (benchmark-run 1
      (while (not (eobp))
        (next-overlay-change (point))
        (next-overlay-change (+ (point) 2))
        (forward-char)))))

(perf-define-test-suite perf-noc-suite
  'perf-noc-hierarchical/forward/linear
  'perf-noc-hierarchical/forward/backnforth
  'perf-noc-hierarchical/forward/backnforth)
