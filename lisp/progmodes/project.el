;;; project.el --- Operations on the current project  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2019 Free Software Foundation, Inc.

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

;;; Commentary:

;; This file contains generic infrastructure for dealing with
;; projects, some utility functions, and commands using that
;; infrastructure.
;;
;; The goal is to make it easier for Lisp programs to operate on the
;; current project, without having to know which package handles
;; detection of that project type, parsing its config files, etc.
;;
;; NOTE: The project API is still experimental and can change in major,
;; backward-incompatible ways.  Everyone is encouraged to try it, and
;; report to us any problems or use cases we hadn't anticipated, by
;; sending an email to emacs-devel, or `M-x report-emacs-bug'.
;;
;; Infrastructure:
;;
;; Function `project-current', to determine the current project
;; instance, and 5 (at the moment) generic functions that act on it.
;; This list is to be extended in future versions.
;;
;; Utils:
;;
;; `project-combine-directories' and `project-subtract-directories',
;; mainly for use in the abovementioned generics' implementations.
;;
;; Commands:
;;
;; `project-find-file', `project-find-regexp' and
;; `project-or-external-find-regexp' use the current API, and thus
;; will work in any project that has an adapter.

;;; TODO:

;; * Reliably cache the list of files in the project, probably using
;;   filenotify.el (if supported) to invalidate.  And avoiding caching
;;   if it's not available (manual cache invalidation is not nice).
;;
;; * Build tool related functionality.  Start with a `project-build'
;;   command, which should provide completions on tasks to run, and
;;   maybe allow entering some additional arguments.  This might
;;   be handled better with a separate API, though.  Then we won't
;;   force every project backend to be aware of the build tool(s) the
;;   project is using.
;;
;; * Command to (re)build the tag files in all project roots.  To that
;;   end, we might need to add a way to provide file whitelist
;;   wildcards for each root to limit etags to certain files (in
;;   addition to the blacklist provided by ignores), and/or allow
;;   specifying additional tag regexps.
;;
;; * UI for the user to be able to pick the current project for the
;;   whole Emacs session, independent of the current directory.  Or,
;;   in the more advanced case, open a set of projects, and have some
;;   project-related commands to use them all.  E.g., have a command
;;   to search for a regexp across all open projects.  Provide a
;;   history of projects that were opened in the past (storing it as a
;;   list of directories should suffice).
;;
;; * Support for project-local variables: a UI to edit them, and a
;;   utility function to retrieve a value.  Probably useless without
;;   support in various built-in commands.  In the API, we might get
;;   away with only adding a `project-configuration-directory' method,
;;   defaulting to the project root the current file/buffer is in.
;;   And prompting otherwise.  How to best mix that with backends that
;;   want to set/provide certain variables themselves, is up for
;;   discussion.

;;; Code:

(require 'cl-generic)

(defvar project-find-functions (list #'project-try-vc)
  "Special hook to find the project containing a given directory.
Each functions on this hook is called in turn with one
argument (the directory) and should return either nil to mean
that it is not applicable, or a project instance.")

;;;###autoload
(defun project-current (&optional maybe-prompt dir)
  "Return the project instance in DIR or `default-directory'.
When no project found in DIR, and MAYBE-PROMPT is non-nil, ask
the user for a different directory to look in.  If that directory
is not a part of a detectable project either, return a
`transient' project instance rooted in it."
  (unless dir (setq dir default-directory))
  (let ((pr (project--find-in-directory dir)))
    (cond
     (pr)
     (maybe-prompt
      (setq dir (read-directory-name "Choose the project directory: " dir nil t)
            pr (project--find-in-directory dir))
      (unless pr
        (message "Using '%s' as a transient project root" dir)
        (setq pr (cons 'transient dir)))))
    pr))

(defun project--find-in-directory (dir)
  (run-hook-with-args-until-success 'project-find-functions dir))

(cl-defgeneric project-roots (project)
  "Return the list of directory roots of the current project.

Most often it's just one directory which contains the project
build file and everything else in the project.  But in more
advanced configurations, a project can span multiple directories.

The directory names should be absolute.")

;; FIXME: Add MODE argument, like in `ede-source-paths'?
(cl-defgeneric project-external-roots (_project)
  "Return the list of external roots for PROJECT.

It's the list of directories outside of the project that are
still related to it.  If the project deals with source code then,
depending on the languages used, this list should include the
headers search path, load path, class path, and so on.

The rule of thumb for whether to include a directory here, and
not in `project-roots', is whether its contents are meant to be
edited together with the rest of the project."
  nil)

(cl-defgeneric project-ignores (_project _dir)
  "Return the list of glob patterns to ignore inside DIR.
Patterns can match both regular files and directories.
To root an entry, start it with `./'.  To match directories only,
end it with `/'.  DIR must be one of `project-roots' or
`project-external-roots'."
  ;; TODO: Document and support regexp ignores as used by Hg.
  ;; TODO: Support whitelist entries.
  (require 'grep)
  (defvar grep-find-ignored-files)
  (nconc
   (mapcar
    (lambda (dir)
      (concat dir "/"))
    vc-directory-exclusion-list)
   grep-find-ignored-files))

(cl-defgeneric project-file-completion-table (project dirs)
  "Return a completion table for files in directories DIRS in PROJECT.
DIRS is a list of absolute directories; it should be some
subset of the project roots and external roots.

The default implementation delegates to `project-files'."
  (let ((all-files (project-files project dirs)))
    (lambda (string pred action)
      (cond
       ((eq action 'metadata)
        '(metadata . ((category . project-file))))
       (t
        (complete-with-action action all-files string pred))))))

(cl-defmethod project-roots ((project (head transient)))
  (list (cdr project)))

(cl-defgeneric project-files (project &optional dirs)
  "Return a list of files in directories DIRS in PROJECT.
DIRS is a list of absolute directories; it should be some
subset of the project roots and external roots.

The default implementation uses `find-program'.  PROJECT is used
to find the list of ignores for each directory."
  (require 'xref)
  (cl-mapcan
   (lambda (dir)
     (project--files-in-directory dir
                                  (project--dir-ignores project dir)))
   (or dirs (project-roots project))))

(defun project--files-in-directory (dir ignores &optional files)
  (require 'find-dired)
  (defvar find-name-arg)
  (let ((default-directory dir)
        (command (format "%s %s %s -type f %s -print0"
                         find-program
                         (file-local-name dir)
                         (xref--find-ignores-arguments
                          ignores
                          (expand-file-name dir))
                         (if files
                             (concat (shell-quote-argument "(")
                                     " " find-name-arg " "
                                     (mapconcat
                                      #'shell-quote-argument
                                      (split-string files)
                                      (concat " -o " find-name-arg " "))
                                     " "
                                     (shell-quote-argument ")"))"")
                         )))
    (project--remote-file-names
     (split-string (shell-command-to-string command) "\0" t))))

(defun project--remote-file-names (local-files)
  "Return LOCAL-FILES as if they were on the system of `default-directory'."
  (let ((remote-id (file-remote-p default-directory)))
    (if (not remote-id)
        local-files
      (mapcar (lambda (file)
                (concat remote-id file))
              local-files))))

(defgroup project-vc nil
  "Project implementation using the VC package."
  :version "25.1"
  :group 'tools)

(defcustom project-vc-ignores nil
  "List of patterns to include in `project-ignores'."
  :type '(repeat string)
  :safe 'listp)

;; FIXME: Using the current approach, major modes are supposed to set
;; this variable to a buffer-local value.  So we don't have access to
;; the "external roots" of language A from buffers of language B, which
;; seems desirable in multi-language projects, at least for some
;; potential uses, like "jump to a file in project or external dirs".
;;
;; We could add a second argument to this function: a file extension,
;; or a language name.  Some projects will know the set of languages
;; used in them; for others, like VC-based projects, we'll need
;; auto-detection.  I see two options:
;;
;; - That could be implemented as a separate second hook, with a
;;   list of functions that return file extensions.
;;
;; - This variable will be turned into a hook with "append" semantics,
;;   and each function in it will perform auto-detection when passed
;;   nil instead of an actual file extension.  Then this hook will, in
;;   general, be modified globally, and not from major mode functions.
;;
;; The second option seems simpler, but the first one has the
;; advantage that the user could override the list of languages used
;; in a project via a directory-local variable, thus skipping
;; languages they're not working on personally (in a big project), or
;; working around problems in language detection (the detection logic
;; might be imperfect for the project in question, or it might work
;; too slowly for the user's taste).
(defvar project-vc-external-roots-function (lambda () tags-table-list)
  "Function that returns a list of external roots.

It should return a list of directory roots that contain source
files related to the current buffer.

The directory names should be absolute.  Used in the VC project
backend implementation of `project-external-roots'.")

(defun project-try-vc (dir)
  (let* ((backend (ignore-errors (vc-responsible-backend dir)))
         (root (and backend (ignore-errors
                              (vc-call-backend backend 'root dir)))))
    (and root (cons 'vc root))))

(cl-defmethod project-roots ((project (head vc)))
  (list (cdr project)))

(cl-defmethod project-external-roots ((project (head vc)))
  (project-subtract-directories
   (project-combine-directories
    (mapcar
     #'file-name-as-directory
     (funcall project-vc-external-roots-function)))
   (project-roots project)))

(cl-defmethod project-ignores ((project (head vc)) dir)
  (let* ((root (cdr project))
          backend)
    (append
     (when (file-equal-p dir root)
       (setq backend (vc-responsible-backend root))
       (mapcar
        (lambda (entry)
          (if (string-match "\\`/" entry)
              (replace-match "./" t t entry)
            entry))
        (vc-call-backend backend 'ignore-completion-table root)))
     (project--value-in-dir 'project-vc-ignores root)
     (mapcar
      (lambda (dir)
        (concat dir "/"))
      vc-directory-exclusion-list))))

(defun project-combine-directories (&rest lists-of-dirs)
  "Return a sorted and culled list of directory names.
Appends the elements of LISTS-OF-DIRS together, removes
non-existing directories, as well as directories a parent of
whose is already in the list."
  (let* ((dirs (sort
                (mapcar
                 (lambda (dir)
                   (file-name-as-directory (expand-file-name dir)))
                 (apply #'append lists-of-dirs))
                #'string<))
         (ref dirs))
    ;; Delete subdirectories from the list.
    (while (cdr ref)
      (if (string-prefix-p (car ref) (cadr ref))
          (setcdr ref (cddr ref))
        (setq ref (cdr ref))))
    (cl-delete-if-not #'file-exists-p dirs)))

(defun project-subtract-directories (files dirs)
  "Return a list of elements from FILES that are outside of DIRS.
DIRS must contain directory names."
  ;; Sidestep the issue of expanded/abbreviated file names here.
  (cl-set-difference files dirs :test #'file-in-directory-p))

(defun project--value-in-dir (var dir)
  (with-temp-buffer
    (setq default-directory dir)
    (let ((enable-local-variables :all))
      (hack-dir-local-variables-non-file-buffer))
    (symbol-value var)))

(declare-function grep-read-files "grep")
(declare-function xref--show-xrefs "xref")
(declare-function xref-backend-identifier-at-point "xref")
(declare-function xref--find-ignores-arguments "xref")

;;;###autoload
(defun project-find-regexp (regexp)
  "Find all matches for REGEXP in the current project's roots.
With \\[universal-argument] prefix, you can specify the directory
to search in, and the file name pattern to search for.  The
pattern may use abbreviations defined in `grep-files-aliases',
e.g. entering `ch' is equivalent to `*.[ch]'.  As whitespace
triggers completion when entering a pattern, including it
requires quoting, e.g. `\\[quoted-insert]<space>'."
  (interactive (list (project--read-regexp)))
  (let* ((pr (project-current t))
         (files
          (if (not current-prefix-arg)
              (project-files pr (project-roots pr))
            (let ((dir (read-directory-name "Base directory: "
                                            nil default-directory t)))
              (project--files-in-directory dir
                                           (project--dir-ignores pr dir)
                                           (grep-read-files regexp))))))
    (project--find-regexp-in-files regexp files)))

(defun project--dir-ignores (project dir)
  (let* ((roots (project-roots project))
         (root (cl-find dir roots :test #'file-in-directory-p)))
    (if (not root)
        (project-ignores nil nil)       ;The defaults.
      (let ((ignores (project-ignores project root)))
        (if (file-equal-p root dir)
            ignores
          ;; FIXME: Update the "rooted" ignores to relate to DIR instead.
          (cl-delete-if (lambda (str) (string-prefix-p "./" str))
                        ignores))))))

;;;###autoload
(defun project-or-external-find-regexp (regexp)
  "Find all matches for REGEXP in the project roots or external roots.
With \\[universal-argument] prefix, you can specify the file name
pattern to search for."
  (interactive (list (project--read-regexp)))
  (let* ((pr (project-current t))
         (files
          (project-files pr (append
                             (project-roots pr)
                             (project-external-roots pr)))))
    (project--find-regexp-in-files regexp files)))

(defun project--find-regexp-in-files (regexp files)
  (pcase-let*
      ((output (get-buffer-create " *project grep output*"))
       (`(,grep-re ,file-group ,line-group . ,_) (car grep-regexp-alist))
       (status nil)
       (hits nil)
       (xrefs nil)
       (command (format "xargs -0 grep %s -nHe %s"
                        (if (and case-fold-search
                                 (isearch-no-upper-case-p regexp t))
                            "-i"
                          "")
                        (shell-quote-argument (xref--regexp-to-extended regexp)))))
    (with-current-buffer output
      (erase-buffer)
      (with-temp-buffer
        (insert (mapconcat #'identity files "\0"))
        (setq status
              (project--process-file-region (point-min)
                                            (point-max)
                                            shell-file-name
                                            output
                                            nil
                                            shell-command-switch
                                            command)))
      (goto-char (point-min))
      (when (and (/= (point-min) (point-max))
                 (not (looking-at grep-re))
                 ;; TODO: Show these matches as well somehow?
                 (not (looking-at "Binary file .* matches")))
        (user-error "Search failed with status %d: %s" status
                    (buffer-substring (point-min) (line-end-position))))
      (while (re-search-forward grep-re nil t)
        (push (list (string-to-number (match-string line-group))
                    (match-string file-group)
                    (buffer-substring-no-properties (point) (line-end-position)))
              hits)))
    (setq xrefs (xref--convert-hits (nreverse hits) regexp))
    (unless xrefs
      (user-error "No matches for: %s" regexp))
    (xref--show-xrefs xrefs nil)))

(defun project--process-file-region (start end program
                                     &optional buffer display
                                     &rest args)
  ;; FIXME: This branching shouldn't be necessary, but
  ;; call-process-region *is* measurably faster, even for a program
  ;; doing some actual work (for a period of time). Even though
  ;; call-process-region also creates a temp file internally
  ;; (http://lists.gnu.org/archive/html/emacs-devel/2019-01/msg00211.html).
  (if (not (file-remote-p default-directory))
      (apply #'call-process-region
             start end program nil buffer display args)
    (let ((infile (make-temp-file "ppfr")))
      (unwind-protect
          (progn
            (write-region start end infile nil 'silent)
            (apply #'process-file program infile buffer display args))
        (delete-file infile)))))

(defun project--read-regexp ()
  (let ((id (xref-backend-identifier-at-point (xref-find-backend))))
    (read-regexp "Find regexp" (and id (regexp-quote id)))))

;;;###autoload
(defun project-find-file ()
  "Visit a file (with completion) in the current project's roots.
The completion default is the filename at point, if one is
recognized."
  (interactive)
  (let* ((pr (project-current t))
         (dirs (project-roots pr)))
    (project-find-file-in (thing-at-point 'filename) dirs pr)))

;;;###autoload
(defun project-or-external-find-file ()
  "Visit a file (with completion) in the current project's roots or external roots.
The completion default is the filename at point, if one is
recognized."
  (interactive)
  (let* ((pr (project-current t))
         (dirs (append
                (project-roots pr)
                (project-external-roots pr))))
    (project-find-file-in (thing-at-point 'filename) dirs pr)))

(defun project-find-file-in (filename dirs project)
  "Complete FILENAME in DIRS in PROJECT and visit the result."
  (let* ((table (project-file-completion-table project dirs))
         (file (project--completing-read-strict
                "Find file" table nil nil
                filename)))
    (if (string= file "")
        (user-error "You didn't specify the file")
      (find-file file))))

(defun project--completing-read-strict (prompt
                                        collection &optional predicate
                                        hist default inherit-input-method)
  ;; Tried both expanding the default before showing the prompt, and
  ;; removing it when it has no matches.  Neither seems natural
  ;; enough.  Removal is confusing; early expansion makes the prompt
  ;; too long.
  (let* ((common-parent-directory
          (let ((common-prefix (try-completion "" collection)))
            (if (> (length common-prefix) 0)
                (file-name-directory common-prefix))))
         (cpd-length (length common-parent-directory))
         (prompt (if (zerop cpd-length)
                     prompt
                   (concat prompt (format " in %s" common-parent-directory))))
         ;; XXX: This requires collection to be "flat" as well.
         (substrings (mapcar (lambda (s) (substring s cpd-length))
                             (all-completions "" collection)))
         (new-collection
          (lambda (string pred action)
            (cond
             ((eq action 'metadata)
              (if (functionp collection) (funcall collection nil nil 'metadata)))
             (t
	      (complete-with-action action substrings string pred)))))
         (new-prompt (if default
                         (format "%s (default %s): " prompt default)
                       (format "%s: " prompt)))
         (res (completing-read new-prompt
                               new-collection predicate t
                               nil ;; initial-input
                               hist default inherit-input-method)))
    (when (and (equal res default)
               (not (test-completion res collection predicate)))
      (setq res
            (completing-read (format "%s: " prompt)
                             new-collection predicate t res hist nil
                             inherit-input-method)))
    (concat common-parent-directory res)))

(declare-function fileloop-continue "fileloop" ())

;;;###autoload
(defun project-search (regexp)
  "Search for REGEXP in all the files of the project.
Stops when a match is found.
To continue searching for next match, use command \\[fileloop-continue]."
  (interactive "sSearch (regexp): ")
  (fileloop-initialize-search
   regexp (project-files (project-current t)) 'default)
  (fileloop-continue))

;;;###autoload
(defun project-query-replace-regexp (from to)
  "Search for REGEXP in all the files of the project.
Stops when a match is found.
To continue searching for next match, use command \\[fileloop-continue]."
  (interactive
   (pcase-let ((`(,from ,to)
                (query-replace-read-args "Query replace (regexp)" t t)))
     (list from to)))
  (fileloop-initialize-replace
   from to (project-files (project-current t)) 'default)
  (fileloop-continue))

(provide 'project)
;;; project.el ends here
