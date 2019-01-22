;; rewriter base code.

(defvar rw-directory (expand-file-name "."))

(require 'add-log)
(setq add-log-always-start-new-record t)

(require 'subr-x)
(require 'cl-macs)

(setq add-log-full-name
      (string-trim (shell-command-to-string "git config user.name")))

(setq add-log-mailing-address
      (string-trim (shell-command-to-string "git config user.email")))

(defconst rw-base (file-name-directory load-file-name))
(push rw-base load-path)

(unless argv
  (error "Usage: gdb-rewriter COMMAND [ARG...]"))

(defconst rw-subcommand (pop argv))

(defvar rw-memoize-files nil)

(defun rw-files ()
  (unless rw-memoize-files
    (setq rw-memoize-files
	  (sort
	   (cl-remove-if
	    (lambda (name)
	      (string-match "/\\(gnulib\\|testsuite\\)/" name))
	    (directory-files-recursively rw-directory "\\.[cyhl]$"))
	   #'string<)))
  rw-memoize-files)

(defun rw-rewrite (callback)
  (dolist (file (rw-files))
    ;; FIXME verbose mode
    (message "Processing %s" file)
    (find-file file)
    (goto-char (point-min))
    (funcall callback)
    (when (buffer-modified-p)
      (save-buffer))
    (kill-buffer)))

(defun rw-save-buffers ()
  "Save unsaved buffers.  Handy for ChangeLog."
  (dolist (buf (buffer-list))
    (set-buffer buf)
    (when (and buffer-file-name (buffer-modified-p))
      (save-buffer))))

(defun rw-add-change-log-entry ()
  (save-excursion
    (add-change-log-entry)
    (setq add-log-always-start-new-record nil)))

;; This is local to the ChangeLog.
(defvar-local rw-was-first-cl-entry t)

(defun rw-final-change-log-text (text)
  ;; Called in the source buffer, only do something if modified.
  (if (buffer-modified-p)
      (save-excursion
	(find-file (find-change-log))
	;; Point should already be at the right spot.
	(insert text)
	(fill-paragraph)
	(unless rw-was-first-cl-entry
	  (delete-blank-lines))
	(setq rw-was-first-cl-entry nil))))

(load (concat "rewriter-" rw-subcommand))
(rw-save-buffers)
