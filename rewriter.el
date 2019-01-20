;; rewriter base code.

(defvar rw-directory ".")

(require 'add-log)
(setq add-log-always-start-new-record t)

(defconst rw-base (file-name-directory load-file-name))
(push rw-base load-path)

(unless argv
  (error "Usage: gdb-rewriter COMMAND [ARG...]"))

(defconst rw-subcommand (pop argv))

(defun rw-files ()
  (sort (directory-files-recursively rw-directory "\\.[cyhl]$") #'string<))

(defun rw-rewrite (callback)
  (dolist (file (rw-files))
    (unless (string-match "/\\(gnulib\\|testsuite\\)/" file)
      ;; FIXME verbose mode
      ;;(message "Processing %s" file)
      (find-file file)
      (goto-char (point-min))
      (funcall callback)
      (when (buffer-modified-p)
	(save-buffer))
      (kill-buffer))))

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
