;; rewriter base code.

(defvar rw-directory ".")

;; FIXME should just scan the directory
(defvar rw-subdirs  '("." "cli" "mi" "python" "tui" "guile" "nat"
		      "target" "common"))

(require 'add-log)
(setq add-log-always-start-new-record t)

(defconst rw-base (file-name-directory load-file-name))
(push rw-base load-path)

(unless argv
  (error "Usage: gdb-rewriter COMMAND [ARG...]"))

(defconst rw-subcommand (pop argv))

(defun rw-files ()
  (sort (apply #'nconc
	       (mapcar (lambda (dir)
			 (directory-files (expand-file-name dir rw-directory)
					  t "\\.[cyhl]$"))
		       rw-subdirs))
	#'string<))

(defun rw-rewrite (callback)
  (dolist (file (rw-files))
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
  (add-change-log-entry)
  (setq add-log-always-start-new-record nil))

(load (concat "rewriter-" rw-subcommand))
(rw-save-buffers)
