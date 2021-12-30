;; rewriter base code.

(defvar rw-directory (expand-file-name "."))

;; Rewriter is only ever run in a subdirectory of the top-level
;; binutils-gdb repository.
(defvar rw-base-directory (expand-file-name ".."))

;; Which subdir we're in.
(defvar rw-subdir-name (file-name-nondirectory rw-base-directory))

(require 'subr-x)
(require 'cl-macs)

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
	   (if argv
	       (progn
		 (cl-assert (string= (pop argv) "--"))
		 argv)
	     (cl-remove-if
	      (lambda (name)
		(string-match "/\\(gnulib\\|testsuite\\)/" name))
	      (directory-files-recursively rw-directory "\\.\\([cyhl]\\|cc\\)$")))
	   ;; Note that we sort in reverse order, so that ChangeLog
	   ;; entries ultimately end up in the correct order.
	   #'string>)))
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
  "Save unsaved buffers."
  (dolist (buf (buffer-list))
    (set-buffer buf)
    (when (and buffer-file-name (buffer-modified-p))
      (save-buffer))))

(defun rw-skip-intro-comment ()
  ;; The intro comment.
  (forward-comment 1)
  (while (progn
	   (skip-chars-forward " \t\r\n")
	   (and (looking-at "/\\*")
		(not (rw-looking-at-h-comment))))
    (forward-comment 1)))

(defun rw-skip-expr (ender)
  (skip-syntax-forward " ")
  (while (not (looking-at ender))
    (forward-sexp)
    (skip-syntax-forward " ")))

(defun rw-extract-and-skip-expr (ender)
  (let ((start (point)))
    (rw-skip-expr ender)
    (buffer-substring start (point))))

(defun rw-git-commit (message)
  (call-process "git" nil nil nil "commit" "-a" "-m" message)
  ;; Reset the ChangeLog state.
  (setq add-log-always-start-new-record t))

(load (concat "rewriter-" rw-subcommand))
(rw-save-buffers)
