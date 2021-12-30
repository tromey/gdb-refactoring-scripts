;; rewrite a single gdbarch method.

;; Usage: emacs --script rewriter.el gdbarch

(defvar rw-arch-method)

(defvar rw-arch-setter)
(defvar rw-arch-getter)
(defvar rw-arch-p)

(defun rw-arch-setter ()
  (while (re-search-forward rw-arch-setter nil t)
    (let ((start (match-beginning 0))
	  lhs)
      (goto-char (match-end 0))
      (skip-syntax-forward " ")
      (when (looking-at "(")
	(forward-char)
	(setq lhs (rw-extract-and-skip-expr ","))
	;; Skip the ",".
	(forward-char)
	(skip-syntax-forward " ")
	(delete-region start (point))
	(insert lhs "->" rw-arch-method ".set (")))))

(defun rw-arch-getter ()
  (while (re-search-forward rw-arch-getter nil t)
    (let ((start (match-beginning 0))
	  (is-p-form (match-beginning 1))
	  lhs)
      (goto-char (match-end 0))
      (skip-syntax-forward " ")
      (when (looking-at "(")
	(forward-char)
	(setq lhs (rw-extract-and-skip-expr "[,)]"))
	(when (looking-at ",")
	  ;; Skip the ",".
	  (forward-char)
	  (skip-syntax-forward " "))
	(delete-region start (point))
	(insert lhs "->" rw-arch-method
		(if is-p-form
		    ".is_set"
		  "")
		" (")))))

(defun rw-arch-process ()
  (let ((rw-arch-setter (concat "\\_<set_gdbarch_" rw-arch-method "\\_>"))
	(rw-arch-getter (concat "\\_<gdbarch_" rw-arch-method "\\(_p\\)?\\_>"))
	(rw-arch-p (concat "\\_<gdbarch_" rw-arch-method "\\_>")))
    (unless buffer-read-only
      (rw-arch-setter)
      (goto-char (point-min))
      (rw-arch-getter))))

(defun rw-scan-gdbarch.sh ()
  (find-file "gdbarch.sh")
  (goto-char (point-min))
  (re-search-forward "cat <<EOF")
  (forward-line)
  (while (not (looking-at "EOF"))
    ;; Treat non-matching lines as comments.
    (when (looking-at "^[^;\n]+;[^;\n]+;\\([^;\n]+\\);")
      (let ((rw-arch-method (match-string 1))
	    (changed nil))
	(message "Processing %s" rw-arch-method)
	(save-excursion
	  (dolist (file (rw-files))
	    (find-file file)
	    (goto-char (point-min))
	    (rw-arch-process)
	    (when (buffer-modified-p)
	      (setq changed t)
	      (save-buffer))))
	(if changed
	    (rw-git-commit (concat "Changes for " rw-arch-method))
	  ;; This shouldn't happen, but let's check.
	  (message (concat "!!!! No changes for " rw-arch-method)))))
    (forward-line)))

(rw-scan-gdbarch.sh)
