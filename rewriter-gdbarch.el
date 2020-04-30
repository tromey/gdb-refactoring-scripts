;; rewrite a single gdbarch method.

;; Usage: emacs --script rewriter.el gdbarch method-name

(defconst rw-arch-method (pop argv))

(defconst rw-arch-setter (concat "\\_<set_gdbarch_" rw-arch-method "\\_>"))
(defconst rw-arch-getter (concat "\\_<gdbarch_" rw-arch-method "\\(_p\\)?\\_>"))
(defconst rw-arch-p (concat "\\_<gdbarch_" rw-arch-method "\\_>"))

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
  (unless buffer-read-only
    (rw-arch-setter)
    (goto-char (point-min))
    (rw-arch-getter)))

(rw-rewrite #'rw-arch-process)
