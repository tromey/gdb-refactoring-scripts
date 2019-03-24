;; Rewrite F(x) to x->g()

(when (>= (length argv) 3)
  (error "Usage: gdb-rewriter method FROM TO"))

(defconst rw-raw-from (pop argv))
(defconst rw-from (concat "\\_<" rw-raw-from "\\_>"))
(defconst rw-to (pop argv))

(defun rw-method-replace ()
  (while (re-search-forward rw-from nil t)
    (let ((start (match-beginning 0)))
      (goto-char (match-end 0))
      (skip-chars-forward " \t\n")
      (when (looking-at "(")
	(let ((end (1+ (point))))
	  (forward-list)
	  (backward-char)
	  (delete-region start end)
	  (delete-char 1)
	  (insert "->" rw-to " ()")
	  (rw-add-change-log-entry)))))
  (rw-final-change-log-text (concat "Replace \"" rw-raw-from
				    "\" with method call.")))

(rw-rewrite #'rw-method-replace)
