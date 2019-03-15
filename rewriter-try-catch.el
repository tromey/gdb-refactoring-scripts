;; Rewrite TRY/CATCH.

(defconst rw-try-catch-rx
  (concat "^[ \t]*\\(?:"
	  ;; Match 1
	  "\\(TRY\\)"
	  ;; Match 2, 3, 4
	  "\\|\\(CATCH[ \t]*(\\([^, \t]*\\),[ \t]*\\([^ \t]*\\))\\)"
	  ;; Match 5
	  "\\|\\(END_CATCH;?\\)"
	  "\\)[ \t]*$"))

(defun rw-rewrite-try-catch ()
  (while (re-search-forward rw-try-catch-rx nil t)
    (cond
     ((match-beginning 1)
      (replace-match "try" t t nil 1))
     ((match-beginning 2)
      (replace-match "catch (const struct gdb_exception_\\4 &\\3)"
		     t nil nil 2))
     ((match-beginning 5)
      (beginning-of-line)
      (delete-region (point) (progn (forward-line) (point))))
     (t
      (error "BUG"))))
  (when (buffer-modified-p)
    (goto-char (point-min))
    (rw-add-change-log-entry)
    (rw-final-change-log-text "Use C++ exception handling.")))

(rw-rewrite #'rw-rewrite-try-catch)
