;; Rewrite throw_exception to "throw" in some cases

(defun rw-rewrite-rethrow ()
  (while (re-search-forward "catch (.*&\\(.*\\))[ \t]*$" nil t)
    (let ((var-name (match-string 1)))
      (end-of-line)
      (search-forward "{")
      (backward-char)
      (let ((bound (save-excursion
		     (forward-list)
		     (point)))
	    (throw-expr (regexp-quote (concat "throw_exception ("
					      var-name
					      ")"))))
	(while (re-search-forward throw-expr bound t)
	  (replace-match "throw" t t))))))

(rw-rewrite #'rw-rewrite-rethrow)
