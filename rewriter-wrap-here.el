;; Rewrite wrap_here calls to use integer argument.

(defun rw-wrap-here ()
  (while (re-search-forward "wrap_here (" nil t)
    (goto-char (match-end 0))
    (cond
     ((looking-at "\" *\"")
      (let ((n (- (match-end 0) (match-beginning 0) 2)))
	(delete-region (match-beginning 0) (match-end 0))
	(insert (number-to-string n))))
     ((looking-at "n_spaces (")
      (delete-region (match-beginning 0) (match-end 0))
      (rw-skip-expr ")")
      (delete-char 1))
     (t
      (rw-error "non-matching call to wrap_here")))))

(rw-rewrite #'rw-wrap-here)
