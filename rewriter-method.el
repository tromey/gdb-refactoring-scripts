;; Rewrite F(x) to x->g()
;; Handle multiple transforms at once.

(defvar rw-from nil)
(defvar rw-alist nil)

(let ((from-list))
  (while argv
    (let ((item (pop argv)))
      (push item from-list)
      (push (cons item (pop argv)) rw-alist)))
  (setq rw-from (concat "\\_<\\("
			(mapconcat #'identity from-list "\\|")
			"\\)\\_>")))

(defun rw-method-replace ()
  (while (re-search-forward rw-from nil t)
    (let ((start (match-beginning 0))
	  (item (match-string 0)))
      (goto-char (match-end 0))
      (skip-chars-forward " \t\n")
      (when (looking-at "(")
	(let ((end (1+ (point))))
	  (forward-list)
	  (backward-char)
	  (delete-region start end)
	  (delete-char 1)
	  (insert "->" (cdr (assoc item rw-alist)) " ()"))))))

(rw-rewrite #'rw-method-replace)
