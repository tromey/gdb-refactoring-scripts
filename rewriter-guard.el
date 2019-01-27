;; Add include guards

(defun rw-check-end-guard-and-zap ()
  (goto-char (point-max))
  (skip-chars-backward " \t\r\n")
  (beginning-of-line)
  (when (looking-at "#endif")
    (delete-region (point) (point-max))
    t))

(defun rw-check-guard (name)
  ;; This accepts a bit too much but we don't care.
  (when (looking-at "#if\\(ndef\\|[ \t]*![ \t]*defined\\)[ \t]*(?\\([^)\n]+\\))?$")
    ;; Check this guard.
    (let ((old-name (match-string 2)))
      (if (string= name old-name)
	  :ok
	(forward-line)
	(when (looking-at (concat "#define\\s-+"
				  (regexp-quote old-name)
				  "\\([ \t]+1\\)?$"))
	  (forward-line)
	  (when (save-excursion (rw-check-end-guard-and-zap))
	    (point)))))))

(defun rw-rewrite-guard (new-name where text)
  (goto-char (point-min))
  (rw-add-change-log-entry)
  (goto-char where)
  (delete-blank-lines)
  (insert "\n#ifndef " new-name "\n"
	  "#define " new-name "\n\n")
  (goto-char (point-max))
  (delete-blank-lines)
  (insert "\n#endif /* " new-name " */\n")
  (rw-final-change-log-text text))

(defun rw-rewrite-guards ()
  (when (and (not buffer-read-only)
	     (string-match "\\.h$" (buffer-file-name)))
    ;; Skip all available.
    (forward-comment 9999)
    (let* ((rel-name (file-relative-name (buffer-file-name) rw-directory))
	   (include-name (string-join (split-string
				       (upcase rel-name) "[-./]") "_")))
      (if (and (looking-at "#if\\(ndef\\| ![ \t]*defined\\)")
	       (save-excursion
		 (save-match-data
		   (goto-char (c-scan-conditionals 1))
		   (skip-chars-forward " \t\r\n")
		   (eobp))))
	  (let ((start (match-beginning 0))
		(end (rw-check-guard include-name)))
	    (if (integerp end)
		(progn
		  (delete-region start end)
		  (rw-rewrite-guard include-name start
				    "Rename include guard."))
	      (if (not (eq end :ok))
		  (message "... missing include guard!?"))))
	;; No include guard.
	(goto-char (point-min))
	;; Put the include guard just after the GPL comment.
	(forward-comment 1)
	(skip-chars-forward " \t\r\n")
	(rw-rewrite-guard include-name (point) "Add include guard.")))))

(rw-rewrite #'rw-rewrite-guards)
