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

(defun rw-rewrite-guard (new-name text)
  (goto-char (point-min))
  (forward-comment 1)
  (insert "\n\n#ifndef " new-name "\n"
	  "#define " new-name "\n\n")
  (delete-region (point) (save-excursion
			   (skip-chars-forward " \t\r\n")
			   (point)))
  (goto-char (point-max))
  (delete-blank-lines)
  (insert "\n#endif /* " new-name " */\n"))

(defun rw-rewrite-guards ()
  (when (and (not buffer-read-only)
	     (string-match "\\.h$" (buffer-file-name)))
    ;; Skip all available.
    (forward-comment 9999)
    (let* ((rel-name (file-relative-name (buffer-file-name)
					 (if (equal rw-subdir-name "gdb")
					     rw-directory
					   rw-base-directory)))
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
		  (rw-rewrite-guard include-name "Rename include guard."))
	      (if (not (eq end :ok))
		  (message "... missing include guard!?"))))
	;; No include guard.
	(rw-rewrite-guard include-name "Add include guard.")))))

(rw-rewrite #'rw-rewrite-guards)
