;;; Look for malformed TRY_CATCH

(defconst dir-to-scan (pop argv))

(defun scan-try-catch ()
  (unless (file-directory-p dir-to-scan)
    (error "Usage: emacs --script try-catch.el DIR"))

  ;; Lame subdir list.
  (dolist (subdir '("." "cli" "mi" "python" "tui"))
    (dolist (file (directory-files (expand-file-name subdir dir-to-scan)
				   t "[.][chy]$"))
      (save-excursion
	(find-file file)
	(goto-char (point-min))
	(message "Processing %s" file)
	;; Skip #define TRY_CATCH ..
	(while (re-search-forward "^[^#].*\\_<TRY_CATCH\\_>" nil 'move)
	  (let ((here (point)))
	    (skip-chars-forward " \t\n")
	    (if (not (looking-at "("))
		;; Probably a comment -- skip it.
		nil
	      (forward-sexp)
	      (skip-chars-forward " \t\n")
	      ;; It is hard to handle non-braced bodies in elisp.
	      (if (not (looking-at "{"))
		  (message "%s:%d: non-conforming TRY_CATCH"
			   file
			   (line-number-at-pos here))
		(let ((end (save-excursion
			     (forward-sexp)
			     (point))))
		  ;; Note that there's no need to scan nested
		  ;; TRY_CATCHes.
		  (while (re-search-forward "\\_<\\(goto\\|return\\)\\_>"
					    end 'move)
		    (if (not (nth 8 (syntax-ppss)))
			(message "%s:%d: %s in TRY_CATCH"
				 file
				 (line-number-at-pos (point))
				 (match-string 1)))))))))
	(kill-buffer)))))

(byte-compile 'scan-try-catch)

(scan-try-catch)
