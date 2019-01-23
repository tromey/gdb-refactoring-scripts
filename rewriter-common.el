;; Rewrite include x to include common/x

(defconst rw-common-dir (expand-file-name "common" rw-directory))

(defun rw-common-includes ()
  (goto-char (point-min))
  ;; could maybe check for "" -vs- <> consistency.
  (while (re-search-forward "^#\\s-*include \"\\([^\"]*\\)\"" nil t)
    (let ((name (match-string 1)))
      (unless (or (file-exists-p (expand-file-name name))
		  (file-exists-p (expand-file-name name rw-directory)))
	(when (file-exists-p (expand-file-name name rw-common-dir))
	  (replace-match (concat "common/" name) nil t nil 1)
	  (rw-add-change-log-entry)))))
  (rw-final-change-log-text "Fix common/ includes."))

(rw-rewrite #'rw-common-includes)
