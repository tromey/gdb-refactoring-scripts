;; Rewrite include x to include common/x

(defun rw-common-includes ()
  (goto-char (point-min))
  ;; could maybe check for "" -vs- <> consistency.
  (while (re-search-forward "^#include \"\\([^\"]*\\)\"" nil t)
    (let ((name (match-string 1)))
      (unless (member (expand-file-name name rw-directory) (rw-files))
	;; "common" here seems like a hack
	(when (member (expand-file-name name "common") (rw-files))
	  (replace-match (concat "common/" name) nil t nil 1)
	  (rw-add-change-log-entry)))))
  (rw-final-change-log-text "Fix common/ includes."))

(rw-rewrite #'rw-common-includes)
