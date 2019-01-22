;; Rewrite include x to include common/x

(defconst rw-common-dir (expand-file-name "common" rw-directory))

(defun rw-common-includes ()
  (goto-char (point-min))
  ;; could maybe check for "" -vs- <> consistency.
  (while (re-search-forward "^#include \"\\([^\"]*\\)\"" nil t)
    (let ((name (match-string 1)))
      (unless (member (expand-file-name name rw-directory) (rw-files))
	(when (member (expand-file-name name rw-common-dir) (rw-files))
	  (replace-match (concat "common/" name) nil t nil 1)
	  (rw-add-change-log-entry)))))
  (rw-final-change-log-text "Fix common/ includes."))

(rw-rewrite #'rw-common-includes)
