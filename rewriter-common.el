;; Rewrite include x to include common/x

(defconst rw-common-dir (expand-file-name "gdbsupport" rw-base-directory))

(defun rw-common-includes ()
  (goto-char (point-min))
  ;; could maybe check for "" -vs- <> consistency.
  ;; Note this avoids "/" in the name.
  (while (re-search-forward "^#\\s-*include \"\\([^\"/]*\\)\"" nil t)
    (let ((name (match-string 1)))
      (when (file-exists-p (expand-file-name name rw-common-dir))
	(replace-match (concat "gdbsupport/" name) nil t nil 1)
	(rw-add-change-log-entry))))
  (rw-final-change-log-text "Fix gdbsupport/ includes."))

(rw-rewrite #'rw-common-includes)
