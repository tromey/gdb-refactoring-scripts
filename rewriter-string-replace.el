;; Rewrite a string.

(when (>= (length argv) 3)
  (error "Usage: gdb-rewriter string-replace FROM TO"))

(defconst rw-raw-from (pop argv))
(defconst rw-from (concat "\\_<" rw-raw-from "\\_>"))
(defconst rw-to (pop argv))

(defun rw-srepl-one ()
  (while (re-search-forward rw-from nil t)
    (replace-match rw-to nil t)
    (rw-add-change-log-entry))
  (rw-final-change-log-text (concat "Replace \"" rw-raw-from
				    "\" with \"" rw-to "\".")))

(rw-rewrite #'rw-srepl-one)
