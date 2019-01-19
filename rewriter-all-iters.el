;; Rewrite "FUNC (ARG)" to "ARG->METHOD ()"
;; If METHOD is not given, FUNC is reused.

(when (>= (length argv) 3)
  (error "Usage: gdb-rewriter all-iters FUNC [METHOD]"))

(defconst rw-iter-name (pop argv))
(defconst rw-iter-rx (concat "\\_<" rw-iter-name " (\\([^)]*\\))"))
(defconst rw-iter-new-name (if argv (pop argv) rw-iter-name))
(defconst rw-iter-replace (concat "\\1->" rw-iter-new-name " ()"))

(defun rw-iter-one ()
  (while (re-search-forward rw-iter-rx nil t)
    (replace-match rw-iter-replace)
    (rw-add-change-log-entry)))

(rw-rewrite #'rw-iter-one)
