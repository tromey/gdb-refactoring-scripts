;; Rewrite F(x, ...) -> x->f(...)

(defvar rw-function
  (concat "\\_<" (regexp-quote (pop argv)) "\\_>\\s *(\\s *"))
(defvar rw-new-name (pop argv))

(defun rw-fn-to-method ()
  (while (re-search-forward rw-function nil t)
    (delete-region (match-beginning 0) (match-end 0))
    (rw-skip-expr ",")
    ;; Delete the ",".
    (let ((start (point)))
      (forward-char)
      (skip-chars-forward " \t\n")
      (delete-region start (point)))
    (insert "->" rw-new-name " (")
    (backward-char)
    (let ((start (point)))
      (forward-list)
      (indent-region start (point)))))

(rw-rewrite #'rw-fn-to-method)
