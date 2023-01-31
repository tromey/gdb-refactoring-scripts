;; Rewrite F(x, ...) -> x->f(...)
;; Usage: fn-to-method [-z] FUNC METHOD [N]
;; Rewrite FUNC to METHOD
;; If -z is given, FUNC is a regexp and METHOD can contain
;; substitutions.  FIXME can't use -r due to some emacs bug
;; If N is given, it is the argument number of the 'this' argument.
;; Defaults to 0; arguments start at 0.  Incompatible with -z.

(defvar rw-is-rx nil)
(when (equal (car argv) "-z")
  (setq rw-is-rx t)
  (pop argv))

(defvar rw-original-rx
  (if rw-is-rx
      (pop argv)
    (regexp-quote (pop argv))))

(defvar rw-function
  ;; Allow an optional "::" before the function name.
  (concat "\\(::\\)?\\_<\\("
	  rw-original-rx
	  "\\)\\_>[ \t\n]*(\\s *"))
(defvar rw-new-name (pop argv))
(defvar rw-arg (if (and (stringp (car argv))
			(string-match "^[0-9]+$ " (car argv)))
		   (string-to-number (pop argv))
		 0))

(defun rw-remove-comma ()
  (when (looking-at ",")
    (let ((start (point)))
      (forward-char)
      (skip-chars-forward " \t\n")
      (delete-region start (point)))))

(defun rw-rewrite-fn-name (text)
  (if rw-is-rx
      (progn
	(cl-assert (string-match rw-original-rx text))
	(replace-match rw-new-name t nil text))
    rw-new-name))

(defun rw-fn-to-method ()
  (while (re-search-forward rw-function nil t)
    (goto-char (match-end 2))
    (let ((starting-text (buffer-substring (match-beginning 2)
					   (match-end 2))))
      ;; Delete the original function name.
      (delete-region (match-beginning 2) (match-end 2))
      ;; If there was a "::", delete it too.
      (when (match-beginning 1)
	(delete-region (match-beginning 1) (match-end 1)))
      ;; Now rewrite the function name if needed.
      (let ((new-name (rw-rewrite-fn-name starting-text))
	    (saved-text nil))
	(save-excursion
	  ;; Move forward past the "(".
	  (skip-chars-forward " \t\n")
	  (when (looking-at "(")
	    (forward-char))
	  (skip-chars-forward " \t\n")
	  (let ((remove-comma t))
	    (save-excursion
	      ;; Skip leading arguments.
	      (dotimes (_ignore rw-arg)
		(when (looking-at ",")
		  (setq remove-comma nil)
		  (forward-char)
		  (skip-chars-forward " \t\n"))
		(rw-skip-expr "[,)]"))
	      ;; Could be at the start of the arguments, or at the ","
	      ;; before the argument we want to extract.  Delete the "," if
	      ;; it is there.
	      (rw-remove-comma)
	      ;; Extract the argument.
	      (let ((start (point)))
		(rw-skip-expr "[,)]")
		(setq saved-text (buffer-substring start (point)))
		(delete-region start (point)))
	      ;; If we need the ",", re-insert it.
	      (when (and remove-comma (looking-at ","))
		(rw-remove-comma)))))
      ;; Insert the argument again, this time as 'this'.
      (if (string-match "^&" saved-text)
	  ;; Turn f(&x) -> x.m()
	  (insert (substring saved-text 1) "." new-name)
	;; Turn f(*x) -> (*x)->m()
	(if (string-match "^\\*" saved-text)
	    (insert "(" saved-text ")->" new-name)
	  ;; Turn f(x) -> x->m().
	  (insert saved-text "->" new-name)))
      (let ((start (point)))
	(forward-list)
	(indent-region start (point)))))))

(rw-rewrite #'rw-fn-to-method)
