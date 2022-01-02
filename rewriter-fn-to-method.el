;; Rewrite F(x, ...) -> x->f(...)
;; Usage: fn-to-method FUNC METHOD [N]
;; Rewrite FUNC to METHOD
;; If N is given, it is the argument number of the 'this' argument.
;; Defaults to 0; arguments start at 0.

(defvar rw-function
  (concat "\\_<\\(" (regexp-quote (pop argv)) "\\)\\_>[ \t\n]*(\\s *"))
(defvar rw-new-name (pop argv))
(defvar rw-arg (if argv
		   (string-to-number (pop argv))
		 0))

(defun rw-remove-comma ()
  (when (looking-at ",")
    (let ((start (point)))
      (forward-char)
      (skip-chars-forward " \t\n")
      (delete-region start (point)))))

(defun rw-fn-to-method ()
  (while (re-search-forward rw-function nil t)
    (goto-char (match-end 1))
    ;; Delete the original function name.
    (delete-region (match-beginning 1) (match-end 1))
    (let ((saved-text nil))
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
      (if (string-match "^&" saved-text)
	  (insert (substring saved-text 1) "." rw-new-name)
	(insert saved-text "->" rw-new-name))
      (let ((start (point)))
	(forward-list)
	(indent-region start (point))))))

(rw-rewrite #'rw-fn-to-method)
