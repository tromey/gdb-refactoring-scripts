;;; This rewrites TRY_CATCH to "try" and sometimes an
;;; additional "catch".  It isn't quite finished.
;;; The output isn't valid, it still needs post-editing

;;; TO DO:
;;; * RETURN_MASK handling must be implemented
;;;   it should turn into a different type on the 'catch'
;;; * unconditionally emit a "catch", sometimes "catch (...) { }"

(defconst dir-to-scan (pop argv))
(unless (file-directory-p dir-to-scan)
  (error "Usage: emacs --script tc-rewrite.el DIR"))

(defconst trycatch-dirs '("." "cli" "mi" "python" "tui"))

(defun trycatch-files ()
  (apply #'nconc
	 (mapcar (lambda (dir)
		   (directory-files (expand-file-name dir dir-to-scan)
				    t "\\.c$"))
		 trycatch-dirs)))

(defconst trycatch-symbol-rx
  ;; bizarrely we can't seem to do this with [:something:].
  "\\_<[[:alnum:]_]+\\_>")

(defconst trycatch-tc-rx
  (concat "TRY_CATCH\s-*(\\s-*\\(" trycatch-symbol-rx "\\)\\s-*"
	  ",\\s-*RETURN_MASK_\\([^)]+\\))"))

(defconst trycatch-decl-rx
  "^\\s-*volatile struct gdb_exception\\s-*[^\n]*\n")

(defconst trycatch-cont-break-rx
  "\\_<\\(break\\|continue\\)\\_>")

(defun trycatch-zap-exceptions ()
  (goto-char (point-min))
  (while (re-search-forward trycatch-decl-rx nil t)
    (replace-match "")))

;; check if conforming and return end boundary if so.
(defun trycatch-conforming ()
  (skip-chars-forward " \t\n")
  (if (looking-at "{")
      (save-excursion (forward-sexp) (point))
    (message "%s:%d: non-conforming TRY_CATCH"
	     (buffer-file-name)
	     (line-number-at-pos))
    nil))

(defun trycatch-warn-cb (limit)
  (while (re-search-forward trycatch-cont-break-rx limit t)
    (message "%s:%d: %s in TRY_CATCH"
	     (buffer-file-name)
	     (line-number-at-pos)
	     (match-string 0))))

(defun trycatch-rewrite-try ()
  (goto-char (point-min))
  (while (re-search-forward trycatch-tc-rx nil t)
    (let ((symbol (match-string 1))
	  (mask (match-string 2)))
      (replace-match "try")
      (let ((end-of-body (trycatch-conforming)))
	(when end-of-body
	  (trycatch-warn-cb end-of-body)
	  (goto-char end-of-body)
	  (skip-chars-forward " \t\n")
	  (if (looking-at (concat "if (" symbol "\\.reason < 0)"))
	      (replace-match (concat "catch (const gdb_exception &"
				     symbol ")"))))))))

(defun trycatch-rewrite-one (file)
  (find-file file)
  (trycatch-zap-exceptions)
  (trycatch-rewrite-try)
  (if (buffer-modified-p)
      (save-buffer)))

(defun trycatch-rewrite ()
  (dolist (file (trycatch-files))
    (message "Processing %s" file)
    (trycatch-rewrite-one file)))

(trycatch-rewrite)
