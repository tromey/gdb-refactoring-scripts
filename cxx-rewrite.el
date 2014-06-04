;;; This rewrites C++ keywords in the gdb source.


(defconst dir-to-scan (pop argv))
(unless (file-directory-p dir-to-scan)
  (error "Usage: emacs --script cxx-rewrite.el DIR"))

(defconst cxxrewrite-dirs '("." "cli" "mi" "python" "tui"))

(defun cxxrewrite-files ()
  (apply #'nconc
	 (mapcar (lambda (dir)
		   (directory-files (expand-file-name dir dir-to-scan)
				    t "\\.[chy]$"))
		 cxxrewrite-dirs)))

(defconst cxxrewrite-keywords
  '(("bool" . "boolean")
    ("catch" . "catcher")
    ("class" . "theclass")
    ("const_cast" . nil)
    ("delete" . "to_delete")
    ("dynamic_cast" . nil)
    ("explicit" . "is_explicit")
    ("export" . "to_export")
    ("false" . nil)
    ("friend" . nil)
    ("mutable" . "mutable_obj")
    ("namespace" . "the_namespace")
    ("new" . "newobj")
    ("noexcept" . nil)
    ("nullptr" . nil)
    ("operator" . "oper")
    ("private" . "priv")
    ("protected" . nil)
    ("public" . "is_public")
    ("reinterpret_cast" . nil)
    ("static_cast" . nil)
    ("template" . "templ")
    ("this" . "self")
    ("throw" . nil)
    ("true" . nil)
    ("try" . "attempt")
    ("typename" . "type_name")
    ("typeid" . "type_id")
    ("using" . "using_decl")
    ("virtual" . nil)))

(defconst cxxrewrite-keywords-rx
  (regexp-opt (mapcar #'car cxxrewrite-keywords) 'symbols))

(defun cxxrewrite-symbols ()
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (while (re-search-forward cxxrewrite-keywords-rx nil t)
      (unless (nth 8 (syntax-ppss))
	(let ((new-val (cdr (assoc (match-string 0) cxxrewrite-keywords))))
	  (if new-val
	      (replace-match new-val t t)
	    (message "%s:%d: could not replace %s"
		     (buffer-file-name)
		     (line-number-at-pos)
		     (match-string 0))))))))

(defun cxxrewrite-rewrite-one (file)
  (find-file file)
  (unless buffer-read-only
    (cxxrewrite-symbols)
    (if (buffer-modified-p)
	(save-buffer))))

(defun cxxrewrite-rewrite ()
  (dolist (file (cxxrewrite-files))
    (message "Processing %s" file)
    (cxxrewrite-rewrite-one file)))

(cxxrewrite-rewrite)
