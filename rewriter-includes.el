;; Sort includes.

(defconst rw-include-comments
  '(("0" . "Standard C includes")
    ("1" . "Standard C++ includes")
    ("2" . "Local non-gdb includes")
    ("3" . "Local subdirectory includes")
    ("4" . "Local includes")))

(defconst rw-include-regexp
  (concat "/\\* \\("
	  (mapconcat #'cdr rw-include-comments "\\|")
	  "\\).  \\*/$"))

(defun rw-looking-at-h-comment ()
  "True if looking at a standard header comment."
  (looking-at rw-include-regexp))

(defun rw-skip-intro-comment ()
  ;; The intro comment.
  (forward-comment 1)
  (while (progn
	   (skip-chars-forward " \t\r\n")
	   (and (looking-at "/\\*")
		(not (rw-looking-at-h-comment))))
    (forward-comment 1)))

(defun rw-skip-intro (filename is-header)
  ;; Skip all available.
  (rw-skip-intro-comment)
  (when is-header
    (if (looking-at "#if\\(ndef\\| !\\s-*defined\\)")
	(forward-line)
      (message "%s does not have #ifndef !?" filename))
    (if (looking-at "#define")
	(forward-line)
      (message "%s does not have #define !?" filename))
    (skip-chars-forward " \t\r\n")))

(defun rw-include-key (style name)
  (if (string= style "<")
      (if (string-suffix-p ".h" name)
	  ;; Put C includes before C++ includes.
	  "0"
	;; C++ includes
	"1")
    (cond
     ;; This eventually and trickily arranges for non-gdb headers to
     ;; be included earlier.
     ((not (member (expand-file-name name rw-directory) (rw-files)))
      ;; Must be between " and <.
      "2")
     ;; Sort subdirectories into a single stanza.
     ((save-match-data (string-match "/" name))
      "3")
     (t "4"))))

(defun rw-include-comment (style)
  (or (cdr (assoc style rw-include-comments))
      (error "Missing style %s" style)))

(defun rw-scan-condition ()
  (let ((kind nil)
	(start (point))
	(ok t)
	(first-name nil))
    ;; Looking at the #if.
    (forward-line)
    (while (looking-at "#include \\([\"<]\\)\\([^\">]*\\)[\">]")
      (let ((style (match-string 1))
	    (name (match-string 2)))
	(setq style (rw-include-key style name))
	(unless first-name
	  (setq first-name name))
	(if kind
	    (unless (equal kind style)
	      (message "Inconsistent conditional include at %s"
		       (line-number-at-pos))
	      (setq ok nil))
	  (setq kind style)))
      (forward-line))
    (if (and ok (looking-at "#endif"))
	(progn
	  (forward-line)
	  (list first-name kind (buffer-substring start (1- (point)))))
      (goto-char start)
      nil)))

(defun rw-collect-includes ()
  (let ((result '())
	(last-include (point))
	(keep-going t))
    (while keep-going
      (setq keep-going nil)
      (skip-chars-forward " \t\r\n")
      ;; Skip the standard comments.
      (when (rw-looking-at-h-comment)
	(forward-comment 1)
	(skip-chars-forward " \t\r\n"))
      (cond
       ((looking-at "#include \\([\"<]\\)\\([^\">]*\\)[\">]")
	(let ((style (match-string 1))
	      (name (match-string 2)))
	  (setq style (rw-include-key style name))
	  (push (list name style (match-string 0)) result))
	(forward-line)
	(setq last-include (point))
	(setq keep-going t))
       ((looking-at "#if.*HAVE_[A-Z0-9_]*")
	(let ((cond-include (rw-scan-condition)))
	  (when cond-include
	    (push cond-include result)
	    (setq last-include (point))
	    (setq keep-going t))))))
    (goto-char last-include)
    result))

(defun rw-delete-include (include-list include)
  (cl-remove-if
   (lambda (item)
     (string= (car item) include))
   include-list))

(defun rw-insert-new-includes (is-header filename include-list)
  (let ((result nil))
    (unless is-header
      (let ((main-header
	     (cond
	      ((string-match "/\\(arch\\|common\\|nat\\|target\\)/" filename)
	       "common/common-defs.h")
	      ((string-match "/gdbserver/" filename)
	       "server.h")
	      (t "defs.h"))))
	(push (concat "#include \"" main-header "\"\n") result)
	;; Delete both forms to preserve idempotency.
	(setq include-list (rw-delete-include include-list
					      (file-name-nondirectory main-header)))
	(setq include-list (rw-delete-include include-list main-header))
	(let ((header (concat (file-name-sans-extension filename) ".h")))
	  (when (and (member (expand-file-name header rw-directory) (rw-files))
		     ;; Ugh.
		     (not (string-match "/thread-iter\\.h$" header)))
	    (let* ((base-dir
		    (cond
		     ((string-match "/gdbserver/" filename)
		      (file-name-directory filename))
		     (t rw-directory)))
		   (relative-name (file-relative-name header base-dir)))
	      (push (concat "#include \"" relative-name "\"\n") result)
	      ;; Delete both forms to preserve idempotency.
	      (setq include-list
		    (rw-delete-include
		     include-list
		     (file-name-nondirectory relative-name)))
	      (setq include-list (rw-delete-include include-list
						    relative-name)))))))
    (setq include-list
	  (sort include-list
		(lambda (a b)
		  ;; <> sorts earlier than ""
		  (if (string< (cadr a) (cadr b))
		      t
		    (if (string= (cadr a) (cadr b))
			(string< (car a) (car b)))))))
    ;; Insert a newline between stanzas.
    (let ((last-bracket
	   ;; Do not need a new stanza initially in the .h case.
	   (if is-header
	       (cadr (car include-list))
	     "")))
      (dolist (item include-list)
	(let ((new-bracket (cadr item)))
	  (unless (string= last-bracket new-bracket)
	    ;; (insert "\n")
	    (push (concat "\n/* " (rw-include-comment new-bracket) ".  */\n")
		  result)
	    (setq last-bracket new-bracket))
	  (push (concat (nth 2 item) "\n") result))))
    (apply #'concat (nreverse result))))

(defun rw-rewrite-includes ()
  (c++-mode)
  (unless (or buffer-read-only
	      ;; This file is weird so we skip it.
	      (string-match "/gdbreplay\\.c$" (buffer-file-name)))
    (let ((is-header (string= (file-name-extension (buffer-file-name)) "h")))
      (rw-skip-intro (buffer-file-name) is-header)
      (let* ((start (point))
	     (include-list (rw-collect-includes)))
	(when (> (point) start)
	  (let ((end (point))
		(new-text (rw-insert-new-includes is-header
						  (buffer-file-name)
						  include-list)))
	    (unless (string= new-text
			     (buffer-substring start end))
	      (delete-region start end)
	      (insert new-text)
	      ;; Make sure there is a newline before the body of the file.
	      (unless (looking-at "\n")
		(insert "\n"))
	      (rw-add-change-log-entry))))))
    (rw-final-change-log-text "Sort headers.")))

(rw-rewrite #'rw-rewrite-includes)
