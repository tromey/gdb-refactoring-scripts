;; Sort includes.

;; FIXME rewrite to common/
;; FIXME insert header guards

(defun rw-skip-intro (filename is-header)
  ;; Skip all available.
  (forward-comment 9999)
  (when is-header
    (if (looking-at "#if\\(ndef\\| !\\s-*defined\\)")
	(forward-line)
      (message "%s does not have #ifndef !?" filename))
    (if (looking-at "#define")
	(forward-line)
      (message "%s does not have #define !?" filename))
    (skip-chars-forward " \t\r\n")))

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
      (cond
       ((looking-at "#include \\([\"<]\\)\\([^\">]*\\)[\">]")
	(let ((style (match-string 1))
	      (name (match-string 2)))
	  ;; This eventually and trickily arranges for non-gdb headers
	  ;; to be included earlier.
	  (when (and (string= style "\"")
		     (not (member (expand-file-name name rw-directory)
				  (rw-files))))
	    ;; Must be between " and <.
	    (setq style "."))
	  (push (list name style (match-string 0)) result))
	(forward-line)
	(setq last-include (point))
	(setq keep-going t))
       ((looking-at "#if.*HAVE_[A-Z0-9_]*_H")
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
  (unless is-header
    (let ((main-header
	   (cond
	    ((string-match "/\\(arch\\|common\\|nat\\|target\\)/" filename)
	     "common-defs.h")
	    ((string-match "/gdbserver/" filename)
	     "server.h")
	    (t "defs.h"))))
      (insert "#include \"" main-header "\"\n")
      (setq include-list (rw-delete-include include-list main-header))
      (let ((header (concat (file-name-sans-extension filename) ".h")))
	(when (and (member (expand-file-name header rw-directory) (rw-files))
		   ;; Ugh.
		   (not (string-match "/stabsread\\.h$" header))
		   (not (string-match "/thread-iter\\.h$" header)))
	  (let ((base-dir
		 (cond
		  ((string-match "/gdbserver/" filename)
		   (file-name-directory filename))
		  (t rw-directory))))
	    (insert "#include \"" (file-relative-name header base-dir) "\"\n"))
	  (setq include-list (rw-delete-include
			      include-list
			      (file-relative-name header rw-directory)))))))
  (setq include-list
	(sort include-list
	      (lambda (a b)
		;; <> sorts earlier than ""
		(if (string> (cadr a) (cadr b))
		    t
		  (if (string= (cadr a) (cadr b))
		      (string< (car a) (car b)))))))
  ;; Insert a newline between base and <> and "" stanzas.
  (let ((last-bracket
	 ;; Do not need a new stanza initially in the .h case.
	 (if is-header
	     (cadr (car include-list))
	   "")))
    (dolist (item include-list)
      (let ((new-bracket (cadr item)))
	(unless (string= last-bracket new-bracket)
	  (insert "\n")
	  (setq last-bracket new-bracket))
	(insert (nth 2 item) "\n")))))

(defun rw-rewrite-includes ()
  (unless (or buffer-read-only
	      ;; This file is weird so we skip it.
	      (string-match "/gdbreplay\\.c$" (buffer-file-name)))
    (let ((is-header (string= (file-name-extension (buffer-file-name)) "h")))
      (rw-skip-intro (buffer-file-name) is-header)
      (let* ((start (point))
	     (include-list (rw-collect-includes)))
	(when (> (point) start)
	  (delete-region start (point))
	  (rw-add-change-log-entry)
	  (rw-insert-new-includes is-header (buffer-file-name) include-list))))
    (rw-final-change-log-text "Sort headers.")))

(rw-rewrite #'rw-rewrite-includes)
