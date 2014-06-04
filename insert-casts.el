;;; Insert casts for -Wc++-compat.
;;; Run a build with -Wc++-compat and direct the output to a file
;;; named "Log" in the build/gdb directory.
;;; Note you need LANG=C and -ftrack-macro-expansion=0 -Wc++-compat
;;; Then run this script.  It will rewrite the sources to insert
;;; some needed casts.
;;; It doesn't handle everything, just a subset.  And since gcc's
;;; error message locations aren't always precise, it sometimes
;;; introduces errors.
;;; This script converts about 90% of the needed spots.


;; DIR is the build directory of gdb.
(defconst dir-to-rebuild (pop argv))
(unless (file-directory-p dir-to-rebuild)
  (error "Usage: emacs --script insert-casts.el DIR"))
(setq default-directory dir-to-rebuild)

;; Ensure gcc emits easy-to-parse quotes.
(setenv "LANG" "C")

(defun insert-casts-clean ()
  (message "make mostlyclean")
  (call-process "make" nil nil nil "mostlyclean"))

(defun insert-casts-rewrite-one (filename line col type-name)
  ;; Don't bother generated files.
  (if (not (equal (file-name-directory filename) "./"))
      (save-excursion
	(find-file filename)
	;; ... or generated files in the source tree.
	(unless buffer-read-only
	  (message "Rewriting %s:%d:%d" filename line col)
	  (goto-char (point-min))
	  (forward-line (1- line))
	  (forward-char (1- col))
	  ;; Some errors are on the '=', some on the RHS.
	  (skip-chars-forward " \t\n=")
	  (cond
	   ((looking-back "[[:space:]\n]=[[:space:]\n]+")
	    (insert "(" type-name ") "))
	   ((looking-back "\\_<return[[:space:]]+")
	    (insert "(" type-name ") "))
	   ((looking-at "\\_<return[[:space:]]+")
	    (forward-word)
	    (skip-chars-forward " \t\n")
	    (insert "(" type-name ") ")))))))

(defconst insert-casts-rx
  "^\\([^\n:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error: request for implicit conversion from '[^']+' to '\\([^']+\\)'")

;; rebuild with something like and put output in Log
;; (call-process "make" nil (current-buffer) nil
;; 		    "-k" "CFLAGS=-ftrack-macro-expansion=0 -Wc++-compat")

(defun insert-casts-rewrite ()
  (find-file "Log")
  (goto-char (point-min))
  (while (re-search-forward insert-casts-rx nil t)
    (let ((filename (match-string 1))
	  (line (string-to-int (match-string 2)))
	  (col (string-to-int (match-string 3)))
	  (type-name (match-string 4)))
      ;; (message "Examining %s:%d:%d" filename line col)
      (insert-casts-rewrite-one filename line col type-name))))

(defun insert-casts ()
  ;; (insert-casts-clean)
  (insert-casts-rewrite)
  (dolist (buf (buffer-list))
    (set-buffer buf)
    (and (buffer-file-name)
	 (buffer-modified-p)
	 (save-buffer))))

(insert-casts)

