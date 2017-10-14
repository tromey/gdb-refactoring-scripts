(defconst dir-to-scan (pop argv))
(unless (file-directory-p dir-to-scan)
  (error "Usage: emacs --script rewrite-add-info.el DIR"))

(defvar all-info-functions nil)

(defconst cxxrewrite-dirs '("." "cli" "mi" "python" "tui" "guile" "nat" "common"))

(defun list-files ()
  (apply #'nconc
	 (mapcar (lambda (dir)
		   (directory-files (expand-file-name dir dir-to-scan)
				    t "\\.[chy]$"))
		 cxxrewrite-dirs)))

(defun scan-file-for-add-info (file)
  ;; (message "Scanning %s" file)
  (find-file file)
  (goto-char (point-min))
  (while (re-search-forward "\\s-+add_info (\".*?\", \\([a-z_]+\\)" nil t)
    ;; (message "   ... found %s" (match-string 1))
    (push (match-string 1) all-info-functions)))

(defun scan-all-files (files)
  (mapc #'scan-file-for-add-info files))

(defun rewrite-one-file (file rx)
  (find-file file)
  (unless buffer-read-only
    (goto-char (point-min))
    (while (re-search-forward rx nil t)
      (message "  Found %s" (match-string 1))
      (goto-char (match-end 0))
      (insert "const ")
      (save-excursion
	(add-change-log-entry))
      (search-forward ",")
      (when (save-excursion
	      (end-of-line)
	      (> (current-column) 79))
	(insert "\n")
	(indent-for-tab-command)))))

(defun rewrite-files (files)
  (let* ((rx-list (sort (delete-dups all-info-functions) #'string<))
	 (rx (concat "^\\(" (regexp-opt rx-list) "\\)\\s-*(")))
    (message "RX = %S" rx)
    (dolist (file files)
      (rewrite-one-file file rx))))

(let ((all-files (list-files)))
  (message "Scanning...")
  (scan-all-files all-files)
  (message "Rewriting...")
  (rewrite-files all-files)
  (save-some-buffers t))
