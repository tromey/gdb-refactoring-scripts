;; Rewrite duplicate and commented-out includes.

(defun rw-include-ok (file)
  "Return non-nil if FILE can be included more than once."
  (string-match "\\(opcode/\\|\\.def$\\)" file))

(defun rw-duplicate-includes ()
  (while (re-search-forward "^/\\*\\s-*#include.*\\*/$" nil t)
    (delete-region (match-beginning 0) (1+ (match-end 0)))
    (rw-add-change-log-entry))
  (let ((table (make-hash-table :test 'equal)))
    (goto-char (point-min))
    ;; could maybe check for "" -vs- <> consistency.
    (while (re-search-forward "^#include [\"<]\\([^\">]*\\)[\">]" nil t)
      (let ((name (match-string 1)))
	  (when (and (gethash name table)
		     (not (rw-include-ok name)))
	    (message "Deleting #include {{%s}}" name)
	    (delete-region (match-beginning 0) (1+ (match-end 0)))
	    (rw-add-change-log-entry))
	  (puthash name t table))))
  (rw-final-change-log-text "Fix includes."))

(rw-rewrite #'rw-duplicate-includes)
