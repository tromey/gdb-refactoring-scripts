;; Usage: rename-function FROM TO
;; Will re-indent the parameters as well.

(defconst rw-from (concat "\\_<" (pop argv) "\\_>"))
(defconst rw-to (pop argv))

(rw-rewrite
 (lambda ()
   (unless buffer-read-only
     (while (re-search-forward rw-from nil t)
       (replace-match rw-to nil t)
       (skip-chars-forward " \t")
       (when (looking-at "(")
	 (save-excursion
	   (let ((start (point)))
	     (forward-list)
	     (indent-region start (point)))))))))
