(let* ((parent-directory (file-name-parent-directory (file-name-directory load-file-name)))
		       (folder "utilities")
		       (files (list "data-structure-utilities" "date-utilities"))
		       (filenames (mapcar (lambda (file)
				 (file-name-concat parent-directory folder file)
				 )
			files))
		       )
	(mapc (lambda (filename)
		(load filename))
	filenames))

(defun org-table-to-alist (table)
    (map-apply (lambda (key value)
	     (let (
		   (val (car value))
		   )
		 (when (stringp val)
		   (setq val (string-to-number val))
		   )
		 (cons key val)
		 )
	     )
	     table)
  )

 (defun org-table-to-hash-table (table &optional key-function)
 (let (
       (alist (org-table-to-alist table))
     )

  (alist-to-hash-table alist key-function))
 )

(defun org-table-totals-for-date-range (table &optional date-range)
  "Create a table where the first column is a day and second column is the sum for that day"
  (let* (
      (format-string-name (or date-range "day"))
      (key-function (get-timestamp-format-function format-string-name))
	)
    (hash-table-to-list-of-lists (org-table-to-hash-table table key-function))
    )
  )

(defun org-table-average-for-date-range (table &optional date-range)
  (let* (
       (format-string-name (or date-range "day"))
       (key-function (get-timestamp-format-function format-string-name))
       (table-name (format "Average per %s" format-string-name))
	 )
    (list (list table-name (seq-average (org-table-to-hash-table table key-function))))
     )
   )

(defun org-link-creator (file)
    (format "** [[file:%s][%s]]\n" file file))

(defun directory-to-table-of-contents (directory file-extension)
  (let*(
	(file-regex (concat "\\" file-extension "$"))
	(files (directory-files directory nil file-regex))
	(file-list (sort files #'string<))
	(table-header "* Table des matières\n")
	(file-links (mapconcat #'org-link-creator  file-list))
	(table-of-contents (mapconcat #'identity (list table-header file-links)))
	)
      table-of-contents)
  )
