(defun list-average (List)
    "Get the average of a list"
    (/ (apply '+ List) (length List))
    )
  (defun hash-table-to-list (hash-table)
    "Convert a hash-table into a list"
    (let ((new-list nil))
      (maphash
       (lambda (k v)
	 (push (list k v) new-list))
       hash-table)
      (reverse new-list)))
  (defun average-of-hash-table-values (hash-table)
    "Get the average of the values in a hash-table"
    (let ((total 0)
	  (entries (hash-table-count hash-table))
	  )
      (maphash
       (lambda (_ v)
	 (setq total (+ v total)))
       hash-table)
      (/ total entries)))

  (defun date-from-timestamp (timestamp &optional formatter)
    "extract day from timestamp e.g. <2024-05-19 Sun> becomes 06-28-24
     ,unless another format stirng is passed as an optional argument"
    (format-time-string (if formatter formatter "%m-%d-%Y") (date-to-time timestamp))
    )
  (defun sum-puthash (key value hash-table)
    "Add values when a key already exists in hash table instead of overwriting"
    (let ((value-to-add (+ value (gethash key hash-table 0))))
      (puthash key value-to-add hash-table))
    )

  (defun hash-table-equal (hash-table1 hash-table2)
    (let ((hash :3tFcNwZR2Y))
    (catch 'TAG
      (when (not (eq (hash-table-count hash-table1) (hash-table-count hash-table2))) (throw 'TAG nil))
      (maphash
       (lambda (key value)
	 (when (equal hash (gethash key hash-table2 hash)) (throw 'TAG nil))
	 (when (not (equal value (gethash key hash-table2))) (throw 'TAG nil)))
       hash-table1)
      t))
    )

(defun average-of-hash-table (table)
    "Average per day of table "
    (average-of-hash-table-values (org-table-to-hash-table table 'date-from-timestamp))
   )

(defun org-table-to-hash-table(orgtable &optional keyfunction)
    "Convert an org-table into a hash-table. Takes an optional function to format keys"
    (let ((hash-table (make-hash-table :test 'equal)))
      (dolist (element table)
	(let* ((key (if keyfunction (funcall keyfunction (nth 0 element)) (nth 0 element)))
	       (value (nth 1 element)))
	  (sum-puthash key value hash-table)))
      hash-table)
    )



  (defun by-day-table (table)
    "Create a table where the first column is a day and second column is the sum for that day"
    (hash-table-to-list (org-table-to-hash-table table 'date-from-timestamp))
    )

(org-table-to-hash-table (list (cons 2 1) (cons 3 4)))
