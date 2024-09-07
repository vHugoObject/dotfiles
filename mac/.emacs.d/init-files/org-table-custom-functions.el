(defun list-average (List)
   "Get the average of a list"
   (/ (apply '+ List) (length List))
   )
 (defun hash-table-to-alist (hash-table)
   "Convert a hash-table into a list"
   (let ((org-table))
     (maphash
      (lambda (k v)
	(push (cons k v) org-table))
      hash-table)
     (reverse org-table)))

 (defun date-from-timestamp (timestamp &optional formatter)
   "extract day from timestamp e.g. <2024-05-19 Sun> becomes 05-19-24
    ,unless another format stirng is passed as an optional argument"
   (format-time-string (or formatter "%m-%d-%Y") (date-to-time timestamp))
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

(defun alist-to-hash-table(alist &optional key-function)
 "Convert an org-table into a hash-table. Takes an optional function to format keys"
 (let ((hash-table (make-hash-table :test 'equal))
       (key-function (or key-function #'identity))
       )
   (map-do (lambda (key value)
	     (let (
		   (puthash-function (if (integerp value) #'sum-puthash #'puthash))		      
		   )
	       (apply puthash-function (list (funcall key-function key) value hash-table))
	       )
	     )
	   alist)

   hash-table)

 )

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

(defconst date-format-strings '(("time" "%H:%M") ("day" "%m-%d-%Y")
				("day-of-the-week" "%A")
				("week" "%U") ("month" "%B")
				("year" "%Y"))

  "alist of date format strings")
(defun org-table-totals-by-date-range (table &optional date-range)
  "Create a table where the first column is a day and second column is the sum for that day"
  (hash-table-to-alist (alist-to-hash-table table 'date-from-timestamp))
  )

(defun org-table-average-by-date-range (table &optional date-range)    
    (average-of-hash-table-values (alist-to-hash-table table 'date-from-timestamp))
   )
