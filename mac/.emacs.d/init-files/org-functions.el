(defconst date-format-strings-map (list (cons "hour" "%H")
					(cons "day" "%m/%d/%Y")
					(cons "weekday" "%A")
					(cons "week" "%U")
					(cons "month" "%B")
					(cons "year" "%Y"))

      "alist of date format strings")

(defconst one-day
  (date-to-time "1970-01-01 18:00")
  "a single day, to be used with time-add and time-subtract")

(defconst seconds-in-one-day
  86400
  "total seconds in one day")

(cl-defun time-diff ((date-one date-two) &optional divisor)
 (/ (time-subtract date-two date-one) (or divisor seconds-in-one-day))
 )


(defun format-list-of-date-strings (list-of-string-dates)
  (mapcar (lambda (string-date) (date-to-time string-date)) list-of-string-dates)
    )  

(defun get-date-format-string (format-string-name)
      (let (
	  (format-string-names (map-keys date-format-strings-map))
	  )
      (unless (member format-string-name format-string-names)
	 (user-error "Please pass a valid date format-string-name")
	)

      (alist-get format-string-name date-format-strings-map  nil nil 'equal))    
      )

  (defun timestamp-formatter (formatter timestamp)
    (format-time-string formatter (date-to-time timestamp))
    )

(defun get-timestamp-format-function (format-string-name)
    (apply-partially #'timestamp-formatter (get-date-format-string format-string-name))
    )


(cl-defun get-dates-in-range ((start end) &optional format-string-name)
  (let* (
	 (number-of-dates-to-add (time-diff (list start end) seconds-in-one-day))
	 (format-string-name (or format-string-name "day"))
	 (date-format-string (get-date-format-string format-string-name))
	 (date-range '())
	 (range '())
	 )
	 (dotimes (_ number-of-dates-to-add) (push one-day date-range))
	 (seq-reduce (lambda (acc elt)
		       (let* (
			      (new (time-add acc elt))
			      (date (format-time-string date-format-string new))
			     )
			(push date range) 
			 new)
		       )
	  date-range start)
	 (push (format-time-string date-format-string start) range)
      (sort range #'string<))
    )

(defun hash-table-to-list-of-lists (hash-table)
   "Convert a hash-table into a list"
   (let ((org-table))
     (maphash
      (lambda (k v)
	(push (list k v) org-table))
      hash-table)
     (reverse org-table)))

(defun alist-to-hash-table(alist &optional key-function)
   (let ((hash-table (make-hash-table :test 'equal))
	 (key-function (or key-function #'identity))
	 )
     (map-do (lambda (key value)		   
		 (apply #'my-puthash (list (funcall key-function key) value hash-table))		 
	       )
	     alist)
     hash-table))

   (defun hash-table-equal (hash-table1 hash-table2)
   (let ((hash :3tFc-NwZR2Y))
   (catch 'TAG
     (when (not (eq (hash-table-count hash-table1) (hash-table-count hash-table2))) (throw 'TAG nil))
     (maphash
      (lambda (key value)
	(when (equal hash (gethash key hash-table2 hash)) (throw 'TAG nil))
	(when (not (equal value (gethash key hash-table2))) (throw 'TAG nil)))
      hash-table1)
     t))
   )

(cl-defgeneric my-puthash (key value hash-table)
   "Add values when a key already exists in hash table instead of overwriting"     
     (puthash key value hash-table)
   )

(cl-defmethod my-puthash (key (value number) hash-table)
  (let ((value-to-add (+ value (gethash key hash-table 0))))
     (puthash key value-to-add hash-table))
   )


(cl-defgeneric seq-average (seq)
  "Get the average of the values in a alist, plist or hashtable"     
   (let* (
	  (values (map-values seq))
	  (entries (length values))	      
	  (total (apply '+ values))
	 )
     (/ total entries))
   )

(cl-defmethod seq-average ((seq list))
  "Get the average of the values in a list"
   (/ (apply '+ seq) (length seq))
   )

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
