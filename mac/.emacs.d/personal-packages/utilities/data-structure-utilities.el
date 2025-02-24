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
