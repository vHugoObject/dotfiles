(load "init-files/org-table-custom-functions")

(ert-deftest org-list-average-test ()
  "test list-average function"
  :tags '(:org)
  (let ((test-cases '(((2 1) . 1)
		((9 8 7 9) . 8)
		((110 1000 900 3000 30) . 1008)
		))
	)

    (map-do (lambda (test-case answer)
	      (should (= (list-average test-case) answer))
	      ) test-cases)

 ))

(ert-deftest org-hash-table-to-list-test ()      
 "test hash-table-to-list"
 :tags '(:org)
 (let ((table-1 (make-hash-table :test 'equal))
      (table-2 (make-hash-table :test 'equal))
      (table-3 (make-hash-table :test 'equal))
      (table-4 (make-hash-table :test 'equal))
      (list-1 (list(list 1 2)(list 3 4)))
      (list-2 (list(list 'a 'b)(list 'c 'd)))
      (list-3 (list(list 'a 1)(list 'c 2)))
      (list-4 (list(list 1 'a)(list 2 'c)))
      )

   (puthash 1 2 table-1)
   (puthash 3 4 table-1)
   (puthash 'a 'b table-2)
   (puthash 'c 'd table-2)
   (puthash 'a 1 table-3)
   (puthash 'c 2 table-3)
   (puthash 1 'a table-4)
   (puthash 2 'c table-4)

 (let (
       (test-cases (list
		   (cons table-1 list-1)
		   (cons table-2 list-2)
		   (cons table-3 list-3)
		   (cons table-4 list-4)
		   ))

       ) (map-do (lambda (test-case answer)
	     (should (equal (hash-table-to-list test-case) answer))
	     )
   test-cases))


 )
)

(ert-deftest org-average-of-hash-table-values-test ()
  "test average-of-hash-table-values"
  :tags '(:org)
  (let ((table-holder (make-hash-table :test 'equal))
       (table-1 (make-hash-table :test 'equal))
       (table-2 (make-hash-table :test 'equal))
       (table-3 (make-hash-table :test 'equal))
       (table-4 (make-hash-table :test 'equal))

       (average-1 1602)
       (average-2 233333)
       (average-3 6)
       (average-4 15)	 
       )
    (puthash 1 110 table-1)
    (puthash 2 1000 table-1)
    (puthash 3 900 table-1)
    (puthash 4 3000 table-1)
    (puthash 5 3000 table-1)

    (puthash 'a 150000 table-2)
    (puthash 'b 300000 table-2)
    (puthash 'c 200000 table-2)
    (puthash 'c 250000 table-2)

    (puthash 13 2 table-3)
    (puthash 14 7 table-3)
    (puthash 15 9 table-3)

    (puthash 'x 12 table-4)
    (puthash 'y 18 table-4)

    (puthash average-1 table-1 table-holder)
    (puthash average-2 table-2 table-holder)
    (puthash average-3 table-3 table-holder)
    (puthash average-4 table-4 table-holder)

    (maphash (lambda (k v)
	       (should (= (average-of-hash-table-values v) k))
	       ) table-holder)

    )
  )

(ert-deftest org-date-from-timestamp-test ()
"test date-from-timestamp"
:tags '(:org)
(let ((test-cases '(("<2024-05-19 Sun>" . "05-19-2024")
	       ("<2024-06-28 Sun>" . "06-28-2024")
	       ("<2024-06-25 Tue 16:24>" . "06-25-2024")
	       ("<2024-07-04 Thu 13:31>" . "07-04-2024")		 
	       ))
       )
   (map-do (lambda (test-case answer)
	     (should (equal (date-from-timestamp test-case) answer))
	     ) test-cases)

)
)

(ert-deftest sum-puthash-test ()
  "test sum-puthash"
  :tags '(:org)

  (cl-flet (
	    (create-test-case (alist table)
	      (map-do (lambda (key value)
			(sum-puthash key value table))
		      alist)
	      )
	    )
    (let* (
	  (table-1 (make-hash-table :test 'equal))
	  (alist-1 (list (cons 1 2)
		     (cons 3 4)
		     )
	       )
	  (expected-sum-1 2)

	  (table-2 (make-hash-table :test 'equal))
	  (alist-2 (list (cons 'a 1)
		     (cons 'a 3)
		     )
	       )
	  (expected-sum-2 4)

	  (table-3 (make-hash-table :test 'equal))
	  (alist-3 (list (cons 'x 10)
		     (cons 'x 1000)
		     (cons 'x 20)
		     (cons 'y 200)
		     )
	       )
	  (expected-sum-3 1030)

	  (table-4 (make-hash-table :test 'equal))
	  (alist-4 (list (cons 'z 1.5)
			 (cons 'z 1.5)
			 (cons 'z 3)
			 (cons 'z 9)
			 (cons 'aa 9)
			 (cons 'aa 9)
		     )
	       )
	  (expected-sum-4 15)

	  (table-5 (make-hash-table :test 'equal))
	  (alist-5 (list (cons 'b -9)
			 (cons 'b -9)
			 (cons 'b -9)
			 (cons 'b -9)
			 (cons 'b -9)
			 (cons 'b 45)
			 )
		   )

	  (expected-sum-5 0)
	  (test-sums (list (cons alist-1 table-1)
			    (cons alist-2 table-2)
			    (cons alist-3 table-3)
			    (cons alist-4 table-4)
			    (cons alist-5 table-5)
			    ))
       )


  (map-do #'create-test-case test-sums)


  (let* (
	(actual-sum-1 (gethash 1 table-1))
	(actual-sum-2 (gethash 'a table-2))
	(actual-sum-3 (gethash 'x table-3))
	(actual-sum-4 (gethash 'z table-4))    
	(actual-sum-5 (gethash 'b table-5))

	(test-cases (list
		    (cons actual-sum-1 expected-sum-1)
		    (cons actual-sum-2 expected-sum-2)
		    (cons actual-sum-3 expected-sum-3)
		    (cons actual-sum-4 expected-sum-4)
		    (cons actual-sum-5 expected-sum-5)		      
		    ))

	) (map-do (lambda (test-case answer)
	      (should (= test-case answer)))
    test-cases))


  )
    )

 )

(ert-deftest org-hash-table-equal ()
  :tags '(:org)
      (let* (
	   (test-hash-table1 (make-hash-table :test 'equal))
	   (test-hash-table2 (make-hash-table :test 'equal))
	   (test-hash-table3 (make-hash-table :test 'equal))
	   (test-hash-table4 (make-hash-table :test 'equal))
	   (same-hash-tables (list test-hash-table1 test-hash-table2))
	   (test-hash-table-variables1 (list (cons "name" "test-name")
				    (cons "displayName" "test-displayName")
				    (cons "state" "AVAILABLE")
				    (cons "repository" "test/test-repository")
				    ))
	   (test-hash-table-variables2 (list (cons "name" "test-name")
				    (cons "displayName" "test-displayName")
				    (cons "state" "AVAILABLE")
				    ))
	   (test-hash-table-variables3 (list (cons "name" "test-name")
				    (cons "displayName" "test-displayName")
				    (cons "state" "AVAILABLE")
				    (cons "not" "the-same")
				    ))
	   (test-cases(list (cons (cons test-hash-table1 test-hash-table2) t)
			    (cons (cons test-hash-table1 test-hash-table3) nil)
			    (cons (cons test-hash-table2 test-hash-table4) nil)
				    ))
	   )


    (cl-flet (
	      (map-alist-hash-table (alist hash-table)
		(map-do (lambda (key value) (puthash key value hash-table))
			 alist)
		)
	      )
      (mapc (lambda (hash-table) (map-alist-hash-table test-hash-table-variables1 hash-table)) same-hash-tables)

      (map-do (lambda (key value) (puthash key value test-hash-table3))
	      test-hash-table-variables2)

      (map-do (lambda (key value) (puthash key value test-hash-table4))
	      test-hash-table-variables3)

      (map-do (lambda (key value)
		(should (equal (hash-table-equal (car key) (cdr key)) value))
		)
	      test-cases)
      )

  )
      )

(provide 'org-table-custom-functions-tests)
