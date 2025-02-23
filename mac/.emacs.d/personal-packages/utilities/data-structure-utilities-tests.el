(let* ((parent-directory (file-name-directory load-file-name))
	  (file-name "data-structure-utilities"))
    (load (file-name-concat parent-directory file-name)))

(ert-deftest seq-average-hash-table-test ()
  "test average-of-hash-table-values"
  :tags '(:org)

  (cl-flet (
	   (create-test-case (table alist)
	     (map-do (lambda (key value)
		       (puthash key value table))
		     alist)
	     )
	   )

  (let* (
       (table-1 (make-hash-table :test 'equal))
       (table-values-1 (list (cons 1 110)
			   (cons 2 1000)
			   (cons 3 900)
			   (cons 4 3000)
			   (cons 5 3000)
			   ))

       (average-1 1602)

       (table-2 (make-hash-table :test 'equal))
       (table-values-2 (list (cons 'a 150000)
			   (cons 'b 300000)
			   (cons 'c 250000)
			   ))
       (average-2 233333)

       (table-3 (make-hash-table :test 'equal))
       (table-values-3 (list (cons 13 2)
			   (cons 14 7)
			   (cons 15 9)
			   ))
       (average-3 6)

       (table-4 (make-hash-table :test 'equal))
       (table-values-4 (list (cons 'x 15)
			   ))
       (average-4 15)
       (test-cases (list
		    (cons table-1 table-values-1)
		    (cons table-2 table-values-2)
		    (cons table-3 table-values-3)
		    (cons table-4 table-values-4)
		    ))
       (tests (list
		    (cons table-1 average-1)
		    (cons table-2 average-2)
		    (cons table-3 average-3)
		    (cons table-4 average-4)
		    ))
       )

    (map-do #'create-test-case test-cases)      
    (map-do (lambda (table average)
	       (should (= (seq-average table) average))
	       ) tests)

    )
  )
  )

(ert-deftest seq-average-lists-test ()
  :tags '(:org)
  (let* (
	(test-one (list 2 1))
	(answer-one 1)
	(test-two (list 9 8 7 9))
	(answer-two 8)
	(test-three (list 110 1000 900 3000 30))
	(answer-three 1008)
	(test-cases (list
		    (cons test-one answer-one)
		    (cons test-two answer-two)
		    (cons test-three answer-three)
		    )
	 )
	)

    (map-do (lambda (test-case answer)
	      (should (= (seq-average test-case) answer))
	      ) test-cases)

 ))

(ert-deftest hash-table-to-list-of-lists-test ()      
"test hash-table-to-list-of-lists"
:tags '(:org)
(cl-flet (
	   (create-test-case (table alist)
	     (map-do (lambda (key value)
		       (puthash key value table))
		     alist)
	     )
	   )

(let* (
       (table-1 (make-hash-table :test 'equal))
      (alist-1 (list (cons 1 2)
		    (cons 3 4)
		    )
	      )

      (answer-1 (list (list 1 2)
		    (list 3 4)
		    )
	      )

     (table-2 (make-hash-table :test 'equal))
     (alist-2 (list (cons 'a 'b)
		    (cons 'c 'd)
		    )
	      )
     (answer-2 (list (list 'a 'b)
		    (list 'c 'd)
		    )
	      )

     (table-3 (make-hash-table :test 'equal))
     (alist-3 (list (cons 'a 1)
		    (cons 'c 2)
		    )
	      )
     (answer-3 (list (list 'a 1)
		    (list 'c 2)
		    )
	      )

     (table-4 (make-hash-table :test 'equal))
     (alist-4 (list (cons 1 'a)
		    (cons 2 'c)
		    )
	      )
     (answer-4 (list (list 1 'a)
		    (list 2 'c)
		    )
	      )

      (test-cases (list
		  (cons table-1 alist-1)
		  (cons table-2 alist-2)
		  (cons table-3 alist-3)
		  (cons table-4 alist-4)
		  ))

      (tests (list
		  (cons table-1 answer-1)
		  (cons table-2 answer-2)
		  (cons table-3 answer-3)
		  (cons table-4 answer-4)
		  ))

      )

      (map-do #'create-test-case test-cases)

 (map-do (lambda (test-case answer)
	     (should (equal (hash-table-to-list-of-lists test-case) answer))
	     )
   tests)
 )
      )

)

(ert-deftest my-puthash-test ()
  "test my-puthash"
  :tags '(:org)

  (cl-flet (
	    (create-test-case (alist table)
	      (map-do (lambda (key value)
			(my-puthash key value table))
		      alist)
	      )
	    )
    (let* (
	  (table-1 (make-hash-table :test 'equal))
	  (alist-1 (list (cons 1 2)
		     (cons 3 4)
		     )
	       )
	  (expected-value-1 2)

	  (table-2 (make-hash-table :test 'equal))
	  (alist-2 (list (cons 'a 1)
		     (cons 'a 3)
		     )
	       )
	  (expected-value-2 4)

	  (table-3 (make-hash-table :test 'equal))
	  (alist-3 (list (cons 'x 10)
		     (cons 'x 1000)
		     (cons 'x 20)
		     (cons 'y 200)
		     )
	       )
	  (expected-value-3 1030)

	  (table-4 (make-hash-table :test 'equal))
	  (alist-4 (list (cons 'z "a")
			 (cons 'z "b")
			 (cons 'z "c")
			 (cons 'z "d")
			 (cons 'aa "e")
			 (cons 'aa "f")
		     )
	       )
	  (expected-value-4 "d")

	  (table-5 (make-hash-table :test 'equal))
	  (alist-5 (list (cons 'b "-9")
			 (cons 'b "-9")
			 (cons 'b "-9")
			 (cons 'b "-9")
			 (cons 'b "-9")
			 (cons 'b "45")
			 )
		   )

	  (expected-value-5 "45")
	  (test-values (list (cons alist-1 table-1)
			    (cons alist-2 table-2)
			    (cons alist-3 table-3)
			    (cons alist-4 table-4)
			    (cons alist-5 table-5)
			    ))
       )


  (map-do #'create-test-case test-values)


  (let* (
	(actual-value-1 (gethash 1 table-1))
	(actual-value-2 (gethash 'a table-2))
	(actual-value-3 (gethash 'x table-3))
	(actual-value-4 (gethash 'z table-4))    
	(actual-value-5 (gethash 'b table-5))

	(test-cases (list
		    (cons actual-value-1 expected-value-1)
		    (cons actual-value-2 expected-value-2)
		    (cons actual-value-3 expected-value-3)
		    (cons actual-value-4 expected-value-4)
		    (cons actual-value-5 expected-value-5)		      
		    ))

	) (map-do (lambda (answer test-case)
		    (when (integerp answer)
			(should (= test-case answer))

		     )
		    (when (stringp answer)
			(should (string= test-case answer))

		     )
	      )
    test-cases))


  )
    )

 )

(ert-deftest hash-table-equal-test ()
  :tags '(:org)
      (let* (
	   (test-hash-table1 (make-hash-table :test 'equal))
	   (test-hash-table2 (make-hash-table :test 'equal))
	   (test-hash-table3 (make-hash-table :test 'equal))
	   (test-hash-table4 (make-hash-table :test 'equal))
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
	   (test-cases (list
			 (cons test-hash-table-variables1 test-hash-table1)
			 (cons test-hash-table-variables2 test-hash-table2)
			 (cons test-hash-table-variables3 test-hash-table3)			     
			 )
			)			 
	   (tests
	    (list (cons (cons test-hash-table1 test-hash-table1) t)
		       (cons (cons test-hash-table1 test-hash-table2) nil)
		       (cons (cons test-hash-table2 test-hash-table3) nil)
		       ))
	   )


    (cl-flet* (
	      (create-test-case (alist table)
		(map-do (lambda (key value)
		       (puthash key value table))
			alist)
		)	
	      )
      (map-do #'create-test-case test-cases)	
      (map-do (lambda (key value)
		(should (equal (hash-table-equal (car key) (cdr key)) value))
		)
	      tests)
      )

  )
      )

(ert-deftest alist-to-hash-table-test ()
  :tags '(:org)
  (cl-flet (
	    (test-runner (actual-hash-table expected-values)
	      (map-apply (lambda (expected-key expected-value)
			   (should (equal (gethash expected-key actual-hash-table)
					  expected-value))
			   )
			 expected-values)
	      )
	    (test-date-formatter (timestamp)
	      (apply #'timestamp-formatter '(timestamp ""))
	      )
	    )

(let* (
      (test-one (list
		 (cons "<2024-05-19 Sun>" 300)
		 (cons "<2024-05-19 Sun>" 1500)
		 (cons "<2024-05-20 Mon>" 900)
		 (cons "<2024-05-20 Mon>" 100)
		 (cons "<2024-05-21 Tue>" 500)
		 )
		)
      (answer-one (list
		  (cons "<2024-05-19 Sun>" 1800)
		 (cons "<2024-05-20 Mon>" 1000)
		 (cons "<2024-05-21 Tue>" 500)
		 )
		)

      (test-two (list
		 (cons "<2024-05-19 Sun>" "yes")
		 (cons "<2024-05-19 Sun>" "no")
		 (cons "<2024-05-19 Mon>" "no")
		 (cons "<2024-05-20 Mon>" "yes")
		 (cons "<2024-05-21 Tue>" "no")
		 )
		)
      (answer-two (list
		  (cons "<2024-05-19 Sun>" "no")
		 (cons "<2024-05-20 Mon>" "yes")
		 (cons "<2024-05-21 Tue>" "no")
		 )
		)
      (tests (list
	      (cons (list test-one #'identity) answer-one)
	      (cons (list test-two #'identity) answer-two)
	      )
	     )
      )


(map-do (lambda (test expected-values)
	  (let (
		(actual-hash-table (apply #'alist-to-hash-table test))
		)
	    (test-runner actual-hash-table expected-values)
	    )
	  )	 
	tests)
)
  )
  )
