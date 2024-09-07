(setopt user-emacs-directory "~/.emacs.d/"
	init-files-directory "init-files"
	custom-functions-file "org-table-custom-functions")
(load (file-name-concat user-emacs-directory init-files-directory custom-functions-file))

(require 'org-table)

(ert-deftest list-average-test ()
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

(ert-deftest hash-table-to-alist-test ()      
"test hash-table-to-list"
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


      )

      (map-do #'create-test-case test-cases)

 (map-do (lambda (test-case answer)
	     (should (equal (hash-table-to-alist test-case) answer))
	     )
   test-cases)
 )
      )

)

(ert-deftest date-from-timestamp-test ()
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

	) (map-do (lambda (answer test-case)
	      (should (= test-case answer)))
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
		 (cons 1 "a")
		 (cons 2 "b")
		 (cons 3 "c")
		 )
		)
      (answer-two (list
		 (cons 1 "a")
		 (cons 2 "b")
		 (cons 3 "c")
		 )
		)
      (tests (list
	      (cons test-one answer-one)
	      (cons test-two answer-two)
	      )
	     )
      )


(map-do (lambda (test expected-values)
	  (let (
		(actual-hash-table (alist-to-hash-table test))
		)
	    (test-runner actual-hash-table expected-values)
	    )
	  )	 
	tests)
)
  )
  )

(ert-deftest average-of-hash-table-values-test ()
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
	       (should (= (average-of-hash-table-values table) average))
	       ) tests)

    )
  )
  )

(ert-deftest org-table-totals-by-date-range-test ()
    :tags '(:org)	 	
    (let* (
	   (test-one
	    (list
		 (list "<2024-05-19 Sun>" 300)
		 (list "<2024-05-19 Sun>" 1500)
		 (list "<2024-05-20 Mon>" 900)
		 (list "<2024-05-20 Mon>" 1500)
		 (list "<2024-05-21 Tue>" 200)
			  )
	    )
	   (answer-one (list
			(cons "05-19-2024" 1800)
			(cons "05-20-2024" 2400)
			(cons "05-21-2024" 500)
			)
		       )

	   (test-two
	    (list
	     (list "<2024-09-01 Sun 13:07>" 750)
	     (list "<2024-09-02 Sun 17:00>" 300)
	     (list "<2024-09-02 Mon 17:00>" 300)
	     (list "<2024-09-03 Tue 17:00>" 100)
	     (list "<2024-09-03 Tue 17:00>" 300)
	     )
			     )
	   (answer-two (list
			(cons "09-01-2024" 1050)
			(cons "09-02-2024" 300)
			(cons "09-03-2024" 400)
			      )
			     )

	   (test-three
	    (list
			(cons "09-01-2024" 1050)
		    )
	    )
	   (answer-three (list
			  (cons "09-05-2024" 1500)
			      )
			     )
		   (tests (list
			   (cons test-one answer-one)
			   (cons test-two answer-two)
			   (cons test-three answer-three)
			   )
			  )
		   )

	       (map-do (lambda (test expected-alist)
			   (let (
				 (test-table (org-table-to-lisp test))
				 )
			   (should (equal (org-table-totals-by-date-range test-table) expected-alist))
			   )
			   )
		       tests)

	     )
	       )

(ert-deftest org-table-average-by-date-range-test ()  
      (let* (
	       (test-one
   "| <2024-05-19 Sun>       |      300 |
   |------------------------+----------|
   | <2024-05-19 Sun>       |     1500 |
   |------------------------+----------|
   | <2024-05-20 Mon>       |      900 |
   |------------------------+----------|
   | <2024-05-20 Mon>       |     1500 |
   |------------------------+----------|
   | <2024-05-21 Tue>       |     2000 |")

	       (answer-one (list
			   (cons "05-19-2024" 1800)
			  (cons "05-20-2024" 1000)
			  (cons "05-21-2024" 500)
			  )
			 )

	       (test-two
"| <2024-09-01 Sun 13:07> |      750 |
 | <2024-09-01 Sun 17:00> |      300 |
 | <2024-09-02 Mon 20:56> |      840 |
 | <2024-09-02 Mon 21:0>  |      100 |
 | <2024-09-03 Tue 10:33> |     1000 |"
			 )
	       (answer-two (list
			   (cons "09-01-2024" 2000)
			  (cons "09-02-2024" 1600)
			  (cons "09-03-2024" 250)
			  )
			 )		 
	       (test-three
		"| <2024-09-05 Thu 19:28> |     1500 |"
		)
	       (answer-three (list
			   (cons "09-05-2024" 1500)
			  )
			 )
	       (tests (list
		       (cons test-one answer-one)
		       (cons test-two answer-two)
		       (cons test-three answer-three)
		       )
		      )
	       )

	)
      (map-do (lambda (test expected-alist)		       
		       (should (equal (org-table-average-by-date-range range test) expected-alist))
		     )
		   tests)
      )

(provide 'org-table-custom-functions-tests)
