(setopt user-emacs-directory "~/.emacs.d/"
	init-files-directory "init-files"
	custom-functions-file "org-functions")
(load (file-name-concat user-emacs-directory init-files-directory custom-functions-file))

(require 'org-table)

(ert-deftest get-date-format-string-test ()
  :tags '(:org)
  (let (
	(format-string-names (list "hour" "day" "weekday" "week" "month" "year"))
	(formatters (list "%H" "%m/%d/%Y" "%A" "%U" "%B" "%Y"))
	)
    (seq-do-indexed (lambda (format-string-name index)
		      (should (string= (get-date-format-string format-string-name) (nth index formatters)))
		      )
		    format-string-names)

      )
    )

(ert-deftest timestamp-formatter-test ()
"test timestamp-formatter"
:tags '(:org)
(let ((test-cases (list
		   (cons (list "%H" "<2024-07-04 Thu 13:31>") "13")
		   (cons (list "%m-%d-%Y" "<2024-05-19 Sun>") "05-19-2024")
		   (cons (list "%A" "<2024-06-28 Fri>") "Friday")
		   (cons (list "%U" "<2024-06-25 Tue 16:24>") "25")
		   (cons (list "%B" "<2024-07-04 Thu 13:31>") "July")
		   (cons (list "%Y" "<2024-07-04 Thu 13:31>") "2024")
	       ))
       )
   (map-do (lambda (test-case answer)
	     (should (string= (apply #'timestamp-formatter test-case) answer))
	     ) test-cases)

)
)

(ert-deftest get-timestamp-format-function-test ()
  :tags '(:org)
  (let (
	(test-cases (list
	 (cons "hour" (cons "<2024-07-04 Thu 13:31>" "13"))
	 (cons "day" (cons "<2024-05-19 Sun>" "05/19/2024"))
	 (cons "weekday" (cons "<2024-06-28 Fri>" "Friday"))
	 (cons "week" (cons "<2024-06-25 Tue 16:24>" "25"))
	 (cons "month" (cons "<2024-07-04 Thu 13:31>" "July"))
	 (cons "year" (cons "<2024-07-04 Thu 13:31>" "2024"))
	       ))
	)
    (map-do (lambda (format-string test)
	      (should (string= (funcall (get-timestamp-format-function format-string) (car test)) (cdr test)))
	      )
	    test-cases)
    )
    )

(ert-deftest time-diff-test ()
  :tags '(:org)
  (let* (
	 (test-arguments-one (list (list "<2024-05-19 Sun>" "<2024-05-21 Tue>") seconds-in-one-day)
			     )
	 (answer-one 2)
	 (test-arguments-two (list (list "<2024-05-20 Mon>" "<2024-05-28 Tue>") seconds-in-one-day)
			     )
	 (answer-two 8)
	 (test-arguments-three (list (list "<2024-05-19 Sun>" "<2024-06-19 Wed>") seconds-in-one-day)
			       )
	 (answer-three 31)

	 (test-cases (list
		      (cons test-arguments-one answer-one)
		      (cons test-arguments-two answer-two)
		      (cons test-arguments-three answer-three)
		      ))
	 )
    (map-do (lambda (test-args expected-answer)
	      (let* (
		    (test-dates (format-list-of-date-strings (car test-args)))
		    (test-divisor (car (cdr test-args)))
		    (actual-answer (time-diff test-dates test-divisor))
		    )

		(should (= actual-answer expected-answer))
		)
	      )
     test-cases)
    )
  )

(ert-deftest format-list-of-date-strings-test ()
  :tags '(:org)
  (let* (
	 (test-arguments-one (list "<2024-05-19 Sun>" "<2024-05-21 Tue>")			  
	 )
	 (answer-one '((26185 34640)
		      (26188 10832)))
	 (test-arguments-two (list "<2024-05-20 Mon>" "<2024-05-28 Tue>")			  
	 )
	 (answer-two '((26186 55504)
		       (26197 25808)))
	 (test-arguments-three (list "<2024-05-19 Sun>" "<2024-06-19 Wed>")			  
	 )
	 (answer-three '((26185 34640)
			 (26226 26064)))
	 (test-cases (list
		     (cons test-arguments-one answer-one)
		     (cons test-arguments-two answer-two)
		     (cons test-arguments-three answer-three)
		     )
	  )
	 )
    (map-do (lambda (test-dates expected-answer)
	      (let (
		     (actual-answer (format-list-of-date-strings test-dates))
		     )
		(should (equal actual-answer expected-answer))
		)
	      )
     test-cases)

      )
    )

(ert-deftest get-dates-in-range-test ()
  :tags '(:org)
  (let* (
	 (test-arguments-one (list "<2024-05-19 Sun>" "<2024-05-21 Tue>")
			     )

	 (answer-one (list "05/19/2024" "05/20/2024" "05/21/2024"))

	 (test-arguments-two (list "<2024-05-20 Mon>" "<2024-05-28 Tue>")			  
	 )

	(answer-two (list "05/20/2024" "05/21/2024" "05/22/2024"
		"05/23/2024" "05/24/2024" "05/25/2024"
		"05/26/2024" "05/27/2024" "05/28/2024"))

	(test-arguments-three (list "<2024-05-19 Sun>" "<2024-06-19 Wed>")			  
	 )

	(answer-three (list "05/19/2024" "05/20/2024" "05/21/2024"
		  "05/22/2024" "05/23/2024" "05/24/2024"
		  "05/25/2024" "05/26/2024" "05/27/2024"
		  "05/28/2024" "05/29/2024" "05/30/2024"
		  "05/31/2024" "06/01/2024" "06/02/2024"
		  "06/03/2024" "06/04/2024" "06/05/2024"
		  "06/06/2024" "06/07/2024" "06/08/2024"
		  "06/09/2024" "06/10/2024" "06/11/2024"
		  "06/12/2024" "06/13/2024" "06/14/2024"
		  "06/15/2024" "06/16/2024" "06/17/2024"
		  "06/18/2024" "06/19/2024"))

	(test-cases (list
		     (cons test-arguments-one answer-one)
		     (cons test-arguments-two answer-two)
		     (cons test-arguments-three answer-three)
		     )
	 )

	)
    (map-do (lambda (test-arguments expected-range)
	      (let* (
		    (test-dates (format-list-of-date-strings test-arguments))
		    (actual-range (funcall #'get-dates-in-range test-dates))
		    )
		(should (equal actual-range expected-range))
		)

	      )
     test-cases)
    )

    )

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

(defun create-test-org-table (test)
  (org-table-to-lisp (orgtbl-to-orgtbl test '(:hlines t)))
  )

(ert-deftest org-table-to-alist-test ()
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
	   (answer-one
	    (list
		 (cons "<2024-05-19 Sun>" 300)
		 (cons "<2024-05-19 Sun>" 1500)
		 (cons "<2024-05-20 Mon>" 900)
		 (cons "<2024-05-20 Mon>" 1500)
		 (cons "<2024-05-21 Tue>" 200)
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
	   (answer-two
	    (list
	     (cons "<2024-09-01 Sun 13:07>" 750)
	     (cons "<2024-09-02 Sun 17:00>" 300)
	     (cons "<2024-09-02 Mon 17:00>" 300)
	     (cons "<2024-09-03 Tue 17:00>" 100)
	     (cons "<2024-09-03 Tue 17:00>" 300)
	     )
			     )
	   (test-three
	    (list
			(list "09-01-2024" 1050)
		    )
	    )
	   (answer-three (list
			  (cons "09-01-2024" 1050)
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
		(let* (
		      (test-table (create-test-org-table test))
		      )
		  (should (equal (seq-difference (org-table-to-alist test-table) expected-alist) nil))
		  )
		)
	      tests)
      )
	       )

(ert-deftest org-table-to-hash-table-test ()
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
		 (list "<2024-05-19 Sun>" 300)
		 (list "<2024-05-19 Sun>" 1500)
		 (list "<2024-05-20 Mon>" 900)
		 (list "<2024-05-20 Mon>" 100)
		 (list "<2024-05-21 Tue>" 500)
		 )
		)
      (answer-one (list
		  (cons "<2024-05-19 Sun>" 1800)
		 (cons "<2024-05-20 Mon>" 1000)
		 (cons "<2024-05-21 Tue>" 500)
		 )
		)
      (test-two
	    (list
	     (list "<2024-09-01 Sun 13:07>" 750)
	     (list "<2024-09-02 Mon 17:00>" 300)
	     (list "<2024-09-02 Mon 17:00>" 300)
	     (list "<2024-09-03 Tue 17:00>" 100)
	     (list "<2024-09-03 Tue 17:00>" 300)
	     )
			     )
      (answer-two (list
		  (cons "09/01/2024" 750)
		 (cons "09/02/2024" 600)
		 (cons "09/03/2024" 400)
		 )
		)

      (format-string-name "day")
      (test-date-formatter (get-timestamp-format-function format-string-name))

      (tests (list
	      (cons (list (create-test-org-table test-one) #'identity) answer-one)
	      (cons (list (create-test-org-table test-two) test-date-formatter) answer-two)
	      )
	     )
      )


(map-do (lambda (test-arguments expected-values)
	  (let* (
		(actual-hash-table (apply #'org-table-to-hash-table test-arguments))
		)
	    (test-runner actual-hash-table expected-values)
	    )
	  )	 
	tests)
)
  )
  )

(ert-deftest org-table-totals-for-date-range-test ()
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
	   (answer-one
	    (list
		 (list "05/19/2024" 1800)
		 (list "05/20/2024" 2400)
		 (list "05/21/2024" 200)
			  )
	    )

	   (test-two
	    (list
	     (list "<2024-09-01 Sun 13:07>" 750)
	     (list "<2024-09-02 Mon 17:00>" 300)
	     (list "<2024-09-02 Mon 17:05>" 300)
	     (list "<2024-09-03 Tue 17:00>" 100)
	     (list "<2024-09-03 Tue 17:00>" 300)
	     )
			     )
	   (answer-two
	    (list
	     (list "09/01/2024" 750)
	     (list "09/02/2024" 600)
	     (list "09/03/2024"  400)
	     )
			     )
	   (test-three
	    (list
			(list "<2024-09-01 Sun 13:07>" 1050)
		    )
	    )
	   (answer-three (list
			  (list "09/01/2024" 1050)
			      )
			     )

	   (tests (list
		   (cons (list (create-test-org-table test-one) "day") answer-one)
		   (cons (list (create-test-org-table test-two) "day") answer-two)
		   (cons (list (create-test-org-table test-three) "day") answer-three)
		   )
		  )
	   )

      (map-do (lambda (test-arguments expected-table)
		(let* (
		      (actual-table (apply #'org-table-totals-for-date-range test-arguments))
		      )
		  (should (equal actual-table expected-table))
		  )
		)
	      tests)
	       )
    )

(ert-deftest org-table-average-for-date-range-test ()
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
			(list "Average per day" 1466)
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
			(list "Average per day" 583)
			      )
			     )

	   (test-three
	    (list
			(list "09-01-2024" 1500)
		    )
	    )
	   (answer-three (list
			  (list "Average per day" 1500)
			      )
			     )

	   (tests (list
		   (cons (list (create-test-org-table test-one) "day") answer-one)
		   (cons (list (create-test-org-table test-two) "day") answer-two)
		   (cons (list (create-test-org-table test-three) "day") answer-three)
		   )

		  )
	   )

	       (map-do (lambda (test-arguments expected-table)
			   (let* (
				 (actual-table (apply #'org-table-average-for-date-range test-arguments))
				 )
			   (should (equal actual-table expected-table))
			   )
			   )
		       tests)

	     )
	       )

(ert-deftest org-link-creator-test ()
  :tags '(:org)
  (let* (
	 (file-one "file-one.org")
	 (answer-one "** [[file:file-one.org][file-one.org]]\n")
	 (test-two "test-two.org")
	 (answer-two "** [[file:test-two.org][test-two.org]]\n")
	 (tests (list
		 (cons file-one answer-one)
		 (cons test-two answer-two)
		 )
		)
	 )
    (map-do (lambda (test expected-link)
	      (let (
		    (actual-link (org-link-creator test))
		    )
		(should (string= actual-link expected-link))
		)
	      )
     tests)
    )
  )

(ert-deftest directory-to-table-of-contents-test ()
  :tags '(:org)
  (cl-flet* (
	    (make-temp-test-files (list-of-test-files)
	      (mapcar (lambda (test-file)
			(when (file-exists-p test-file) (delete-file test-file))
			(make-empty-file test-file)
			(should (equal (file-exists-p test-file) 't))
			)
	       list-of-test-files)
	      )

	    (delete-temp-test-files (list-of-test-files)
	      (mapcar (lambda (test-file)
			(when (file-exists-p test-file) (delete-file test-file))
			(should (equal (file-exists-p test-file) nil))
			)
	       list-of-test-files)
	      )

	    (full-test-file-addresses (list-of-test-files)
	      (mapcar (lambda (test-file)
			(file-name-concat temporary-file-directory test-file)
			)
	       list-of-test-files)		
	      )

	    (create-expected-list (expected-entries)
	      (mapconcat #'identity expected-entries)
	      )


	    (test-runner (test-case expected-list)
	      (funcall #'make-temp-test-files (car test-case))
	      (let* (
		    (file-extension (car (cdr test-case)))
		    (test-arguments (list temporary-file-directory file-extension))
		    (actual-list (apply #'directory-to-table-of-contents test-arguments))
		    )
		(unwind-protect
		    (should (string= actual-list expected-list))
		  (funcall #'delete-temp-test-files (car test-case))
		    )

		)


	      )
	    )
    (let* (	  
	(test-files-one (full-test-file-addresses (list "file-one.org" "file-two.org" "file-three.el")))
	(test-extension-one ".org")
	(test-answer-one (create-expected-list (list "* Table des matières\n" "** [[file:file-one.org][file-one.org]]\n" "** [[file:file-two.org][file-two.org]]\n")))

	(test-files-two (full-test-file-addresses (list "file-4.el" "file-5.el" "file-6.og")))
	(test-extension-two ".el")
	(test-answer-two (create-expected-list (list "* Table des matières\n" "** [[file:file-4.el][file-4.el]]\n" "** [[file:file-5.el][file-5.el]]\n")))

	(tests (list
		(cons (list test-files-one test-extension-one) test-answer-one)
		(cons (list test-files-two test-extension-two) test-answer-two)
		)
	 )
	)
      (map-do #'test-runner tests)
    )
    )

  )

(provide 'org-table-custom-functions-tests)
