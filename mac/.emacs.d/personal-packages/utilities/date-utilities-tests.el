(let* ((parent-directory (file-name-directory load-file-name))
	(file-name "date-utilities"))
  (load (file-name-concat parent-directory file-name)))

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
