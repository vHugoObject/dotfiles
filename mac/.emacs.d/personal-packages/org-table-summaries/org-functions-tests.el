(require 'org-table)

(let* ((file-directory (file-name-directory (or buffer-file-name load-file-name)))
	(file-name "org-functions")

  (load (file-name-concat file-directory file-name))))

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
