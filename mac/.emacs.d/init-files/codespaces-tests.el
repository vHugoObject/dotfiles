(ert-deftest codespaces-setup-test ()
(let (
    (tramp-methods ())
    (tramp-completion-function-alist ())      
    (expected-ghcs-methods '((tramp-login-program "gh")
		      (tramp-login-args (("codespace") ("ssh") ("-c") ("%h")))
		      (tramp-remote-shell "/bin/sh")
		      (tramp-remote-shell-login ("-l"))
		      (tramp-remote-shell-args ("-c"))))
    (expected-tramp-completion-function-alist '((codespaces-tramp-completion "")))
    )


  (codespaces-setup)

  (should (equal (cdr (assoc "ghcs" tramp-methods)) expected-ghcs-methods))
  (should (equal (cdr (assoc "ghcs" tramp-completion-function-alist)) expected-tramp-completion-function-alist))


)
)

(defun setup-fixture (body)

  (unwind-protect
      (cl-defstruct (codespaces-space (:constructor codespaces-make-space) (:copier nil))
	"Codespace information as fetched from GitHub."
	(name nil :type string)
	(display-name nil :type string)
	(state 'unknown :type symbol)
	(repository nil :type string)
	(ref nil :type string))
    (let ((expected-struct (codespaces-make-space
		      :name "test-name"
		      :display-name  "test-displayName"
		      :state 'available
		      :repository "test/test-repository"
		      :ref "test-ref"
		      ))
	  (test-gitStatus (make-hash-table :test 'equal))	    
	  )
      (puthash "ref" "test-ref" test-gitStatus)
      (let (
	     (test-hashtable (make-hash-table :test 'equal))
	     (test-hashtable-variables (list (cons "name" "test-name")
				  (cons "displayName" "test-displayName")
				  (cons "state" "AVAILABLE")
				  (cons "repository" "test/test-repository")
				  (cons "gitStatus" test-gitStatus)
				  )
			    )
	     (test-json "[{\"displayName\": \"test-displayName\",
  \"gitStatus\": {\"ref\": \"test-ref\"},
\"name\": \"test-name\",
\"repository\": \"test/test-repository\",
\"state\": \"Available\"}]")
	 )
	 (mapcar (lambda (pair) (puthash (car pair) (cdr pair) test-hashtable))
	      test-hashtable-variables)



      (let ((test-codespaces-hashtable (make-hash-table :test 'equal))
	    )
	(puthash "test-displayName" expected-struct test-codespaces-hashtable)
	(funcall body))
      )
      )

      )
  )

(ert-deftest codespaces-space-from-hashtable-test ()
  (setup-fixture
   (lambda ()
	 (should (equal (codespaces-space-from-hashtable test-hashtable) expected-struct))
       )
     )
  )

(ert-deftest codespaces-space-readable-name-test ()
  (setup-fixture
   (lambda ()
     (should (equal (codespaces-space-readable-name expected-struct) "test-displayName")))
   )
  )

(ert-deftest codespaces-space-repository-name-test ()
  (setup-fixture
   (lambda ()
     (should (equal (codespaces-space-repository-name expected-struct) "test-repository")))
   )
  )

(ert-deftest codespaces-space-describe-test ()
  (setup-fixture
   (lambda ()
     (should (equal (codespaces-space-describe expected-struct) " | available | test/test-repository | test-ref")))
   )
  )

(ert-deftest codespaces-space-available-p-test ()
  (setup-fixture
   (lambda ()
     (should (equal (codespaces-space-available-p expected-struct) 't)))
   )
  )

(ert-deftest codespaces-space-shutdown-p-test ()
  (setup-fixture
   (lambda ()
     (let ((test-struct (codespaces-make-space
		      :name "test-name"
		      :display-name  "test-displayName"
		      :state 'shutdown
		      :repository "test-repository"
		      :ref "test-ref"
		      )))

      (should (equal (codespaces-space-shutdown-p test-struct) 't)))
     ))
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



(ert-deftest codespaces--build-table-test ()
  (setup-fixture
   (lambda ()
     (let (
	   (tables-equal (hash-table-equal (codespaces--build-table (json-parse-string test-json)) test-codespaces-hashtable))
	   )
	   (should (equal tables-equal 't)))

   )
  )
  )

(provide 'codespaces-tests)
