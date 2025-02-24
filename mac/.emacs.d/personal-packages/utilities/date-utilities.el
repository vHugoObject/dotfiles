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
