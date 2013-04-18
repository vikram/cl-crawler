(in-package :com.crawler)

;;;
;;; time & date
;;;

(defun short-date-time ()
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time (get-universal-time) 0)
    (format nil "~A~A~A~A~A~A" ye mo da ho mi se)))

(defun now ()
  (get-universal-time))

(defparameter +day-names+
  ;; this next last
  (make-array 7 :initial-contents '("MONDAY" "TUESDAY" "WEDNESDAY" "THURSDAY"
                                     "FRIDAY" "SATURDAY" "SUNDAY")))

(defun days-from-today-date (date)
  (days-from-today-time (string->dttm date)))

(defun days-from-today-time (time)
  (let ((start-of-day (string->dttm (dttm->string (now) :format :date))))
    (multiple-value-bind (day extra) (floor (/ (- time start-of-day) 24 3600))
      day)))

(defun stylized->day (time)
  (let ((start-of-day (string->dttm (dttm->string (now) :format :date))))
    (multiple-value-bind (day extra) (floor (/ (- time start-of-day) 24 3600))
      (multiple-value-bind (se mi ho da mo ye dw dst tz) (decode-universal-time start-of-day 0)
         (multiple-value-bind (se1 mi1 ho1 da1 mo1 ye1 dw1 dst1 tz1) (decode-universal-time time 0)
           (cond ((= day 0) "TODAY")
                 ((= day -1) "YESTERDAY")
                 ((= day 1) "TOMORROW")
                 ((and (>= day -7) (< day 0)) (concatenate 'string "LAST " (aref +day-names+ dw1)))
                 ((and (>= 6 (+ dw day)) (> day 0)) (concatenate 'string "THIS " (aref +day-names+ dw1)))
                 ((and (>= 13 (+ dw day)) (> day 0))  (concatenate 'string "NEXT " (aref +day-names+ dw1)))
                 (t (dttm->string time :format :caldate))))))))

(defparameter +month-names+
  (make-array 12 :initial-contents '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
                                     "Sep" "Oct" "Nov" "Dec")))

(defparameter +week-days+
  (make-array 7 :initial-contents '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))

(defparameter +time-zones+
  '((5 "EDT" . "EST") (6 "CDT" . "CST") (7 "MDT" . "MST") (8 "PDT" . "PST")
    (0 "BST" . "GMT") (-2 "MET DST" . "MET")))

(defun tz->string (tz dst &optional (long t))
  "Convert the CL timezone (rational [-24;24], multiple of 3600) to a string."
  (declare (type rational tz))
  (multiple-value-bind (hr mm) (floor (abs (- (if dst 1 0) tz)))
    (let ((mi (floor (* 60 mm)))
          (zo (assoc tz +time-zones+)))
      (format nil "~:[+~;-~]~2,'0d~:[:~;~]~2,'0d~@[ (~a)~]"
              (minusp tz) hr long mi
              (and long (if dst (cadr zo) (cddr zo)))))))

(defun string->tz (obj)
  "Find the OBJ (symbol or string) in +TIME-ZONES+."
  (find obj +time-zones+ :test
        (lambda (st el) (or (string-equal st (cadr el))
                            (string-equal st (cddr el))))))

(defun current-time (&optional (out t))
  "Print the current time to the stream (defaults to t)."
  (multiple-value-bind (se mi ho da mo ye dw dst tz) (get-decoded-time)
    (declare (fixnum se mi ho da mo ye dw) (type rational tz))
    (format out "~4d-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d ~a"
            ye mo da (aref +week-days+ dw) ho mi se
            (tz->string tz dst))))

(defparameter +day-sec+ (* 24 60 60) "The number of seconds per day.")

(deftype days-t () '(signed-byte 20))

(defstruct (date)
  "The date structure -- year, month, and day."
  (ye 1 :type days-t)
  (mo 1 :type (integer 1 12))
  (da 1 :type (integer 1 31))
  (dd nil :type (or null days-t))) ; days since the epoch (1900-1-1 == 0)

(defmethod print-object ((dt date) (out stream))
  (if *print-readably* (call-next-method)
      (format out "~4,'0d-~2,'0d-~2,'0d" (date-ye dt)
              (date-mo dt) (date-da dt))))

(defun date-mon-name (dt)
  "Return the name of the month."
  (declare (type date dt))
  (aref +month-names+ (1- (date-mo dt))))

(defun date-mon-offset (dt)
  "Return the number of characters printed for the previous months."
  (declare (type date dt))
  (let ((pos (1- (date-mo dt))))
    (reduce #'+ +month-names+ :key #'length :end pos :initial-value pos)))

(defun print-date-month (dt &optional (str t))
  "Print the date to the STREAM, month and year only."
  (declare (type date dt))
  (format str "~a ~d" (date-mon-name dt) (date-ye dt)))

(defun date2num (dt)
  "Convert the date to the numerical format YYYYMMDD."
  (declare (type date dt))
  (+ (* 10000 (date-ye dt)) (* 100 (date-mo dt)) (date-da dt)))

(defun date2time (dt)
  "Call `encode-universal-time' on the date.
Returns the number of seconds since the epoch (1900-01-01)."
  (declare (type date dt))
  (encode-universal-time 0 0 0 (date-da dt) (date-mo dt) (date-ye dt) 0))

(defun time2date (num)
  "Convert the universal time (GMT) to date."
  (declare (real num))
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time num 0)
    (declare (ignore se mi ho))
    (make-date :ye ye :mo mo :da da :dd (floor num +day-sec+))))

(defun days2date (days)
  "Convert the number of days since the epoch (1900-01-01) to the date."
  (declare (type days-t days))
  (time2date (* days +day-sec+)))

(defun today ()
  "Return today's date."
  (time2date (get-universal-time)))

(defun days-between (d0 &optional (d1 (today)))
  "Return the number of days between the two dates."
  (declare (type date d0 d1))
  (- (date-dd d1) (date-dd d0)))

(defun tomorrow (&optional (dd (today)) (skip 1))
  "Return the next day in a new date structure.
With the optional second argument (defaults to 1) skip as many days.
I.e., (tomorrow (today) -1) is yesterday."
  (declare (type date dd) (type days-t skip))
  (days2date (+ (date-dd dd) skip)))

(defun yesterday (&optional (dd (today)) (skip 1))
  "Return the previous day.  Calls tomorrow."
  (declare (type date dd) (type days-t skip))
  (tomorrow dd (- skip)))

(defun weekday (&key (time (get-universal-time)))
  (multiple-value-bind (se mi ho da mo ye dd dst1 tz1)
      (decode-universal-time time 0)
    (declare (ignore se mi ho da mo ye dst1 tz1))
    dd))

(defgeneric date-formatter (format se mi ho da mo ye dd dst tz)
  (:documentation "Format the decoded time using the given format spec.
The supported specs are:
  function -- run it
  string   -- assume it's a format string for the other arguments
  :date    -- \"2001-04-22\", see <http://www.w3.org/TR/NOTE-datetime>
  :datetime -- \"2001-04-22T04:53:44\", ditto
  :short   -- \"2001-04-22 Sun 04:53:44\"
  :long    -- \"2001-04-22 Sun 04:53:44 +0000 (GMT)\"
  :mbox    -- \"Sun Apr 22 04:53:44 2001\"
  :usa     -- \"Sun, 22 Apr 2001 04:53:44 +0000 (GMT)\"")
  (:method ((format function) se mi ho da mo ye dd dst tz)
    (funcall format se mi ho da mo ye dd dst tz))
  (:method ((format string) se mi ho da mo ye dd dst tz)
    (format nil format se mi ho da mo ye dd dst tz))
  (:method ((format t) se mi ho da mo ye dd dst tz)
    (error "~s: unknown format: ~s [date: ~s]" 'date-formatter format
           (date-formatter :short se mi ho da mo ye dd dst tz)))
  (:method ((format (eql :date)) se mi ho da mo ye dd dst tz)
    (declare (ignore se mi ho dd dst tz))
    (format nil "~d-~2,'0d-~2,'0d" ye mo da))
  (:method ((format (eql :prettytime)) se mi ho da mo ye dd dst tz)
    (declare (ignore se mi ho dd dst tz))
    (format nil "~A.~2,'0d ~A" (if (> ho 12) (- ho 12) ho) mi (if (> ho 12) "pm" "am")))
  (:method ((format (eql :mtime)) se mi ho da mo ye dd dst tz)
    (declare (ignore se mi ho dd dst tz))
    (parse-integer (format nil "~2,'0d~2,'0d" ho mi)))
  (:method ((format (eql :time)) se mi ho da mo ye dd dst tz)
    (declare (ignore se mi ho dd dst tz))
    (format nil "~2,'0d:~2,'0d" ho mi))
  (:method ((format (eql :yymmdd)) se mi ho da mo ye dd dst tz)
    (declare (ignore se mi ho dd dst tz))
    (format nil "~d~2,'0d~2,'0d" ye mo da))
  (:method ((format (eql :shortyymmdd)) se mi ho da mo ye dd dst tz)
    (declare (ignore se mi ho dd dst tz))
    (format nil "~d~2,'0d~2,'0d" (rem ye 100) mo da))
  (:method ((format (eql :caldate)) se mi ho da mo ye dd dst tz)
    (declare (ignore se mi ho dd dst tz))
    (format nil "~2,'0d-~2,'0d-~d" da mo ye))
  (:method ((format (eql :datetime)) se mi ho da mo ye dd dst tz)
    (declare (ignore dd))
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d~@[Z~a~]"
            ye mo da ho mi se (and (/= 0 tz) (tz->string tz dst nil))))
  (:method ((format (eql :ts)) se mi ho da mo ye dd dst tz)
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            ye mo da ho mi se))
  (:method ((format (eql :short)) se mi ho da mo ye dd dst tz)
    (format nil "~d-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d~@[ ~a~]"
            ye mo da (aref +week-days+ dd) ho mi se
            (and (/= 0 tz) (tz->string tz dst))))
  (:method ((format (eql :long)) se mi ho da mo ye dd dst tz)
    (format nil "~d-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d ~a"
            ye mo da (aref +week-days+ dd) ho mi se (tz->string tz dst)))
  (:method ((format (eql :mbox)) se mi ho da mo ye dd dst tz)
    (format nil "~a ~a ~d ~2,'0d:~2,'0d:~2,'0d ~d~@[ ~a~]"
            (aref +week-days+ dd) (aref +month-names+ (1- mo))
            da ho mi se ye (and (/= 0 tz) (tz->string tz dst))))
  (:method ((format (eql :pretty)) se mi ho da mo ye dd dst tz)
    (format nil "~a ~a ~d ~d"
            (aref +week-days+ dd) (aref +month-names+ (1- mo))
            da ye))
  (:method ((format (eql :day)) se mi ho da mo ye dd dst tz)
    (format nil "~a"
            (aref +week-days+ dd)))
  (:method ((format (eql :mon)) se mi ho da mo ye dd dst tz)
    (format nil "~a"
	    mo))
  (:method ((format (eql :year)) se mi ho da mo ye dd dst tz)
    (format nil "~a"
	    ye))
  (:method ((format (eql :usa)) se mi ho da mo ye dd dst tz)
    (format nil "~a, ~d ~a ~d ~2,'0d:~2,'0d:~2,'0d ~a"
            (aref +week-days+ dd) da (aref +month-names+ (1- mo))
            ye ho mi se (tz->string tz dst))))

(defun dttm->string (dttm &key (format :long) (tz 0) (dst nil dst-p))
  "Print the date/time as returned by `encode-universal-time'.
DTTM is the universal time (GMT).
FORMAT is passed to DATE-FORMATTER.
TZ is the time zone in which the time is printed, NIL means local.
DST is Daylight Saving Time indicator."
  (declare (type (integer 0) dttm))
  (multiple-value-bind (se mi ho da mo ye dd dst1 tz1)
      (decode-universal-time dttm tz)
    (date-formatter format se mi ho da mo ye dd (if dst-p dst dst1) tz1)))

(defun to-string (obj)
  "Unintern if symbol and return string."
  (declare (type (or symbol string) obj))
  (etypecase obj
    (string obj)
    (symbol (unintern obj) (symbol-name obj))))

(defun truncater (threshold)
  #'(lambda (num)
      (if (> (- num threshold) 0)
          (- num threshold)
          num)))

(setf (symbol-function 'truncate-24)
      (truncater 24))

(setf (symbol-function 'truncate-60)
      (truncater 60))

(defun string-w3-datetime (str &key (start 0) end)
  "Check whether the string is in the W3 datatime format.
Returns the decoded date or NIL.
See <http://www.w3.org/TR/NOTE-datetime>."
  (declare (type string str))
  (unless end (setq end (length str)))
  (let ((len (- end start)))
    (macrolet ((num (s e)
                 `(parse-integer str :start (+ ,s start) :end (+ ,e start)))
               (ch= (c p) `(char= ,c (aref str (+ ,p start)))))
      (when (and (>= len 10) (ch= #\- 4) (ch= #\- 7))
        (if (= len 10) ; just the date
            (values 0 0 0 (num 8 10) (num 5 7) (num 0 4))
            (when (and (>= len 16) (ch= #\T 10) (ch= #\: 13))
              (let ((z (position #\Z str :start (+ 14 start) :end end)))
                (if z
                    (values
                     (if (>= (+ start 17) end) 0 ; sec
                         (ignore-errors
                           (round (read-from-string
                                   str nil 0 :start (+ start 17) :end z))))
                     (num 14 16) (num 11 13) ; mi ho
                     (num 8 10) (num 5 7) (num 0 4) ; da mo ye
                     (ignore-errors
                       (parse-integer str :start (1+ z) :end end))) ; tz
                    (values
                     (if (>= (+ start 17) end) 0 ; sec
                         (ignore-errors
                           (round (read-from-string
                                   str nil 0 :start (+ start 17) :end end))))
                     (num 14 16) (num 11 13) ; mi ho da mo ye
                     (num 8 10) (num 5 7) (num 0 4))))))))))

(defun string-w3-dttm (str &key (start 0) end)
  (multiple-value-bind (se mi ho da mo ye tz)
      (string-w3-datetime str :start start :end end)
    (and se (encode-universal-time se mi ho da mo ye
                                   (infer-timezone (or tz 0))))))

(defvar *y2k-cut* 50)
(defun fix-y2k (ye)
  "Fix the `Y2K'-buggy year; cuts at `*y2k-cut*'.
You can redefine this function to your taste.
You can disable Y2K fixing with (setf (fdefinition 'fix-y2k) #'identity)"
  (cond ((< ye *y2k-cut*) (+ ye 2000))
        ((<= *y2k-cut* ye 99) (+ ye 1900))
        (t ye)))

(defun infer-month (mon)
  "Get the month from the object, number or name."
  (if (numberp mon) mon
      (let ((pos (position (to-string mon) +month-names+ :test
                           (lambda (s0 s1)
                             (string-equal s0 s1 :start1 0 :end1 3
                                           :start2 0 :end2 3)))))
        (when pos (1+ pos)))))

(defun infer-timezone (obj &optional minutes)
  "Guess the timezone."
  (typecase obj
    (symbol (unintern obj) (infer-timezone (symbol-name obj)))
    (string (or (car (string->tz obj)) 0))
    (number
     (multiple-value-bind (ho mi)
         (cond ((numberp minutes) (values obj minutes))
               ((< -24 obj 24) (values (- obj) 0))
               (t (floor obj 100)))
       ;; CL uses positive offsets to the West of Greenwich,
       ;; while the rest of the world counts positive to the East.
       (- (+ ho (/ mi 60)))))
    (t (error 'case-error :proc 'infer-timezone :args
              (list 'obj obj 'symbol 'string 'number)))))

(defvar +eof+ (list '+eof+))

(defun string-tokens (string &key (start 0) max)
  "Read from STRING repeatedly, starting with START, up to MAX tokens.
Return the list of objects read and the final index in STRING.
Binds `*package*' to the keyword package,
so that the bare symbols are read as keywords."
  (declare (type (or null fixnum) max) (type fixnum start))
  (let ((*package* (find-package :keyword)))
    (if max
        (do ((beg start) obj res (num 0 (1+ num)))
            ((= max num) (values (nreverse res) beg))
          (declare (fixnum beg num))
          (setf (values obj beg)
                (read-from-string string nil +eof+ :start beg))
          (if (eq obj +eof+)
              (return (values (nreverse res) beg))
              (push obj res)))
        (read-from-string (concatenate 'string "(" string ")")
                          t nil :start start))))

(defvar *string-junk* ":-,./")

(defun purge-string (str &optional (junk *string-junk*))
  (substitute-if #\Space (lambda (cc) (find cc junk :test #'char=)) str))

(defun string->dttm (xx)
  "Parse the string into a date/time integer."
  (declare (simple-string xx))
  (or (string-w3-dttm xx)
      (multiple-value-bind (v0 v1 v2 v3 v4 v5 v6 v7)
          (values-list
           (delete-if (lambda (st) ; remove week day names
                        (and (symbolp st)
                             (find (subseq (to-string st) 0
                                           (or (and (>= 3 (length (to-string st)))
                                                    (length (to-string st))) 3))
                                   +week-days+
                                   :test #'string-equal)))
                      (string-tokens
                       (purge-string (if (< (count #\- xx) 2) xx
                                         ;; kill #\- in yyyy-mm-dd
                                         (substitute #\Space #\- xx :count 2))
                                     ":,/")
                       :max 9)))
        (if (numberp v0)
            (encode-universal-time (truncate-60 (round (or v5 0))) (truncate-60 (or v4 0)) (truncate-24 (or v3 0))
                                   (min v0 v2) (infer-month v1)
                                   (fix-y2k (max v0 v2))
                                   (infer-timezone v6 v7))
            (encode-universal-time (truncate-60 (round (or v4 0))) (truncate-60 (or v3 0)) (truncate-24 (or v2 0))
                                   v1 (infer-month v0) (fix-y2k v5)
                                   (infer-timezone v6 v7))))))

(defun time-diff (end beg)
  (/ (- end beg)
     internal-time-units-per-second))

(defun subst-date->time (date time)
  (multiple-value-bind (se mi ho da mo ye dw dst tz) (decode-universal-time time 0)
    (string->dttm (format nil "~A ~A:~A:~A" date ho mi se))))

(defun-exported date-for-day (day &key (today (get-universal-time)))
  (let ((today (find-array +week-days+ (dttm->string today :format :day)))
        (this (find-array +week-days+ day :test #'(lambda (x y) (cl-ppcre:all-matches y x))))) ; to make sure Thu matches Thursday
    (dttm->string (+ (get-universal-time)
                     (* 86400 (if (>= (- this today) 0)
                                  (- this today)
                                  (+ 7 (- this today)))))
                  :format :yymmdd)))
