(in-package :com.crawler)

(defparameter *whitespace-chars* 
  '( #\tab #\space #\newline #\return #\linefeed #\page #\tab))

(defparameter *punctuation-chars* 
  '(#\& #\^ #\+ #\' #\_ #\- #\/ #\\ #\* #\% #\, #\. #\) #\( #\; #\: #\? #\! #\@ ));#\Start-Guarded-Area #\Private-Use-Two))

(defparameter *funny-chars*
  (append *punctuation-chars* *whitespace-chars*))

(defun whitespacep (char)
  (find char *whitespace-chars*))

(defmacro replace-expander (lst str)
  (reduce #'(lambda (x y) 
	      `(cl-ppcre:regex-replace-all ,(car y) ,x ,(cdr y)))
	  `,lst
	  :initial-value str))

(defun trim-string (char-list string)
  (replace-expander (((cl-ppcre:create-scanner
		       (list :sequence
			     :START-ANCHOR 
			     (list :GREEDY-REPETITION 1 NIL `(:char-class ,@char-list))))
		      . "")
		     ((cl-ppcre:create-scanner
		       (list :sequence
			     (list :greedy-repetition 1 nil `(:char-class ,@char-list))
			     :end-anchor)) 
		      . ""))
		    string))

(defun funnies (punctuation)
  (cl-ppcre:create-scanner 
   (list :greedy-repetition 1 nil 
	 (append `(:char-class ,@*whitespace-chars*)
		 (if punctuation
		     *punctuation-chars*
		     nil)))))

(defparameter *funny-punc-scanner* (funnies t))
(defparameter *funny-scanner* (funnies nil))

(defmacro defscan (name string)
  (with-gensyms (str)
    `(progn
       (defparameter ,name
	 (cl-ppcre:create-scanner ,string))
       (defun ,(make-logic-symbol (subseq (symbol-name name) 1 (1- (length (symbol-name name))))) (,str)
	 (cl-ppcre:all-matches-as-strings ,name ,str)))))

(defscan *alpha-p* "[a-zA-Z]")
(defscan *numeric-p* "[0-9]")

(defscan *punctuation*
    (list :GREEDY-REPETITION 1 NIL `(:char-class ,@*punctuation-chars*)))

(defscan *whitespace-scanner*
    (list :sequence
	  (list :GREEDY-REPETITION 1 NIL `(:char-class ,@*whitespace-chars*))))

(defscan *word-punc-scanner*
    (list :alternation
	  (list :GREEDY-REPETITION 1 NIL `(:char-class ,@*punctuation-chars*))
	  (list :greedy-repetition 1 nil `(:char-class (:RANGE #\a #\z)
						       (:RANGE #\A #\Z)
						       (:RANGE #\0 #\9)))))

(defscan *html-scanner*
    (list :alternation
	  (list :sequence
		(list :greedy-repetition 0 1 ":")
		(list :greedy-repetition 1 nil `(:char-class (:RANGE #\a #\z)
							     (:RANGE #\A #\Z)
							     (:RANGE #\0 #\9))))
	  (list :GREEDY-REPETITION 1 NIL `(:char-class ,@*punctuation-chars*))))

(defscan *html-scanner-preserve-space*
    (list :alternation
	  (list :sequence
		(list :greedy-repetition 1 1 ":")
		(list :greedy-repetition 1 nil `(:char-class (:RANGE #\a #\z)
							     (:RANGE #\A #\Z)))
		(list :greedy-repetition 0 nil `(:char-class (:RANGE #\0 #\9))))
	  (list :sequence
		(list :greedy-repetition 1 nil `(:char-class (:RANGE #\a #\z)
							     (:RANGE #\A #\Z)
							     (:RANGE #\0 #\9))))
	  (list :greedy-repetition 1 nil " ")
	  (list :GREEDY-REPETITION 1 NIL `(:char-class ,@*punctuation-chars*))))

(defscan *tag-scanner*
      (list :sequence
		(list :greedy-repetition 0 1 ":")
		(list :greedy-repetition 1 nil `(:char-class (:RANGE #\a #\z)
							     (:RANGE #\A #\Z)
							     (:RANGE #\0 #\9)))))

(defun trim (string)
  (trim-string *whitespace-chars* string))

(defun-exported trim-funny (string)
  (replace-expander (((cl-ppcre:create-scanner
		       (list :sequence
			     :START-ANCHOR 
			     (list :GREEDY-REPETITION 1 NIL `(:char-class ,@*funny-chars*))))
		      . "")
		     ((cl-ppcre:create-scanner
		       (list :sequence
			     (list :greedy-repetition 1 nil `(:char-class ,@*funny-chars*))
			     :end-anchor)) 
		      . ""))
		    string))

(defscan *more-spaces* "[ ]+")

(defparameter *space* " ")

(defun rep (string pattern replace-with)
  (cl-ppcre:regex-replace-all pattern string replace-with))

(defun-exported replace-funny (string &key (punctuation t) (with *space*))
  (trim-funny 
   (rep string 
	(if punctuation
	    *funny-punc-scanner*
	    *funny-scanner*)
	with)))

(defparameter *html-chars* 
  '(("&lt;" . "<")
    ("&gt;" . ">")
    ("&amp;" . "&")
    ("&quot;" . "'")
    ("&#039;" . "\"")))

(defun-exported unescape-string (str)
  (replace-expander (("&lt;" . "<")
		      ("&gt;" . ">")
		      ("&amp;" . "&")
		      ("&quot;" . "'")
		      ("&#039;" . "\""))
		    str))

(defun cleanup-url (url)
  (unescape-string url))

(defun-exported cleanup-text (string &key (upcase t) (punctuation t))
   (trim
   (cl-ppcre:regex-replace-all
    (cl-ppcre:create-scanner
     '(:register
       (:GREEDY-REPETITION 1 NIL (:CHAR-CLASS #\ ))))
    (when-bind (x (unicode-replace
		   (if upcase
		       (string-upcase string)
		       string)))
      (if punctuation
	  (cl-ppcre:regex-replace-all
	   (cl-ppcre:create-scanner 
	    '(:register
	      (:char-class #\Newline #\linefeed #\return #\tab #\space #\. #\, #\) #\( #\; #\: #\? #\! #\@ #\& #\^ #\* #\+ #\' #\_ #\- #\/ #\\ #\&)))
	   x
	   " ")
	  (cl-ppcre:regex-replace-all
	   (cl-ppcre:create-scanner 
	    "\\\\")
	   x
	   "")))
    " ")))

(defun file-type (url)
  (aif (car (cl-ppcre:all-matches-as-strings "\\.[a-z]{3,}$" url))
       (subseq it 1)
       "jpg"))

(defun parse-float (string &key (start 0) end (radix 10) junk-allowed)
  "Converts a substring of STRING, as delimited by START and END, to a 
   floating point number, if possible. START and END default to the 
   beginning and end of the string. RADIX must be between 2 and 36. 
   A floating point number will be returned if the string consists of an
   optional string of spaces and an optional sign, followed by a string
   of digits optionally containing a decimal point, and an optional e or
   E followed by an optionally signed integer. The use of e/E to indicate
   an exponent only works for RADIX = 10. Returns the floating point
   number, if any, and the index for the first character after the number."

  ;; END defaults to the end of the string
  ;; We don't accomplish this by sticking (end (length string)) in the 
  ;; lambda list because I've encountered too many implementations that 
  ;; don't handle such properly. Also, this will work ok if somebody calls
  ;; the function with :end nil.
  (setq end (or end (length string))) 

  ;; Skip over whitespace. If there's nothing but whitespace, signal an error.
  (let ((index (or (position-if-not #'whitespacep string :start start :end end)
                   (if junk-allowed
                       (return-from parse-float (values nil end))
                     (error "No non-whitespace characters in number."))))
        (minusp nil) (decimalp nil) (found-digit nil) 
        (before-decimal 0) (after-decimal 0) (decimal-counter 0)
        (exponent 0)
        (result 0))
    (declare (fixnum index))

    ;; Take care of optional sign.
    (let ((char (char string index)))
      (cond ((char= char #\-)
             (setq minusp t)
             (incf index))
            ((char= char #\+)
             (incf index))))

    (loop
     (when (= index end) (return nil))
     (let* ((char (char string index))
            (weight (digit-char-p char radix)))
       (cond ((and weight (not decimalp))
              ;; A digit before the decimal point
              (setq before-decimal (+ weight (* before-decimal radix))
                    found-digit t))
             ((and weight decimalp)
              ;; A digit after the decimal point
              (setq after-decimal (+ weight (* after-decimal radix))
                    found-digit t)
              (incf decimal-counter))
             ((and (char= char #\.) (not decimalp))
	      ;; The decimal point
              (setq decimalp t))
             ((and (char-equal char #\e) (= radix 10))
	      ;; E is for exponent
              (multiple-value-bind (num idx) 
                  (parse-integer string :start (1+ index) :end end
                                 :radix radix :junk-allowed junk-allowed)
                (setq exponent (or num 0)
                      index idx)
		(when (= index end) (return nil))))
             (junk-allowed (return nil))
             ((whitespacep char)
              (when (position-if-not #'whitespacep string
                                     :start (1+ index) :end end)
                (error "There's junk in this string: ~S." string))
              (return nil))
             (t
              (error "There's junk in this string: ~S." string))))
     (incf index))

    ;; Cobble up the resulting number
    (setq result (float (* (+ before-decimal
                              (* after-decimal 
                                 (expt radix (- decimal-counter))))
                           (expt radix exponent))))

    ;; Return the result
    (values
     (if found-digit
         (if minusp (- result) result)
       (if junk-allowed
           nil
         (error "There's no digits in this string: ~S" string)))
     index)))

(defun replace-chain (string replacements)
  (let ((s string))
    (dolist (replacement replacements)
      (awhen (cl-ppcre:regex-replace-all (car replacement) s (cdr replacement))
	(setf s it)))
    s))
