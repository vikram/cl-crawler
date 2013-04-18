(in-package :com.crawler)

(defun first-string< (x y)
  (string< 
   (first x) 
   (first y)))

(defun-exported just-words (tokens)
  (mapcar #'car tokens))

(defparameter *word-money-scanner*
  (cl-ppcre:create-scanner "\\b[\\\.\\\-a-zA-Z0-9,]+\\b" :multi-line-mode t))

(defparameter *word-scanner*
  (cl-ppcre:create-scanner "\\b[a-zA-Z0-9]*\\b" :multi-line-mode t))

(defun split-text (text)
  (cl-ppcre:all-matches-as-strings
   *word-money-scanner*
   text))

(defun whitespace-splitter (str)
  (cl-ppcre:split *whitespace-scanner* str))

(defun tokenizer (scanner)
  #'(lambda (doc)
      (let ((pos 0))
	(sort 
	 (mapcar 
	  #'(lambda (term) (cons term (incf pos)))
	  (cl-ppcre:all-matches-as-strings 
	   scanner
	   (cleanup-text doc)))
	 #'first-string<))))

(defmacro deftokenizer (name scanner)
  `(setf (symbol-function ',name)
	 (tokenizer ,scanner)))
  
(deftokenizer word-tokenizer *word-scanner*)
(deftokenizer word-money-tokenizer *word-money-scanner*)
(deftokenizer indexable-words "\\b[a-zA-Z0-9]+\\b")
(deftokenizer word-punc-tokenizer *word-punc-scanner*)

(defun word-punc-splitter (str)
  (cl-ppcre:all-matches-as-strings *word-punc-scanner* str))

(defun indexable-words-ranked (string-list)
  (let ((i 0))
    (mappend 
     #'(lambda (string-pair)
	 (incf i)
	 (mapcar 
	  #'(lambda (pair) (cons (car pair) i)) 
	  (indexable-words (cdr string-pair))))
     string-list)))

(defun unicode-replace (string)
  string)

#+sbcl
(defun unicode-replace (string)
  (cl-ppcre:regex-replace-all 
   (cl-ppcre:create-scanner '(:register 
			      (:sequence 
			       (:greedy-repetition 0 nil #\ ) 
			       (:char-class #\Start-Guarded-Area) 
			       (:greedy-repetition 0 nil #\ ))))
   (cl-ppcre:regex-replace-all 
    (cl-ppcre:create-scanner '(:register 
			       (:sequence 
				(:GREEDY-REPETITION 0 NIL #\ ) 
				(:char-class #\Private-Use-Two)))) 
    string "FEET")
   "-"))

;;aserve
(defvar *max-word* 25)
(defparameter *english-stop-words*
  '("a" "an" "and" "are" "as" "at" "be" "but" "by" "for" "if" "see" "do" "has" "so" "why" "how" "www" "am"
    "in" "into" "is" "it" "no" "not" "of" "on" "or" "s" "such" "my" "can" "just" "no" "what" "me" "when"
    "t" "that" "the" "their" "then" "there" "these" "i" "you" "we" "our" "some" "have" "all" "which" "org"
    "they" "this" "to" "was" "will" "with" "its" "like" "com" "http" "co" "your" "from"))
(defvar ch-alpha 0)
(defvar ch-space 1)
(defvar ch-sep   2)  ; separators

(defvar *syntax-table*
    (let ((arr (make-array 100000 :initial-element ch-alpha)))
      
      ; the default so we don't have to set it
      #+ignore (do ((code (char-code #\!) (1+ code)))
	  ((> code #.(char-code #\~)))
	(setf (svref arr code) ch-alpha))
      
      (setf (svref arr (char-code #\space)) ch-space)
      (setf (svref arr (char-code #\Page)) ch-space)
      (setf (svref arr (char-code #\tab)) ch-space)
      (setf (svref arr (char-code #\return)) ch-space)
      (setf (svref arr (char-code #\linefeed)) ch-space)
       arr))

(defun stop-word-p (word)
  (declare (simple-base-string word) (cons *english-stop-words*) (optimize (safety 0) (speed 3)))
  (position word *english-stop-words* :test #'(lambda (x y) (string= (string-downcase x)
								     y))))

(cl-ppcre:define-parse-tree-synonym alpha
    (:CHAR-CLASS (:RANGE #\a #\z) (:RANGE #\A #\Z)))

(defparameter *letter-scanner* 
  (cl-ppcre:create-scanner "."))

(defun letter-tokenizer (string)
  (cl-ppcre:all-matches-as-strings *letter-scanner* string))

(defun letters-p (word)
  (cl-ppcre:scan *letter-scanner* word))

(defun word-stats (word-list)
  (let ((words (freq word-list)))
    (declare (optimize (speed 3) (safety 0) (space 0))
             (sequence word-list) (hash-table words))
    (cdr (hash-table->alist words))))

(defun make-word-list (string)
  (let ((end (length string))
	(word-buf (make-string *max-word*))
	(pos 0)
	word-len
	res)
    (loop
      (multiple-value-setq (word-len pos)
	(tokenize-string string pos end word-buf))
      (if (null word-len)
	  (return))
      (push (subseq word-buf 0 word-len) res))
    (nreverse res)))

(defmacro valid-char-p (c)
  `(svref valid-chars (char-code ,c)))

(defmacro fast-char-downcase (char)
  `(svref #.(let ((arr (make-array 65536)))
	      (dotimes (n 65536)
		(let ((char (code-char n)))
		  (setf (svref arr n) (char-downcase char))))
	      arr)
	  (char-code ,char)))

(defun string-downcase-in-place (str)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (string str))
  (dotimes (n (length str))
    (declare (fixnum n))
    (setf (schar str n) 
      (fast-char-downcase (schar str n)))))

(defmacro advance-to-invalid-char (s p m)
  (let ((string (gensym))
	(pos (gensym))
	(max (gensym)))
    `(let ((,string ,s)
	   (,pos ,p)
	   (,max ,m))
       (declare (optimize (speed 3) (safety 0) (debug 0))
		(simple-string ,string)
		(fixnum ,pos ,max))
       (while (and (< ,pos ,max) (valid-char-p (schar ,string ,pos)))
	 (incf ,pos))
	 
       ,pos)))

(defmacro advance-to-valid-char (s p m)
  (let ((string (gensym))
	(pos (gensym))
	(max (gensym)))
    `(let ((,string ,s)
	   (,pos ,p)
	   (,max ,m))
       (declare (optimize (speed 3) (safety 0) (debug 0))
		(simple-string ,string)
		(fixnum ,pos ,max))
       (while (and (< ,pos ,max) (not (valid-char-p (schar ,string ,pos))))
	 (incf ,pos))
	 
       ,pos)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(let ((valid-chars #.(let ((arr (make-array 65536)))
		       (dotimes (n 65536)
			 (let ((char (code-char n)))
			   (setf (svref arr n)
				 (alphanumericp char))))
				 ;(or (alphanumericp char) (char= char #\:)))))
		       arr)))
  
  (defun tokenize-string (string pos end outbuf)
    (declare (optimize (speed 3) (safety 0) (debug 0))
	     (simple-string string outbuf)
	     (fixnum pos end))
    (setf pos (advance-to-valid-char string pos end))
    (if (= pos end)
	(return-from tokenize-string))
    (let ((count 0))
      (declare (fixnum count))
      (while (< pos end) 
	(let ((char (schar string pos)))
	  (if (not (valid-char-p char))
	      (return))
	  (if (>= count *max-word*)
	      (return-from tokenize-string
		(tokenize-string string 
				 (advance-to-invalid-char string pos end)
				 end
				 outbuf)))
	  
	  (setf (schar outbuf count) (fast-char-downcase char))
	  
	  (incf pos)
	  (incf count)))
      (values count pos)))))

