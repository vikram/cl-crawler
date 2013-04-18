(in-package :com.crawler)

(defun make-curly-readtable (&optional (original-readtable *readtable*)
                             &aux (rv (copy-readtable original-readtable)))
  "Return new readtable with curly syntax enabled."
  (set-macro-character #\{ #'curly-reader nil rv)
  (set-macro-character #\[ #'square-reader nil rv)
  (set-syntax-from-char #\} #\) rv)
  (set-syntax-from-char #\] #\) rv)
  rv)

(defvar *original-readtable* nil
  "Original readtable to restore, used by ENABLE/DISABLE-CURLY-SYNTAX.")

(defmacro enable-curly-syntax ()
  "Enable curly syntax for current file."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *original-readtable* *readtable*
          *readtable* (make-curly-readtable))))

(defmacro disable-curly-syntax ()
  "Disable curly syntax for current file.

Warning: Calling DISABLE-CURLY-SYNTAX when curly syntax is not
enabled can give funny results.  Also, reading multiple files
using ENABLE-CURLY-SYNTAX and DISABLE-CURLY-SYNTAX in different
threads can invoke a disaster.  ENABLE-CURLY-SYNTAX itself is
safe."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless *original-readtable*
       (error "Curly syntax not enabled."))
     (setf *readtable* *original-readtable*
           *original-readtable* nil)))

(defun square-reader (stream char
                      &aux (funs (read-delimited-list #\] stream t)))
  "Reader macro for #\[ character."
  (declare (ignore char))
  (when (null funs)
    (error "Empty square braces."))
  (let ((arg (gensym)))
    `(lambda (,arg)
       (print ,'funs))))

(defun curly-reader (stream char
                     &aux (funs (read-delimited-list #\} stream t)))
  "Reader macro for #\{ character."
  (declare (ignore char))
  (when (null funs)
    (error "Empty curly braces."))
  `(mapcar #'(lambda (x) (print x)) ,funs))

#|


$( (?divs (not div@id))
  ?divs)

($ (?divs div.panel)
 ?divs)

($ (?divs (or div.panel div#content))
 ?divs)

($ (?ps p)
  (nth 5 ?ps))

($ (?trs (in table.orders tr~odd))
  ?trs)

($ mypage (is input@name email)
  nodes)

($ page (like a@href "^http://")
  nodes)

($ page (has p a)
  node)

($ page (nth-child (in ul li) 2)
 nodes)

<rule>: ($ page <query> @body)
<query>: (not <query>)
       : (and <query>*)
       : (or <query>*)
       : (in <query>{2,})
       : (is <query> <lisp expression>)
       : (like <query> <regex>)
       : (<operator> <query>|<lisp expression>*)
       : <selector>
<selector>: node.class
          : node#id
          : node~property
          : node@attribute
<var> : ?<symbol>
<operator>: has parent children next first last even odd lt nth gt empty contains only-child last-child first-child nth-child
|#

(defstruct node name id class att)

(defun find-att (atts char)
  (when-bind (atts (remove-if-not #'(lambda (x) (eql (aref x 0) char)) atts))
    (mapcar #'(lambda (x) (subseq x 1)) atts)))

(defun create-node (name)
  (let* ((n (string-upcase (symbol-name name)))
	 (atts (cl-ppcre:all-matches-as-strings "[~#\\.@][A-Z0-9_]+" n)))
    (make-node :name (first (cl-ppcre:all-matches-as-strings "^[A-Z0-9_]+" n))
	       :id (and atts (find-att atts #\#) (make-logic-symbol (first (find-att atts #\#))))
	       :class (and atts (find-att atts #\.))
	       :att (and atts (find-att atts #\@)))))

(defparameter *tokens* (make-hash-table))

(defun special? (token)
  (aif (member token '($ not and or in is like))
       t
       (gethash token *tokens*)))

(defmacro-exported $ ((var query) page &body body)
  `(let ((,var ,(run-query page query)))
     (declare (ignorable nodes))
     (when ,var
       ,@body)))

(defmacro $* (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(let ((,(first (car binds)) ,(run-query (third (car binds)) (second (car binds)))))
	 (declare (ignorable ,(first (car binds))))
	 ($* ,(cdr binds) ,@body))))
       
(defun @ (html att)
    (extract-next-element html att))

(defun create-ext (q val page)
  (let ((node (create-node q)))
    `,(append
       `(ext ,page 
	    :node ,(make-keyword (node-name node))
	    :id ',(node-id node)
	    :class ',(node-class node)
	    :debug nil)
       (if val 
	   `(:attval (cons ,(make-keyword (node-att node)) ,val))
	   `(:att ,(make-keyword (node-att node)))))))

(defun run-query (page q)
  (if (atom q)
      (if (symbolp q)
	  (create-ext q nil page)
	  `(elmtext ,page ,q))
      (let* ((token (car q))
	     (form (cdr q)))
	(case token
	  (not nil)
	  (and nil)
	  (or nil)
	  (in nil)
	  (is (create-ext (first form) (second form) page))
	  (like nil)))))

(defmacro-exported scrape-cont (site-url filters skip-until &body body)
  (with-gensyms (url-to-test)
    `(read-all-pages
      ,site-url
      #'(lambda (url docid)
	  ,@body)
      :filter
      #'(lambda (,url-to-test)
	  ,(if filters
	       `(and
		 (cl-ppcre:all-matches ,site-url ,url-to-test)
		 ,@(mapcar #'(lambda (x) `(cl-ppcre:all-matches ,x ,url-to-test)) filters))
	       `(cl-ppcre:all-matches ,site-url ,url-to-test)))
      :skip-until ,skip-until)))

(defmacro scrape (site-url filters &body body)
  (with-gensyms (url-to-test)
    `(read-all-pages
      ,site-url
      #'(lambda (url docid)
	  ,@body)
      :filter
      #'(lambda (,url-to-test)
	  ,(if filters
	       `(and
		 (cl-ppcre:all-matches ,site-url ,url-to-test)
		 ,@(mapcar #'(lambda (x) `(cl-ppcre:all-matches ,x ,url-to-test)) filters))
	       `(cl-ppcre:all-matches ,site-url ,url-to-test))))))

(defmacro deffilters (site-url &rest filters)
  (with-gensyms (url)
    `#'(lambda (,url)
	 (and
	  (cl-ppcre:all-matches ,site-url ,url)
	  ,@(mapcar #'(lambda (x) `(cl-ppcre:all-matches ,x ,url)) filters)))))

(defmacro defscrapper (name site-url filters &body body)
  `(scrape ,site-url ,filters ,@body))

(defparameter-exported *debug-extracter* nil)

(defmacro-exported defext (function-spec lambda-list &body body)
  `(defun ,function-spec ,lambda-list
     (if *debug-extracter*
	 (print (list ',function-spec ,@(remove-if #'(lambda (x) (or (eql x '&key) (eql x '&optional) (eql x '&rest) (consp x))) lambda-list)))
	 (progn
	   ,@body))))


