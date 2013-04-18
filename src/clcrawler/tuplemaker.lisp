(in-package :com.crawler)

(defstruct tuple table dbname fields)
(defstruct field name dbname type dbtype conv-fn)

(defmacro-exported defdomain (name &rest tuples)
  (with-gensyms (tuple defs field domainid tableid) 
    `(let ((,defs (mapcar #'(lambda (,tuple)
			      (make-tuple 
			       :table (second ,tuple)
			       :dbname (fourth ,tuple)
			       :fields (mapcar
					#'(lambda (,field) 
					    (apply #'make-field ,field))
					(sixth ,tuple))))
			 ',tuples)))
       (defun ,(make-logic-symbol (s+ (symbol-name name) "-defs")) ()
	 ,defs))))

(defparameter *types* (hash :test 'equal))
(defparameter *simplest* (hash :test 'equal))

(defun add-to-simplest (type detail)
  (hash-put *simplest* (symbol-name type) detail))

(add-to-simplest 'name '(string))
(add-to-simplest 'uniq '(string))
(add-to-simplest 'phone '(string))
(add-to-simplest 'address '(string))
(add-to-simplest 'postcode '(string))
(add-to-simplest 'url '(string))
(add-to-simplest 'date '(string))
(add-to-simplest 'time '(string))

(defun add-to-type (type dbtype)
  (progn
    (hash-put *types* (symbol-name type) (symbol-name dbtype))
    (hash-put *types* (symbol-name dbtype) (symbol-name type))))

(add-to-type 'string 'varchar)

(defun same-type-p (type1 type2)
  (or (eql type1 type2)
      (string= (symbol-name type2) (gethash (symbol-name type1) *types*))))

(defun simplest-type (type)
  (first (hash-get *simplest* (symbol-name type))))

(defun make-conv-fn (field)
  (if (same-type-p (simplest-type (field-type field))
		   (field-dbtype field))
      #'identity))

(defstruct wrapper page pattern type follows-from tuple)

(defmacro-exported defwrapper (name main-url seeder &rest wrappers)
  (with-gensyms (wrapper defs setup url acc pages template in out)
    `(let ((,defs (mapcar #'(lambda (,wrapper)
			     (apply #'make-wrapper ,wrapper))
			 ',wrappers)))
       (defun ,(make-logic-symbol (s+ (symbol-name name) "-defs")) ()
	 ,defs)
       (defun ,(make-logic-symbol (s+ (symbol-name name) "-crawler")) (&key (,setup t))
	 (gather-data 
	  ,main-url
	  ,(format nil "/home/vb/repository/portfolio/crawler/data/seed-urls/~A.urls" (string-downcase (symbol-name name)))
	  #'(lambda (,url)
	      (and
	       (cl-ppcre:all-matches ,main-url ,url)
	       (or
		,@(mapcar #'(lambda (x) `(cl-ppcre:all-matches ,(second (member :pattern x)) ,url))  wrappers))))
	  :setup ,setup
	  :form-urls t
	  :debug nil
	  :seed-urls (seeds ,seeder)))
      ,@(mapcar 
	  #'(lambda (wrapper)
	      `(defun ,(make-logic-symbol (s+ (symbol-name name) "-" (symbol-name (second (member :page wrapper))) "-extracter")) ()
		 (ensure-directories-exist (site-path (url-site ,main-url) "extracted/"))
		 (scrape 
		     ,main-url
		     (,(second (member :pattern wrapper))) 
		   (with-open-file (,out (site-path (url-site ,main-url) (format nil "extracted/~A" docid)) :direction :output :if-exists :supersede :if-does-not-exist :create)
		     (write (funcall #',(second (member :tuple wrapper)) url docid) :stream ,out)))))
	  wrappers)
        (defun ,(make-logic-symbol (s+ (symbol-name name) "-extracter")) ()
	 (progn
	   ,@(mapcar 
	      #'(lambda (wrapper)
		  `(,(make-logic-symbol (s+ (symbol-name name) "-" (symbol-name (second (member :page wrapper))) "-extracter"))))
	      wrappers)))
       ,@(mapcar
	  #'(lambda (wrapper)
	      `(defun ,(make-logic-symbol (s+ (symbol-name name) "-" (symbol-name (second (member :page wrapper))) "-analyzer")) ()
		 (awrapper 
		  ,main-url 
		  (site-path (url-site ,main-url)
			     ,(format nil "wrappers/~A-~A.wrapper" 
				      name
				      (second (member :page wrapper))))
		  ,(second (member :pattern wrapper)))))
	  wrappers)
       (defun ,(make-logic-symbol (s+ (symbol-name name) "-pruner")) ()
	 (progn
	 ,@(mapcar #'(lambda (wrapper)
		       `(let ((i 5))
			  (read-all-pages 
			   ,main-url
			   #'(lambda (url docid)
			       (when (> i 0)
				 (write-subst url (read-page url))
				 (decf i)))
			   :filter #'(lambda (url)
					(and (cl-ppcre:all-matches ,main-url url)
					     (cl-ppcre:all-matches ,(second (member :pattern wrapper)) url))))))
		   wrappers)))
       (defun ,(make-logic-symbol (s+ (symbol-name name) "-analyzer")) ()
	 (progn
	   ,@(mapcar 
	      #'(lambda (wrapper)
		  `(,(make-logic-symbol (s+ (symbol-name name) "-" (symbol-name (second (member :page wrapper))) "-analyzer"))))
	      wrappers)))
       ,@(mapcar
	  #'(lambda (wrapper)
	      `(defun ,(make-logic-symbol (s+ (symbol-name name) "-" (symbol-name (second (member :page wrapper))) "-inducer")) ()
		 (let ((,acc '())
		       (,pages '()))
		   (with-open-file (,in (site-path (url-site ,main-url)
						   ,(format nil "wrappers/~A-~A.wrapper" 
							    name
							    (second (member :page wrapper)))))
			     (dolist (,template (read ,in))
		       (when (and (member :tbtemplate (third ,template))
				  (< (length (car (second ,template))) 10))
			 (push (list (first ,template)
				     (car (second ,template))
				     (third ,template))
			       ,acc))))
		   (read-all-pages ,main-url
				     #'(lambda (url docid)
					 (print (cons 'inducing docid))
					 (read-page url))
				     :filter
				     #'(LAMBDA (url)
					 (AND
					  (CL-PPCRE:ALL-MATCHES ,main-url url)
					  (CL-PPCRE:ALL-MATCHES
					   ,(second (member :pattern wrapper))
					   url)))))))
	  wrappers)
       (defun ,(make-logic-symbol (s+ (symbol-name name) "-inducer")) ()
	 (progn
	   ,@(mapcar
	      #'(lambda (wrapper)
		  `(,(make-logic-symbol (s+ (symbol-name name) "-" (symbol-name (second (member :page wrapper))) "-inducer"))))
	      wrappers)))
       ,@(mapcar
	  #'(lambda (wrapper)
	      `(defun ,(make-logic-symbol (s+ (symbol-name name) "-" (symbol-name (second (member :page wrapper))) "-templater")) ()
		 (with-open-file (,in (site-path (url-site ,main-url)
						 ,(format nil "wrappers/~A-~A.wrapper" 
							  name
							  (second (member :page wrapper)))))
		   (let ((wrap (read ,in)))
		     (read-all-pages 
		      ,main-url
		      #'(lambda (url docid)
			  (print (cons 'templating docid))
			  (write-tagged-html (mark-page (read-page url) wrap) (url-site url) url :template))
		      :filter
		      #'(LAMBDA (url)
			  (AND
			   (CL-PPCRE:ALL-MATCHES ,main-url url)
			   (CL-PPCRE:ALL-MATCHES
			    ,(second (member :pattern wrapper))
			    url))))))))
	  wrappers)
       (defun ,(make-logic-symbol (s+ (symbol-name name) "-templater")) ()
	 (progn
	   ,@(mapcar
	      #'(lambda (wrapper)
		  `(,(make-logic-symbol (s+ (symbol-name name) "-" (symbol-name (second (member :page wrapper))) "-templater"))))
	      wrappers)))
       (defun ,(make-logic-symbol (s+ (symbol-name name) "-setup")) ()
	 (progn
	   (add-to-extracter ',name ',(make-logic-symbol (s+ (symbol-name name) "-extracter")))
	   (add-to-analyzer ',name ',(make-logic-symbol (s+ (symbol-name name) "-analyzer")))
	   (add-to-crawler ',name ',(make-logic-symbol (s+ (symbol-name name) "-crawler")))
	   (add-to-pruner ',name ',(make-logic-symbol (s+ (symbol-name name) "-pruner"))))))))

(defun only-first-n (main-url pattern n fn)
  (let ((i n)
	(pages '()))
    (read-all-pages 
     main-url
     #'(lambda (url docid)
	 (when (> i 0)
	   (push (list docid url (tidy-page (read-page url))) pages)
	   (decf i)))
     :filter
     #'(lambda (url)
	 (and (cl-ppcre:all-matches main-url url)
	      (cl-ppcre:all-matches pattern url))))
    (print 'read-pages)
    (funcall fn pages)))

(defun awrapper (main-url path pattern)
  (only-first-n
   main-url
   pattern
   5
   #'(lambda (pages)
       (write-wrapper 
	path
	(analyze-wrappers pages)))))

(defun-exported uploader (site)
  (let ((extracted (all-files (site-path site "extracted/") :full t)))
    (dolist (extract extracted)
      (with-open-file (in extract)
	(upload-data (read in))))))

(defun upload-data (read-data)
  )
