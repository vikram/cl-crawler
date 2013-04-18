(in-package :com.crawler)

(defparameter *content-type->ext*
  '(("icon" . ".ico")
    ("gif" . ".gif")
    ("png" . ".png")
    ("bmp" . ".bmp")
    ("jpeg" . ".jpg")))
   
(defun deduce-image-extension (lst)
  (dolist (ext *content-type->ext*)
    (when (member (car ext) lst :test #'string=)
      (return (cdr ext)))))

(defun mark-tag-tree (tree)
  (print 'marking-tree)
  (let* ((doc (tag-recurse-expander
	      (((and (tag-p child)
		     (tag-empty-p child)
		     (not (tag-belongs-p child *elements-can-be-empty*)))
		. "tbempty")
	       (*content-elements* . "tbcontent")
	       ((and (tag-p child)
		     (not (tag-empty-p child))
		     (tag-node-p child "div")
		     (not (tag-contains-only child "div")))
		.
		"tbcontent"))
	      tree))
	 (head (stp:find-recursively-if #'(lambda (x) (and (tag-p x) (tag-node-p x "head"))) doc)))
    (stp:append-child head (create-link "http://crawler/css/bots.css"))
    (stp:append-child head (create-script-src "http://crawler/javascripts/jquery-1.2.3.min.js"))
    (stp:append-child head (create-script-src "http://crawler/javascripts/bots.js"))
    doc))

(defun create-link (path)
  (let ((link (stp:make-element "link")))
    (stp:add-attribute link (stp:make-attribute path "href"))
    (stp:add-attribute link (stp:make-attribute "stylesheet" "rel"))
    (stp:add-attribute link (stp:make-attribute "text/css" "type"))
    link))

(defun create-script-src (path)
  (let ((script (stp:make-element "script")))
    (stp:add-attribute script (stp:make-attribute "text/javascript" "type"))
    (stp:add-attribute script (stp:make-attribute path "src"))
    script))

(defun create-script-code (code)
  (let ((script (stp:make-element "script")))
    (stp:add-attribute script (stp:make-attribute "text/javascript" "type"))
    (stp:add-attribute script (stp:make-attribute "javascript" "language"))
    (stp:append-child script (stp:make-text code))
    script))

(defun crawl-page-completely (url)
  (let* ((site (read-site-info url))
	 (docid (make-hash-code url))
	 (page (fetch-while-empty url docid)))
    (write-page url page)))

(defun subst-att (thisurl page &key (debug nil) (fetch t))
  (with-tags/document (tag page)
    (when (tag-p tag) 
      (awhen (first (member (tag-sym tag) *included-link-elements* :key #'car))
	(when-bind (url (tag-elm tag (cdr it)))
	  (unless (string= "" url)
	    (print url)
	    (let* ((site (url-site thisurl))
		   (ext (first (file-extension url)))
		   (name (s+ (make-hash-code url) ext))
		   (path (site-path site (s+ "cleaned/" name))))
	      (when debug
		(print tag)
		(print path))
	      (setf (stp:attribute-value tag (cdr it)) (s+ "/bots/cleaned/" name))
	      (when fetch
		  (unless (probe-file path)
		    (multiple-value-bind (html code headers uri)
			(fetch (normalized-link thisurl url))
		      (when debug (print (first (cl-ppcre:all-matches-as-strings "[a-z]+" (cdr (assoc :content-type headers))))))
		      (casequal (first (cl-ppcre:all-matches-as-strings "[a-z]+" (cdr (assoc :content-type headers))))
				("image" 
				 (progn
				   (when (or (not ext) (string= "" ext))
				     (setf ext (deduce-image-extension (cl-ppcre:all-matches-as-strings "[a-z]+" (cdr (assoc :content-type headers)))))
				     (setf name (s+ (make-hash-code url) ext))
				     (setf path (site-path site (s+ "cleaned/" name)))
				     (setf (stp:attribute-value tag (cdr it)) (s+ "/bots/cleaned/" name)))
				   (write-binary path 1 (vec->lst html))))
				(t
				 (if (tag-node-p tag "img")
				     (setf (stp:attribute-value tag (cdr it)) url)
				     (write-text path html))))))))))))))

(defun write-text (filename text)
  (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string (or text "") out)))

(defun write-html (tag-html filename)
  (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (stp:serialize tag-html (cxml:make-character-stream-sink out))))

(defun write-page (url page title)
  (let ((subst (SUBST-ATT url page)))
    (WRITE-HTML subst (format nil "/home/vb/repository/portfolio/www/src/crawled/~A.html" title))
    (WRITE-HTML (mark-tag-tree subst) (format nil "/home/vb/repository/portfolio/www/src/crawled/~A-pruned.html" title))))

(defun write-subst (url page)
  (let ((subst (SUBST-ATT url page :fetch nil)))
    (write-tagged-html subst url :cleaned)
    (write-tagged-html (mark-tag-tree subst) url :pruned)))

(defun write-tagged-html (tagged-html url label)
  (let ((path (site-path (url-site url) (s+ (string-downcase (symbol-name label)) "/" (make-hash-code url)))))
    (print path)
    (write-html tagged-html (s+ path ".full"))
    (gzip-stream:gzip (s+ path ".full") path)
    (delete-file (s+ path ".full"))))

(defun file-name (name addition)
  (format nil 
	  "/home/vb/repository/portfolio/www/src/crawled/~A~A.html"
	  (join (make-word-list name) "-") 
	  (if (string= addition "")
	      ""
	      (s+ "-" addition))))

(defun write-induced-files (pages)
  (induce-wrappers 
   pages
   #'(lambda (i name url page changes)
       (declare (ignorable i))
	 (let* ((tagged-html (make-changes-to-page page changes))
		(sub (SUBST-ATT url tagged-html)))
	   (write-html sub (file-name name ""))
	   (write-html (prune-tag sub
				  #'(lambda (child) 
				      (and (tag-p child)
					   (tag-class child)
					   (or
					    (tag-class-belongs-p
					     child
					     '("tbnoncontent" "tbsame" "tbempty"))
					    (content-tag-empty-p child)))))
		       (file-name name "pruned"))
	   (write-html (prune-tag sub
				  #'(lambda (child) 
				      (and (tag-p child)
					   (tag-class child)
					   (or
					    (not (tag-class-belongs-p 
						  child 
						  '("tbstructural" "tboptional" "tbtemplate" "tbcontent")))
					    (content-tag-empty-p child)))))
		       (file-name name "template"))))))

(defun induce-wrappers (pages fn &key (debug nil))
  "mark the page with the largest # of blocks with tags same or template or structural"
  (let ((blocks (hash))
	(i 0))
    (dotimes (j (length pages))
      (hash-put blocks (1+ j) (hash :test 'equal)))
    (let ((marked-pages (mapcar #'(lambda (x) (print (first x)) (mark-tag-tree (clast x))) pages)))
      (dolist (page marked-pages)
        (incf i)
        (dolist (node (ext-html page :class "tbcontent"))
          (hash-put (hash-get blocks i) (tag-path node '() t) node)))
          (do-hash (choosen choosen-page-blocks blocks)
            (let ((changes (hash :test 'equal))
                  (choosen-page (nth (1- choosen) marked-pages)))
              (print choosen)
              (do-hash (loc blck (hash-get blocks choosen))
                (print (tag-node blck))
                (setf (gethash blck changes) '())
                (dolist (other (remove-if #'(lambda (x) (= (car x) choosen)) (cdr (hash-table->alist blocks))))
                  (do-hash (oth-loc oth-blck (cdr other))
                    (when (reasonable-path-match loc oth-loc)
                      (let ((similarity (tag-sameness blck oth-blck))
                        (structural (structural-similarity-degree blck oth-blck)))
                        (when debug
                          (print (tag-node oth-blck))
                          (print 'match)
                          (print (cons choosen loc))
                          (print (cons 'structural structural))
                          (print (cons 'same similarity))
                          (print (cons (car other) oth-loc)))
                        (when structural
                          (push (list (car other) oth-blck structural similarity) (gethash blck changes))))))))
                  (funcall fn
                       (1- choosen)
                       (first (nth (1- choosen) pages))
                       (second (nth (1- choosen) pages))
                       choosen-page 
                       changes))))))

(defun make-changes-to-page (page changes)
  (with-tags/document (tag page)
    (when (and (tag-p tag) (tag-has-class tag "tbcontent"))
      (awhen (gethash tag changes)
	(tag-remove-tb-classes tag)
	(tag-add-rel tag (format nil "~A" (average-sims (filter-sims it 0.1 #'<))))
	(if (blck-isdifferent it)
	    (tag-add-class tag "tboptional")
	    (if (< (average-sims (filter-sims it 0.1 #'<)) 0.8)
		(if (> (average-sims (filter-sims it 0.1 #'<)) 0.2)
		    (tag-add-class tag "tbtemplate")
		    (tag-add-class tag "tbstructural"))
		(if (tag-belongs-p tag *feature-elements*)
		    (tag-add-class tag "tblabel")
		    (tag-add-class tag "tbsame"))))))))

(defun merge-blocks (n blcks changes &optional (done '()) (merged '()))
  (setf done (cons (1- n) done))
  (setf merged (append blcks merged))
  (dolist (blck blcks)
    (unless (member (1- (first blck)) done)
      (setf merged 
	    (append 
	     (merge-blocks (1- (first blck))
			   (gethash (second blck)
				    (nth (1- (first blck)) changes))
			   changes
			   done
			   merged)
	     merged))))
  (delete-duplicates merged :test #'equal))

(defun merge-changes (changes)
  (let ((global (hash :test 'equal))
	(i 0)
	(j 0)
	(done (hash :test 'equal))
	(candidates (hash :test 'equal)))
    (dolist (h changes)
      (incf j)
      (do-hash (k v h)
	(unless (hash-get done (done-key j k))
	  (incf i)
	  (hash-put done (done-key j k) t)
	  (let ((blcks (merge-blocks j v changes)))
	    (dolist (blck blcks)
	      (hash-put done (done-key (first blck) (second blck)) t))
	    (hash-put global 
		      (list (tag-loc k) i j)
		      blcks)
	    (hash-put candidates
		      (list (tag-loc k) i j)
		      (remove-if #'(lambda (bk) (= (fourth bk) 0.0))
				 blcks))))))
    (values
     global
     candidates
     done
     changes)))

(defun collect-changes (pages)
  (let ((all-changes '()))
    (induce-wrappers
     pages
     #'(lambda (n name url page changes)
         (print (cons 'inducing name))
	 (push changes all-changes)) 
     :debug nil)
    (nreverse all-changes)))

(defun uniq-location-changes (changes)
  (let ((uniqs (make-counter)))
    (dolist (change (hash-keys changes))
      (incf-counter uniqs (tag-loc change)))
    uniqs))

(defun uniq-locations (changes)
  (let ((uniq-locs (mapcar #'uniq-location-changes changes))
	(uniqs (hash :test 'equal)))
    (dolist (uniq-loc uniq-locs)
      (do-hash (loc count uniq-loc)
	(if (/= count 1) ;not-uniq
	    (if (gethash loc uniqs)
		(incf (gethash loc uniqs) count)
		(setf (gethash loc uniqs) count))
	    (if (or (not (hash-get uniqs loc))
		    (= (hash-get uniqs loc) 1))
		(hash-put uniqs loc 1)))))
    uniqs))

(defun extract-uniqs (locs &key (test #'(lambda (count) (= count 1))))
  (let ((res '()))
    (do-hash (loc count locs)
      (when (funcall test count)
	(push loc res)))
    (nreverse res)))
   
(defun is-disjunct (structs)
  (cdr structs))

(defun is-optional (blcks pages)
  (/= (length blcks) (length pages)))

(defun analyze-wrappers (pages &key (changes (collect-changes pages)))
  (let* ((locs (uniq-locations changes))
	 (uniqs 
	  (mappend #'(lambda (loc)
		      (let* ((blcks (remove-empty 
				     (mapcar #'(lambda (page)
						 (first (ext-by-loc (third page) loc))) ;only expect one
					     pages)))
			     (uniq-structs (freqs (mapcar #'tag-structure blcks) :test #'equal))
			     (class (if (not (is-disjunct uniq-structs))
					(if (is-optional blcks pages)
					    (list :tboptional :tbregular)
					    (list :tbpresent :tbregular))
					(if (is-optional blcks pages)
					    (list :tboptional :tbdisjunct)
					    (list :tbpresent :tbdisjunct)))))
			(mapcar #'(lambda (struct)
				    (let* ((structurally (remove-if-not 
							  #'(lambda (blck)
							      (equal (tag-structure blck) (car struct)))
							  blcks))
					   (tfs (mapcar #'tag-tokens->tf structurally))
					   (idf-tf (doc-tokens->df (mid tfs)))
					   (content (mapcar 
						     #'(lambda (perm) (cosine (first perm) (second perm))) 
						     (permutations (mapcar #'(lambda (x) (tag-tokens->tf x :fn #'tag-noscript-tokens)) structurally)))))
				      (if (= (avg content) 1.0)
					  (list loc struct (cons :tbsame class) idf-tf)
					  (list loc struct (cons :tbtemplate class) idf-tf))))
				 uniq-structs)))
		  (extract-uniqs locs))))
    uniqs))

(defun write-wrapper (path wrapper)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write (mapcar #'(lambda (x)
		       (list (first x)
			     (second x)
			     (third x)
			     (cdr (hash-table->alist (fourth x)))
			     (fifth x)))
		   wrapper) 
	   :stream out)))

(defun template-loc (template)
  (first template))

(defun template-classes (template)
  (third template))

(defun template-struct (template)
  (car (second template)))

(defun mark-page (page wrapper)
  (let ((tagged-html (if (stringp page)
			 (tag-html page)
			 page)))
    (dolist (template wrapper)
      (dolist (tag (ext-by-loc-struct (template-loc template) 
				      (template-struct template) 
				      tagged-html))
	(tag-remove-tb-classes tag)
	(dolist (class (template-classes template))
	  (tag-add-class tag (keyword-name class)))))
    tagged-html))
			     
(defun write-analyzed-html (url name html wrapper)
  (let ((sub (mark-tag-tree (SUBST-ATT url html))))
    (write-html (mark-page sub wrapper) 
		(file-name name "analyzed"))))

