(in-package :com.crawler)

(defun follows (list test)
  (= (length (remove-empty (map-subsequent-two test list #'cons)))
     (1- (length list))))

(defun tag-elm (x att)
  (stp:attribute-value x att))

(defun tag-p (x)
  (typep x 'stp:element))

(defun tag-id (x)
  (tag-elm x "id"))

(defun tag-node (x)
  (if (eql (tag-type x) 'stp:element)
      (stp:local-name x)
      ""))

(defun tag-sym (x)
  (make-keyword (tag-node x)))

(defun tag-type (x)
  (type-of x))

(defun tag-class (x)
  (tag-elm x "class"))

(defun idied (id)
  (when (and id (string/= id ""))
    (format nil "#~A" id)))

(defun classed (class)
  (when (and class (string/= class ""))
    (trim (format nil ".~A" class))))

(defun nthed (n)
  (when n
    (format nil ":~A" n)))

(defun empty-if-nil (x)
  (if x
      x
      ""))

(defun tag-add-class (tag class)
  (aif (stp:find-attribute-named tag "class")
       (setf (stp:attribute-value tag "class")
             (s+ (stp:attribute-value tag "class") " " class))
       (stp:add-attribute tag
                          (stp:make-attribute class "class"))))

(defun tag-add-rel (tag rel)
  (aif (stp:find-attribute-named tag "rel")
       (setf (stp:attribute-value tag "rel")
             rel)
       (stp:add-attribute tag
                          (stp:make-attribute rel "rel"))))

(defun tag-remove-class-if (tag pred)
  (awhen (stp:find-attribute-named tag "class")
    (setf (stp:attribute-value tag "class")
          (join (remove-if pred (make-word-list (stp:attribute-value tag "class"))) " "))))

(defun tag-loc->str (tag &key (nth t))
  (format nil "~A~A~A~A"
          (tag-node tag)
          (empty-if-nil (idied (tag-id tag)))
          (empty-if-nil (classed (tag-class tag)))
          (if (not nth)
              ""
              (empty-if-nil (nthed (tag-nth tag))))))

(defun tag-nth (tag &optional (i 0))
  (handler-case (tag-nth (stp:previous-sibling tag) (1+ i))
    (stp:stp-error () i)))

(defun tag-token (tag &key (no-nodes nil))
  (let ((acc '()))
    (typecase tag
      (stp:element
       (progn
         (when (tag-node-p tag :img)
           (push (tag-elm tag "alt") acc)
           (push (tag-elm tag "title") acc))
         (unless no-nodes
           (push (make-keyword (tag-node tag)) acc))))
      (stp:document nil)
      (stp:comment (tag-elm tag "data"))
      (stp:text (push (split-text (stp:data tag)) acc))
      (stp:document-type nil)
      (stp:processing-instruction (tag-elm tag "href")))
    acc))

(defun tag-path-text (tag &optional (path '()))
  (aif (stp:parent tag)
       (tag-path-text it
                      (typecase tag
                        (stp:element
                         (if (null path)
                             (list (tag-node tag))
                             (cons (tag-node tag) path)))
                        (stp:document path)
                        (stp:text
                         (cons "text" path))
                        (t
                         (cons (stp:local-name tag) path))))
       path))

(defun tag-path (tag &optional (path '()) (full nil))
  (aif (stp:parent tag)
       (tag-path
        it
        (typecase tag
          (stp:element
           (if (null path)
               (list (tag-loc->str tag))
               (if full
                   (cons (tag-loc->str tag) path)
                   (cons (tag-loc->str tag :nth nil) path)))) ;don't record the position for the parent
          (stp:document path)
          (stp:text
           (if full
               (cons (format nil "text:~A" (tag-nth tag)) path)
               path))
          (t
           (cons (stp:local-name tag) path)))
        full)
       path))

(defun tag-name (tag)
  (typecase tag
    (stp:element
     (tag-node tag))
     (stp:text
      "text")
     (t
      (stp:local-name tag))))

(defun string-list= (lst1 lst2)
  (string=
   (apply #'concatenate 'string lst1)
   (apply #'concatenate 'string lst2)))

(defscan *node-scanner* "[a-z]+")

(defun reasonable-path-match (tag1 tag2)
  (let ((t1 (if (consp tag1) tag1 (tag-path tag1)))
        (t2 (if (consp tag2) tag2 (tag-path tag2))))
    (and
     (equal (butlast t1) (butlast t2))
     (string= (first (cl-ppcre:all-matches-as-strings *node-scanner* (car (last t1))))
              (first (cl-ppcre:all-matches-as-strings *node-scanner* (car (last t2))))))))

(defun path= (x y)
  (equal x y))

(defun path> (x y)
  (aif (= (length x) (length y))
       (string> (apply #'concatenate 'string x)
                (apply #'concatenate 'string y))
       (> (length x) (length y))))

(defun path< (x y)
  (aif (= (length x) (length y))
       (string< (apply #'concatenate 'string x)
                (apply #'concatenate 'string y))
       (< (length x) (length y))))

(defun make-node-finder (&key (class nil) (node nil) (attval nil) (id nil) (debug nil))
  (let ((class-matcher
          (if class
              (mapcar #'(lambda (x)
                          (let ((scanner
                                 (cl-ppcre:create-scanner
                                  (format nil "\\b~a\\b" x) :case-insensitive-mode t)))
                            #'(lambda (y)
                                (cl-ppcre:all-matches-as-strings
                                 scanner
                                 y))))
                      (if (consp class)
                          class
                          (list class)))
              nil)))
    #'(lambda (x)
        (when debug (print x))
        (and (tag-p x)
             (or (not node)
                 (tag-node-p x node))
             (or (not class)
                 (not
                  (remove-if #'(lambda (y) (funcall y (tag-class x)))
                             class-matcher)))
             (or (not id)
                 (string= "" id)
                 (string-equal (tag-id x) id))
             (or (not attval)
                 (if (stringp (cdr attval))
                     (string-equal (tag-elm x (car attval)) (cdr attval))
                     (funcall (cdr attval) (tag-elm x (car attval)))))))))

(defun tag-node-p (x node)
  (if (stringp node)
      (string= (tag-node x) node)
      (string= (string-upcase (tag-node x)) (symbol-name node))))

(defun tag-belongs-p (tag bag)
  (member tag bag :test #'tag-node-p))

(defun tag-html (html)
  (chtml:parse html (cxml-stp:make-builder)))

(defun tag-contains (tag node)
  (> (length (ext-html tag :node node)) 1))

(defun tag-contains-only (tag node)
  (= (length (ext-html tag :node node)) (length (stp:list-children tag))))

(defmacro with-tags ((var html) &body body)
  `(simple-bind (document (if (stringp ,html) (tag-html ,html) ,html))
     (stp:do-recursively (,var document)
       ,@body)))

(defmacro with-tags/doc ((doc tag html) &body body)
  `(simple-bind (,doc (if (stringp ,html) (tag-html ,html) ,html))
     (stp:do-recursively (,tag ,doc)
       ,@body)
     ,doc))

(defmacro with-tags/document ((var html) &body body)
  `(with-tags/doc (document ,var ,html)
     ,@body))

(defun ext-html (html &key (class nil) (node nil) (attval nil) (id nil) (debug nil) (att nil))
  (let ((acc '())
        (node-finder (make-node-finder :class class :node node :attval attval :id id :debug debug)))
    (with-tags (a html)
      (when (funcall node-finder a)
        (push a acc)))
    (nreverse
     (if (not att)
         acc
         (mapcar #'(lambda (y) (tag-elm y att)) acc)))))

(defun tag->html (tag-tree)
  (cxml-stp:serialize tag-tree (cxml:make-character-stream-sink *standard-output*)))

(defscan *location-scanner*
    "([a-z0-9]+)#?([a-zA-Z0-9\\-_]*)\\.?([a-zA-Z0-9\\-_ ]*):?([0-9]*)")

(defun tag-recurse-if (fn pred node)
  (labels ((trec (child)
             (if (and (tag-p child) (funcall pred child))
                 (funcall fn child)
                 (stp:do-children (c child)
                   (trec c)))))
    (trec node))
  node)

(defun tag-recurse-bu (fn pred node)
  (labels ((trec (child parent)
             (when (stp:first-child child)
               (stp:do-children (c child)
                 (trec c child)))
             (when (and (tag-p child) (funcall pred child))
               (funcall fn child parent))))
    (let ((tagged (if (stringp node)
                      (tag-html node)
                      node)))
      (trec tagged nil)
      tagged)))

(defun tag-recurse-child (fn pred html)
  (with-tags/doc (doc parent html)
    (when (tag-p parent)
      (dolist (child (stp:list-children parent))
        (when (and (tag-p child)
                   (funcall pred child))
          (funcall fn child parent))))))

(defun tag-remove-child (html pred)
  (tag-recurse-child #'stp:delete-child pred html))

(defun tag-all-tokens (tag &optional (no-nodes nil))
  (tokenize-html tag no-nodes))

(defun tokenize-html (html &optional (no-nodes nil))
  (let ((acc '()))
    (with-tags (elm html)
      (push (tag-token elm :no-nodes no-nodes) acc))
    (remove-if #'not (flatten (nreverse acc)))))

(defun tag-empty-p (tag)
  (if (null tag)
      t
      (if (null (stp:list-children tag))
          (null (car (remove-symbols (tokenize-html tag))))
          (= 0 (length (stp:filter-children (compose #'not #'tag-empty-p) tag))))))

(defmacro tag-recurse-expander (lst html)
  (reduce #'(lambda (x y)
              `(tag-recurse-child
                #'(lambda (child parent) (declare (ignorable parent)) (tag-add-class child ,(cdr y)))
                ,(if (consp (car y))
                     `#'(lambda (child) ,(car y))
                     `#'(lambda (child) (tag-belongs-p child ,(car y))))
                ,x))
          `,lst
          :initial-value html))

(defun tag-structure (tag)
  (let ((acc '()))
    (with-tags (ti tag)
      (when (tag-p ti)
        (push (tag-node ti) acc)))
    (nreverse acc)))

(defmemoize tag->tf 'equal (tag no-nodes)
  (tokens->tf (remove-if #'stop-word-p (tag-all-tokens tag no-nodes))))

(defun tags-same-p (tag1 tag2 &key (threshold 0.95))
  (>
   (tag-sameness tag1 tag2)
   threshold))

(defun tag-sameness (tag1 tag2)
  (let ((tf1 (tag->tf tag1 t))
        (tf2 (tag->tf tag2 t)))
    (if (and (hash-empty? tf1) (hash-empty? tf2))
        1.0
        (if (or (hash-empty? tf1) (hash-empty? tf2))
            0.0
            (cosine
             (tag->tf tag1 t)
             (tag->tf tag2 t))))))

(defun tag-has-class (tag class)
  (awhen (tag-class tag)
    (cl-ppcre:all-matches class it)))

(defun tag-collect-tokens (tag)
  (tag-recurse-if
   #'(lambda (t1) (tag-token t1 :no-nodes t))
   #'(lambda (t1) (and (tag-p t1)
                       (or (not (tag-class t1))
                           (tag-has-class t1 "tbnoncontent"))))
   tag))

(defun tag-class-belongs-p (tag classes)
  (first
    (intersection (make-word-list (tag-class tag))
                  (mapcar #'string-downcase classes)
                  :test #'string=)))

(defun loc->split (loc)
  (cl-ppcre:register-groups-bind (node id classes n)
      (*location-scanner* loc)
    (values node id (split-text classes) n)))

(defun ext-by-nth (html path n)
  (stp:nth-child n (first (ext-by-path html (if (is-text-path path)
                                                (butlast path)
                                                path)))))

(defun remove-tb (classes)
  (remove-if #'(lambda (class) (cl-ppcre:all-matches "^tb[a-z]+" class)) classes))

(defun tag-loc (tag)
  (let ((l '()))
    (mapcar #'(lambda (loc)
                (multiple-value-bind (node id classes n)
                    (loc->split loc)
                  (declare (ignorable n))
                  (push node l)
                  (unless (string= "" id)
                    (push (s+ "#" id) l))
                  (awhen (remove-tb classes)
                    (dolist (class it)
                      (push "." l)
                      (push class l)))
                  (push ">" l)))
            (tag-path tag '() t))
    (join (butlast (nreverse l)) "")))

(defun is-text-path (path)
  (cl-ppcre:all-matches "text:[0-9]+" (clast path)))

(defun loc-n (loc)
  (if (or (not loc) (string= loc ""))
      0
      (multiple-value-bind (1stnode 1stid 1stclasses 1stnstr)
          (loc->split loc)
        (parse-integer 1stnstr :junk-allowed t))))

(defun loc-node (loc)
  (multiple-value-bind (1stnode 1stid 1stclasses 1stnstr)
      (loc->split loc)
    1stnode))

(defun all-ext-by-path (html path)
  (multiple-value-bind (node id classes pos)
      (loc->split (clast path))
    (remove-if-not #'(lambda (n)
		       (when *debug-extracter* (print (tag-path n nil nil)))
		       (equal (butlast (tag-path n nil nil)) (butlast (clean-up-path path))))
		   (ext-html html :node node :id id :class classes))))

(defun tag-path-equal (tp1 tp2)
  (or (equal tp1 tp2)
      (equal (butlast (clean-up-path (append tp1 '("a"))))
	     (butlast (clean-up-path (append tp2 '("a")))))))

(defun ext-by-pos (html path)
  "makes sure that the entire path is the same."
  (let ((istext (is-text-path path)))
    (awhen
	(remove-if-not
	 #'(lambda (tag)
	     (if istext
		 (and (tag-path-equal (butlast path) (tag-path tag nil t))
		      (equal (clast (butlast path)) (clast (tag-path tag nil t))))
		 (equal path (tag-path tag nil t))))
	 (multiple-value-bind (node id classes pos)
	     (if istext
		 (loc->split (clast (butlast path)))
		 (loc->split (clast path)))
	   (ext-html html :node node :id id :class classes)))
      (if (not istext)
	  it
	  (multiple-value-bind (node id classes pos)
	      (loc->split (clast path))
	    (mapcar #'(lambda (tag) (stp:nth-child (parse-integer pos) tag))
		    it))))))

(defun ext-by-pos-loop (html path common-path)
  "finds all tags that match the path without position
   and then finds the one where the uncommon part is positionally same."
  (let ((istext (is-text-path path)))
    (remove-if-not
     #'(lambda (tag)
	 (when (tag-path-equal (butlast path) (tag-path tag nil nil))
	   (equal (if istext
		      (butlast (nthcdr (length common-path) path))
		      (nthcdr (length common-path) path))
		  (nthcdr (length common-path) (tag-path tag nil t)))))
     (multiple-value-bind (node id classes pos)
	 (if istext
	     (loc->split (clast (butlast path)))
	     (loc->split (clast path)))
       (ext-html html :node node :id id :class classes)))))

(defun ext-by-path (html path &key (full t) (debug nil))
  (let ((istext (is-text-path path)))
    (when *debug-extracter* (print (list 'ext (clean-up-path (butlast path)) (tag-path html nil nil))))
    (awhen (remove-if-not
	    #'(lambda (tag)
		(if istext
		    (if full
			(tag-path-equal (butlast path) (tag-path tag nil full))
			(progn
			  (when debug
			    (print (list 'tag (tag-path tag nil full))))
			  (tag-path-equal (clean-up-path (butlast path)) (tag-path tag nil full))))
		    (if full
			(equal path (tag-path tag nil full))
			(equal (clean-up-path path) (tag-path tag nil full)))))
            (multiple-value-bind (node id classes pos)
                (if istext
                    (loc->split (clast (butlast path)))
                    (loc->split (clast path)))
              (ext-html html :node node :id id :class classes)))
      (if istext
	  (progn
	    (when *debug-extracter* (print (list '0found it)))
	    (mapcar #'(lambda (elm)
			(when (stp:list-children elm)
			  (stp:nth-child (loc-n (clast path)) elm)))
		    it))
          it))))

(defun clean-up-path (path)
  (append
   (mapcar #'(lambda (p) 
	      (let ((l '()))
		(multiple-value-bind (node id classes n)
		    (loc->split p)
		  (declare (ignorable n))
		  (push node l)
                  (unless (string= "" id)
                    (push (s+ "#" id) l))
                  (awhen (remove-tb classes)
		    (push "." l)
		    (push (first it) l)
                    (dolist (class (rest it))
		      (push " " l)
                      (push class l)))
		  (join (nreverse l) ""))))
	   (butlast path))
   (list (clast path))))

(defun ext-by-loc (html loc)
  (let ((node (loc->split (clast (cl-ppcre:split ">" loc)))))
    (remove-if-not
     #'(lambda (tag)
         (string= loc (tag-loc tag)))
     (ext-html html :node node))))

(defun ext-by-loc-struct (loc struct html)
  (when-bind (sim (ext-by-loc html loc))
    (remove-if-not #'(lambda (tag)
                       (equal struct
                              (tag-structure tag)))
                   sim)))

(defun tag-tokens (tag &key (no-atts nil))
  (let ((acc '()))
    (with-tags (t1 tag)
      (if (tag-p t1)
          (progn
            (push (s+ ":" (tag-node t1)) acc)
            (when (and (not no-atts) (first (stp:list-attributes t1)))
              (stp:map-attributes 'list
                                  #'(lambda (att)
                                      (push (s+ ":" (stp:local-name att)) acc)
                                      (dolist (word (word-punc-splitter (stp:value att)))
                                        (push word acc))
                                      t)
                                  t1)))
          (if (eql (type-of t1) 'stp:text)
              (dolist (word (word-punc-splitter (stp:data t1)))
                (push word acc))
              (push (s+ ":" (stp:local-name t1)) acc))))
    (remove-empty (nreverse (flatten acc)))))

(defun tag-tokenized (tag)
  (mup (mappend #'split-text (tag-tokens tag))))

(defun tag-tokens->tf (tag &key (fn #'tag-tokenized))
  (tokens->features (funcall fn tag)))

(defun tag-noscript-tokens (tag)
  (mup (mappend #'(lambda (x) (if (symbolp x) nil (split-text x))) (tokenize-html tag))))

(defun tag-remove-tb-classes (tag)
  (tag-remove-class-if tag #'(lambda (class) (cl-ppcre:all-matches "^tb[a-z]+" class))))

(defun content-tag-empty-p (tag)
  (and (tag-has-class tag "tbcontent")
       (tag-definitely-empty-p tag)))

(defun done-key (n tag)
  (cons n (tag-path tag '() t)))

(defun tag-tokens-4-template (tag)
  (let ((acc '()))
    (with-tags (t1 tag)
      (if (tag-p t1)
          (push (s+ ":" (tag-node t1)) acc)
          (if (eql (type-of t1) 'stp:text)
              (push (stp:data t1) acc)
              (push (s+ ":" (stp:local-name t1)) acc))))
    (join (remove-empty-string (nreverse (flatten acc))) " ")))

(defun page-text (page)
  (let ((text '()))
    (with-tags (tag page)
      (when (eql (type-of tag) 'stp:text)
        (push (trim (stp:data tag)) text)))
    (nreverse text)))

(defun page-text/tokens (page)
  (let ((text '()))
    (with-tags (tag (prune-fluff page))
      (if (eql (type-of tag) 'stp:text)
          (push (trim (stp:data tag)) text)
          (if (eql (type-of tag) 'stp:element)
              (push (make-keyword (tag-node tag)) text))))
    (nreverse text)))

(defun prune-html (html node-bag)
  (tag-remove-child
   (tag-remove-child html
                     #'(lambda (child) (tag-belongs-p child node-bag)))
   #'(lambda (tag) (and (tag-empty-p tag)
                        (not (tag-belongs-p tag *elements-can-be-empty*))))))

(defun prune-tag (html pred)
  (tag-recurse-bu
   #'stp:delete-child
   #'(lambda (tag)
       (and (tag-p tag)
            (or
             (tag-definitely-empty-p tag)
             (funcall pred tag))))
   html))

(defun tag-definitely-empty-p (tag)
  (and (tag-empty-p tag)
       (not (tag-belongs-p tag *elements-can-be-empty*))))

(defun prune-fluff (html)
  (prune-html html *definitely-non-content-block-elements*))
