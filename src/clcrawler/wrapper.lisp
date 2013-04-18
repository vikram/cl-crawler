(in-package :com.crawler)

(defun loop-first (q)
  (second (second q)))

(defun loop-next (q)
  (second (third q)))

(defun template-maker (example)
  `((,(make-logic-symbol (symbol-name (record-name example))) (html)
      (list-record
       ,(record-name example)
       ,@(template-maker-record-fields example)))
    ,@(template-maker-record-field-fns example)))

(defun list-record (record &rest elements)
  `(:record ,record ,@(remove-empty elements)))

(defun list-loop (loop &rest elements)
  `(,loop ,@(remove-empty elements)))

(defmacro sub-text (test text-expr extracter record)
  (with-gensyms (results)
    `(let ((,results '()))
       (when (funcall #',test ,text-expr)
         (dolist (value (funcall #',extracter ,text-expr))
           (push ,record ,results)))
       (nreverse ,results))))

(defun simple-collected-elements (html start end pos)
  (let ((collect '())
        (ended nil)
        (started nil))
    (when *debug-extracter*
      (print (list start end pos)))
    (dolist (tag (nthcdr pos (stp:list-children html)))
      (when (string= (tag-node tag) start)
        (setf started t))
      (when (and started
                 (string= (tag-node tag) end))
        (setf ended t))
      (when (and started (not ended))
        (push tag collect))
      (when ended
        (return (nreverse collect))))))

(defmacro loop-for (page test &body body)
  (with-gensyms (len results)
    `(let* ((pos 0)
            (html ,page)
            (,len (length (stp:list-children html)))
            (,results '()))
       (while (> ,len (1+ pos))
         (let ((current (stp:nth-child pos html)))
           (if (funcall #',test current)
               (push ,@body ,results)))
         (incf pos))
       (nreverse ,results))))

(defmacro move (i)
  `(progn
     (incf pos ,i)
     (setf current (stp:nth-child pos html))))

(defmacro skip-nodes (&rest nodes)
  (with-gensyms (test-nodes i)
    `(let ((,i 0))
       (dolist (,test-nodes (groupn-consec (nthcdr pos (stp:list-children html)) ,(length nodes)))
         (incf ,i)
         (when (equal (mapcar #'tag-node ,test-nodes)
                      ',nodes)
           (incf pos (+ ,i ,(length nodes) -1)) ;-1 because we count the current node already
           (setf current (stp:nth-child pos html))
           (return))))))

(defmacro sub-loop (test &body body)
  (with-gensyms (results)
    `(let ((,results '()))
       (while (funcall #',test current)
         (push ,@body ,results))
       (nreverse ,results))))

(defun template-text (elm &key start end skip grab)
  (if (or (not elm) (atom elm) (not (remove-empty elm)))
      ""
      (if (and start end)
	  (let* ((e (if (listp elm)
			(first elm)
			elm))
		 (text (if (eql (type-of e) 'stp:text)
			   e
			   (find 'stp:text (stp:list-children e) :key #'type-of)))
		 (str (if text
			  (word-punc-splitter (trim (stp:data text)))
			  '())))
	    (if str
		(join
		 (if grab
		     (subseq
		      (if skip
			  (nthcdr skip str)
			  str)
		      0
		      grab)
		     (ignore-tokens
		      (skip-tokens str
				   (if start
				       (word-punc-splitter start)))
		      (if end
			  (word-punc-splitter end))))
		 " ")
		""))
	  (awhen (mapcan #'page-text elm)
	    (print it)
	    (first (remove-empty-string it))))))

(defun skip-tokens (tokens skips)
  (if skips
      (aif (position skips (groupn-consec tokens (length skips)) :test #'equal)
           (subseq tokens (+ it (length skips)))
           tokens)
      tokens))

(defun ignore-tokens (tokens ignores)
  (if ignores
      (aif (position ignores (groupn-consec tokens (length ignores)) :test #'equal)
           (subseq tokens 0 it)
           tokens)
      tokens))

(defun showcase-wrapper (page)
  (labels ((cinema (html)
             (list-record
                   :cinema
                   (list :name (cinema-name html))
                   (list :phone (cinema-phone html))
                   (list-loop :films (cinema-films html))))
           (cinema-name (html)
              (template-text
               (ext-by-path html '("html:0" "head:0" "title:0" "text:0") :full t)
               :start nil
               :end "-"))
           (cinema-phone (html)
              (template-text
               (ext-by-path html '("html:0" "body:1" "center:2" "font:0" "text:2") :full t)
               :start "-"
               :end nil))
           (cinema-films (html)
             (loop-for (first (ext-by-path html '("html:0" "body:1" "center:2" "div.film:2") :full t))
                   film-p
                   (list-record :film
                         (list :name (film-name (move 0))
                         (list :rating (film-rating (move 1)))
                         (list :duration (film-duration (move 0))))
                         (skip-nodes "br")
                         (list-loop :days
                                    (sub-loop
                                        day-listing-p
                                      (list-record :day-listing
                                                   (list :day (day-listing-day (move 0)))
                                                   (list :date (day-listing-date (move 0)))
                                                   (list-loop :times
                                                              (sub-text
                                                                  times-p
                                                                  (times (move 0))
                                                                times-time
                                                                (list-record :time (list :time value))))
                                                   (skip-nodes "br")))))))
           (film-p (html)
             (parent/child-path= html '("html" "body" "center" "div" "b" "text")))
           (film-name (html)
             (template-text html :start nil :end nil))
           (film-rating (html)
             (template-text
              html
              :start "Cert"
              :end ","))
           (film-duration (html)
             (template-text
              html
              :start ","
              :end nil))
           (day-listing-p (html)
             (apply-pattern :day (day-listing-day html)))
           (day-listing-day (html)
             (template-text
              html
              :grab 1))
           (day-listing-date (html)
             (template-text
              html
              :skip 1
              :grab 2))
           (times-p (text)
             (times-time text))
           (times (html)
             (awhen (template-text
                     html
                     :start "-"
                     :end ".")
               it))
           (times-time (text)
             (apply-pattern :time text)))
    (cinema page)))

(defun parent/child-path= (tag path)
  (or
   (equal (tag-path-text tag) path)
   (stp:do-recursively (node tag)
     (when (equal (tag-path-text node) path)
       (print 'found-it)
       (return-from parent/child-path= t)))))

(defmacro deftemplate (name example url)
  (with-gensyms (page)
    `(defun ,(make-logic-symbol (s+ (symbol-name name) "-wrapper")) (,page)
       (labels (,@(template-maker (learn-template url example)))
         (,(make-logic-symbol (symbol-name (record-name example))) ,page)))))

(defun template-maker-record-fields (record &key (inside-loop nil))
  (let ((i 0))
    (mapcar #'(lambda (field)
              (incf i)
              (if (eql (first field) :loop)
                  `(list-loop ,(make-keyword (s+ (symbol-name (second (second (second field))))
                                                 "s"))
                              (,(make-logic-symbol (s+ (symbol-name (record-name record))
                                                       "-"
                                                       (symbol-name (second (second (second field))))
                                                       "s"))
                                html))
                  `(list ,(first field)
                         (,(make-logic-symbol (s+ (symbol-name (record-name record))
                                                  "-"
                                                  (symbol-name (first field))))
                           ,(if inside-loop
                                'current
                                'html)))))
          (record-fields record))))

;(nth (1- i) inside-loop)

(defun position-analysis (fields-in-loop)
  `((move 0)
    ,@(mapcar
         #'(lambda (two-fields)
             (if (equal (second two-fields) '(:loop . :same))
                 '(move 0)
                 (let* ((1st (first two-fields))
                        (2nd (second two-fields))
                        (1st-1st (append (third 1st) (fourth 1st)))
                        (1st-2nd (append (third 1st) (fifth 1st)))
                        (2nd-1st (append (third 2nd) (fourth 2nd)))
                        (2nd-2nd (append (third 2nd) (fifth 2nd)))
                        (diff1 (template-path-diff 1st-1st  2nd-1st nil))
                        (diff2 (template-path-diff 1st-2nd 2nd-2nd nil)))
                   (if (= (fifth diff1) (fifth diff2))
                       `(move ,(fifth diff1))
                       `(move :until ,diff1)))))
         (groupn-consec fields-in-loop 2))))

(defun template-maker-record-field-fns (record &key (inside-loop nil))
  (mappend #'(lambda (field)
              (if (eql (first field) :loop)
                  `((,(make-logic-symbol (s+ (symbol-name (record-name record))
                                            "-"
                                            (symbol-name (second (second (second field))))
                                            "s"))
                     (html)
                     (loop-for (first (ext-by-path
                                       html
                                       ',(common
                                        (first (third (third (loop-first field))))
                                        (first (third (third (loop-next field))))
                                        :test #'string=)
                                       :full nil))
                        ,(make-logic-symbol (s+ (symbol-name (second (second (second field)))) "-p"))
                        (list-record ,(second (loop-first field))
					      ,@(template-maker-record-fields
						 (loop-first field)
						 :inside-loop (position-analysis (fourth field))))))
                    (,(make-logic-symbol (s+ (symbol-name (second (second (second field)))) "-p")) (html)
                      (parent/child-path= html
                             ',(if (eql :diff (second (first (fourth field))))
                                  (append
                                   (mapcar #'loc-node (third (first (fourth field))))
                                   (common
                                    (mapcar #'loc-node (fourth (first (fourth field))))
                                    (mapcar #'loc-node (fifth (first (fourth field))))
                                    :test #'string=)))))
                     ,@(template-maker-record-field-fns (loop-first field) 
							:inside-loop 
							(list (first (third (third (loop-first field))))
							      (first (third (third (loop-next field)))))))
                  (multiple-value-bind (start end)
                      (template-start-end (second (third field)))
                    `((,(make-logic-symbol (s+ (symbol-name (record-name record))
                                              "-"
                                              (symbol-name (first field))))
                       (html)
			(template-text
			 ,(insider inside-loop field)
			 :start ,start
			 :end ,end))))))
          (record-fields record)))

;              (string= (tag-node html) "b"))
;                      ,(third (second field))
;                     ,(third (third field))


; ,(template-maker (second (second field)))))
;      (let* ((l-com (length (common (first inside-loop) (second inside-loop) :test #'string=)))
;	     (f-ext (nthcdr l-com (first inside-loop)))
;	     (n-ext (nth l-com (second inside-loop))))

(defun insider (inside-loop field)
  (if inside-loop
      `(let ((e (ext-by-path html ',(first (third field)) :full nil)))
	 (print (list ,(first field) (page-text html)))
	 (print (list ,(first field) (tag-path html nil t)))
	 e)
      `(ext-by-path html ',(first (third field)) :full nil)))

