(in-package :com.crawler)

(defparameter *patterns*
  `((:time . ,(cl-ppcre:create-scanner "[0-9]?[0-9] : [0-9][0-9]"))
    (:num . ,(cl-ppcre:create-scanner "[0-9]+"))
    (:mins . ,(cl-ppcre:create-scanner "[0-9]+ mins" :case-insensitive-mode t))
    (:date . ,(cl-ppcre:create-scanner "[0-9]+[ ]?[a-z]{0,2} (jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]+" :case-insensitive-mode t))
    (:day . ,(cl-ppcre:create-scanner "(monday|tuesday|wednesday|thursday|friday|saturday|sunday)" :case-insensitive-mode t))
    (:small-day . ,(cl-ppcre:create-scanner "(mo|tu|we|th|fr|sa|su|mon|tue|wed|thu|fri|sat|sun)" :case-insensitive-mode t))))

(defun from-example (url example &key (extracter #'elmwrapper) (parser (find-example-parser :extracter extracter)))
  (let ((page (fetch-while-empty url (make-hash-code url))))
    (when page
      (parse-example example (prune-fluff page) :parser parser))))

(defun parse-example (q page &key (parser (find-example-parser)))
  (if (not q)
      nil
      (if (eg-many-p q)
          (parser-function :loop q page parser)
          (if (eg-field-p q)
              (parser-function :field q page parser)
              (if (eg-record-p q)
                  (parser-function :record q page parser)
                  (if (eg-fields-p q)
                      (parser-function :fields q page parser)
                      (cons :error q)))))))

(defun record-name (q)
  (second q))

(defun find-example-parser (&key (extracter #'elmwrapper))
  #'(lambda (type)
      (case type
        (:extracter extracter)
        (:loop #'parse-loop)
        (:field #'parse-field)
        (:record #'parse-record)
        (:fields #'parse-fields))))

(defun parser-function (type q page parser)
  (funcall (funcall parser type) q page parser))

(defun parse-fields (q page parser)
  (remove-empty
   (mapcar #'(lambda (x)
               (unless (equal '(:as-above) x)
                 (parse-example x page :parser parser)))
           q)))

(defun record-fields (q)
  (rest (rest q)))

(defun parse-record (q page parser)
  (append (list :record (second q))
          (parse-example (record-fields q) page :parser parser)))

(defun parse-field (q page parser)
  (list (first q) (second q) (parser-function :extracter (rest q) page parser)))

(defun parse-loop (q page parser)
  (list :loop
        (list :first (parse-example (loop-first q) page :parser parser))
        (list :next (parse-example (loop-next q) page :parser parser))))

(defun make-elmtext ()
  (let ((templates '())
        (visited '()))
    #'(lambda (q page parser)
        (awhen (elmtext q page templates visited)
          (unless (equal (first visited) (first it))
            (push (first it) visited))
          (push it templates)
          it))))

(defun eg-pattern-p (q)
  (and (symbolp (first q))
       (symbolp (second q))
       (eql :pattern (first q))))

(defun eg-record-p (q)
  (and (symbolp (first q))
       (symbolp (second q))))

(defun eg-field-p (q)
  (and (listp q)
       (symbolp (first q))
       (or (stringp (second q))
           (and
            (listp (second q))
            (eql :pattern (car (second q)))))
       (or (null (third q))
           (simple-listp (first (third q)))
           (stringp (second (third q))))))

(defun eg-fields-p (q)
  (and (listp q)
       (null (remove-if #'listp q))))

(defun eg-many-p (q)
  (eql (first q) :loop))

(defun apply-pattern (pattern text)
  (cl-ppcre:all-matches-as-strings (cdr (assoc pattern *patterns*)) text))

(defun elmtext (q page templates visited)
  (let ((scanner (if (stringp (first q))
                     (cl-ppcre:create-scanner (first q))
                     (cl-ppcre:create-scanner (third (first q)))))
        (past nil))
    (with-tags (text page)
      (when (equal (tag-path text nil t) (first visited))
        (setf past t))
      (when past
        (when (eql (type-of text) 'stp:text)
          (let ((data (trim (stp:data text))))
            (awhen (cl-ppcre:all-matches scanner (trim (stp:data text)))
              (when-bind (match (find (tag-path text) templates :test #'equal :key #'car))
                (when-bind (real-match (cl-ppcre:all-matches scanner (second match)))
                  (setf it real-match)
                  (setf data (second match))))
              (return (list (tag-path text nil t)
                            (template-pattern it data))))))))))

(defun template-id (id)
  (format nil "~A" (code-char (+ 190 (incf id)))))

(let ((i 0))
  (defun get-next-template-id ()
    (when (> i 48)
      (setf i 0))
    (template-id (incf i))))

(defscan *template-scanner*
    (list :sequence
          (list :GREEDY-REPETITION 1 NIL `(:char-class ,@(map1-n #'(lambda (i) (code-char (+ 190 i))) 50)))))

(defun ext-template (template str)
  (awhen (cl-ppcre:all-matches *template-scanner* template)
    (subseq str
            (first it)
            (+ (- (length str) (length template)) (second it)))))

(defun template-pattern (matches str)
  (let ((len (length str)))
    (if (= len (- (second matches) (first matches)))
        (get-next-template-id)
        (if (= 2 (length matches))
            (join (list (subseq str 0 (first matches))
                        (get-next-template-id)
                        (subseq str (second matches)))
                  "")
            (replace-chain
             str
             (mapcar
              #'(lambda (x) (cons (subseq str (first x) (second x)) (get-next-template-id)))
              (groupn matches 2)))))))

(defun elmwrapper (q page parser)
  (ext-template (second (second q))
                (trim
                 (stp:data
                  (stp:nth-child
                   (parse-integer (fourth (multiple-value-list (loc->split (clast (first (second q)))))))
                   (first (ext-by-path page (butlast (first (second q))))))))))

(defun learn-example-parser (&key (extracter (make-elmtext)))
  #'(lambda (type)
      (case type
        (:extracter extracter)
        (:field #'parse-field)
        (:record #'parse-record)
        (:fields #'parse-fields)
        (:loop #'(lambda (q page parser)
                   (let ((first (parse-example (loop-first q) page :parser parser))
                         (next (parse-example (loop-next q) page :parser parser)))
                     (list :loop
                           (list :first first (analyze-record-fields first page))
                           (list :next next (analyze-record-fields next page))
                           (analyze-loop first next page))))))))

(defun template-path (q)
  (first (third q)))

(defun fieldname (q)
  (first q))

(defun remove-loop (lst)
  (remove-if #'(lambda (x) (eql :loop (first x))) lst))

(defun analyze-record-fields (record page)
  (let ((paths (mapcar #'template-path (remove-loop (record-fields record)))))
    (if (same paths :test #'equal)
        `(:same ,(first paths))
        (mapcar #'(lambda (x)
                    (list (first (first x)) (first (second x))
                          (template-path-analysis (first x) (second x) page)))
                (groupn-consec (remove-loop (record-fields record)) 2)))))

(defun analyze-loop (first next page)
  (let ((first-fields (record-fields first))
        (next-fields (record-fields next))
        (results '()))
    (dolist (first-field first-fields)
      (let ((next-field (find (fieldname first-field) next-fields :key #'fieldname)))
        (if (not next-field)
            (push (cons (fieldname first-field) :same) results)
            (push (cons (fieldname first-field)
                        (template-path-analysis first-field
                                                next-field
                                                page))
                  results))))
    (nreverse results)))

(defun template= (t1 t2)
  (equal t1 t2))

(defun learn-template (url example)
  (let ((init (from-example url example :extracter (make-elmtext))))
    (from-example url
                  init
                  :parser (learn-example-parser))))

(defun template-pos (tokens)
  (let ((i 0))
    (dolist (token (mapcar #'template-scanner tokens))
      (incf i)
      (when token
          (return i)))))

(defun template-path-diff (1st 2nd page)
  (let* ((common (common 1st 2nd :test #'string=))
         (1st-rest (subseq 1st (length common)))
         (1stn (loc-n (first 1st-rest)))
         (2nd-rest (subseq 2nd (length common)))
         (2ndn (loc-n (first 2nd-rest))))
    `(:diff
      ,common
      ,1st-rest
      ,2nd-rest
      ,(- 2ndn 1stn))))

;      ,(if page
;           (map1-n #'(lambda (n)
;                       (ext-by-nth page common (+ 1stn n)))
;                   (- 2ndn 1stn))
;           (- 2ndn 1stn)))))

(defun template-path-analysis (1st-whole 2nd-whole page)
  (let ((1st (template-path 1st-whole))
        (2nd (template-path 2nd-whole)))
    (if (equal 1st 2nd)
        (list :same
              (second 1st-whole)
              (template-description 1st-whole 2nd-whole))
        (template-path-diff 1st 2nd page))))

(defun template-description (1st 2nd)
  (if (and 1st 2nd)
      (- (template-pos (whitespace-splitter (second (third 2nd))))
	 (template-pos (whitespace-splitter (second (third 1st)))))
      0))

(defun template-start-end (str)
  (let ((answers (aif (cl-ppcre:all-matches *template-scanner* str)
                      (list (trim (subseq str 0 (first it)))
                            (trim (subseq str (second it))))
                      (list "" ""))))
    (values (if (string= "" (first answers))
                nil
                (clast (word-punc-splitter (first answers))))
            (if (string= "" (second answers))
                nil
                (first (word-punc-splitter (second answers)))))))
