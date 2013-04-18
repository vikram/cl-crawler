(in-package :com.crawler)

(defun html->list (html)
  (and html (stringp html) (net.html.parser:parse-html html)))

(defun-exported extract (html tag)
  (if (not html)
      nil
      (if (atom (car html))
          (if (eql tag (car html))
              html
              (extract (cdr html) tag))
          (or (extract (car html) tag)
              (extract (cdr html) tag)))))

(defun-exported extract-lookahead (html test &key (test-child #'identity))
  (labels ((rec (tree acc)
             (cond ((null tree) acc)
                   ((consp (car tree))
                    (rec (cdr tree)
                         (rec (car tree)
                              (if (and (funcall test (car tree)) (funcall test-child tree))
                                  (cons tree acc)
                                  acc))))
                   (t (rec (cdr tree)
                           acc)))))
    (rec html '())))

(defun-exported extract-all (tree test)
  (labels ((rec (tree acc)
             (cond ((null tree) acc)
                   ((consp (car tree)) (rec (cdr tree)
                                            (nconc (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               (cons tree acc)
                               acc))))))
    (rec tree '())))

(defun-exported extract-lookahead-all (tree test)
  (labels ((rec (tree acc)
             (cond ((null tree) acc)
                   ((consp (car tree)) (if (and (simple-listp (car tree)) (funcall test (caar tree)))
                                           (cons tree acc)
                                           (rec (cdr tree)
                                                (nconc (rec (car tree) nil) acc))))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               (cons tree acc)
                               acc))))))
    (rec tree '())))

(defscan *relative-url* "(~/|/|\\./)([-A-Za-z0-9_\\$\\.\\+\\!\\*\\(\\),;:@&=\\?/~\\#\\%]|\\\\)+")

(defscan *absolute-url*
    "([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}|(((news|telnet|nttp|file|http|ftp|https)://)|(www|ftp)[-A-Za-z0-9]*\\.)[-A-Za-z0-9\\.]+)(:[0-9]*)?/[-A-Za-z0-9_\\$\\.\\+\\!\\*\\(\\),;:@&=\\?/~\\#\\%]*[^]'\\.}>\\),\\\"]")

(defscan *hostname*
  "([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}|(((news|telnet|nttp|file|http|ftp|https)://)|(www|ftp)[-A-Za-z0-9]*\\.)[-A-Za-z0-9\\.]+)")

(defscan *domain*
  "([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}|((www|ftp|)[-A-Za-z0-9]*\\.)[-A-Za-z0-9\\.]+)(:[0-9]*)?")

(defscan *protocol*
  "((news|telnet|nttp|file|http|ftp|https)://)")

(defscan *js-url*
    "js$")

(defscan *txt-url*
    "txt$")

(defscan *img-url*
    "(png|gif|tiff|bmp)$")

(defscan *css-url*
    "css$")

(defscan *file-extension*
    "\\.[a-z]+$")

(defun generate-urls (hostname links
                      &key
                      (url hostname)
                      (test (let ((host (cl-ppcre:create-scanner hostname)))
                              #'(lambda (link)
                                  (or (not (cl-ppcre:all-matches "http://" link))
                                      (cl-ppcre:all-matches host link))))))
  (remove-if-not
   #'(lambda (link) (and link (funcall test link)))
   (mapcar #'(lambda (ahref)
               (let ((link (or (awhen (member :href ahref)
                                 (second it))
                               (awhen (member :src ahref)
                                 (second it)))))
                 (normalized-link url link)))
           links)))

(defun-exported normalized-link (hostname link)
  (if (cl-ppcre:all-matches *hostname* link)
      (place-holder-in-page link)
      (when (not (zerop (length link)))
        (if (string= (subseq link 0 1) "/")
            (format nil "~A~A" (first (hostname hostname)) link)
            (if (string= (subseq hostname (1- (length hostname))) "/")
                (format nil "~A~A" hostname link)
                (format nil "~A/~A" hostname link))))))

(defun place-holder-in-page (url)
  (aif (cl-ppcre:all-matches "/#[a-zA-Z0-9_]*$" url)
       (subseq url 0 (first it))
       url))

(defun escape-url (url)
  (cl-ppcre:regex-replace-all " " url "%20"))

(defun escape-parameter (param)
  (cl-ppcre:regex-replace-all "%20" param " "))

(defun valid-url-p (url)
  (not (cl-ppcre:all-matches "([@{}']|/[\.]+)" url)))

(defun normalize-url (url)
  (escape-url
   (if (string= "/" (subseq url (1- (length url))))
       (subseq url 0 (1- (length url)))
       url)))

(defun url-hostname (url)
  (or (first (cl-ppcre:all-matches-as-strings *hostname* url)) nil))

(defparameter *disregard-scanner* (cl-ppcre:create-scanner "(javascript|mailto:|pdf$|#.*$)"
                                                           :case-insensitive-mode t))

(defun disregard-these-links-p (link)
  (not (cl-ppcre:all-matches *disregard-scanner* link)))

(defun extract-next-element (html tag)
  (second (member tag (flatten html))))

(defun-exported extelm (html tag)
  (extract-next-element html tag))

(defun extag (html tag &key (test nil))
  (awhen (extract-lookahead html #'(lambda (x) (and (consp x) (eq (car x) tag))))
    (if test
        (find test it :test #'(lambda (tester elem) (member tester (car elem) :test #'string=)))
        it)))

(defun extract-label (html test)
  (awhen (extag html :label :test test)
    (trim-funny (second it))))

(defmacro deflookahead (tag)
  (with-gensyms (html x)
    `(defun ,(make-logic-symbol (concatenate 'string "extract-lookahead-" (symbol-name tag) "s")) (,html)
       (extract-lookahead ,html #'(lambda (,x) (and (consp ,x) (eql (car ,x) ,tag)))))))

(defmacro defsimpleall (tag)
  (with-gensyms (html x)
    `(defun ,(make-logic-symbol (concatenate 'string "extract-" (symbol-name tag) "s")) (,html)
       (nreverse (extract-all ,html #'(lambda (,x) (eql ,x ,tag)))))))

(defmacro deftagtest (tag)
  (with-gensyms (x)
    `(defun ,(make-logic-symbol (concatenate 'string (symbol-name tag) "p")) (,x)
       (eql ,x ,tag))))

(defmacro def-extract-next-element (tag)
  (with-gensyms (form)
    `(defun ,(make-logic-symbol (concatenate 'string "extract-next-" (symbol-name tag))) (,form)
       (extract-next-element ,form ,tag))))

(deftagtest :a)
(deftagtest :frame)

(defun-exported any-link-p (x) (or (ap x) (framep x)))

(deflookahead :form)
(deflookahead :select)
(deflookahead :td)
(defsimpleall :td)
(defsimpleall :tr)
(defsimpleall :option)

(def-extract-next-element :method)
(def-extract-next-element :action)
(def-extract-next-element :type)
(def-extract-next-element :name)
(def-extract-next-element :value)
(def-extract-next-element :id)
(def-extract-next-element :option)

(defparameter *visual-cue-elements* '(:font :b))

(defun cleanup-html (html)
  (labels ((rec (tree acc)
             (if (null tree)
                 (nreverse acc)
                 (if (simple-listp tree)
                     (cons
                      (append (list (first tree))
                              (awhen (extract-next-type tree)
                                (list :type it))
                              (awhen (extract-next-id tree)
                                (list :id it))
                              (awhen (extract-next-name tree)
                                (list :name it))
                              (awhen (extract-next-value tree)
                                (list :value it)))
                      acc)
                     (if (consp tree)
                         (rec (car tree)
                            (rec (cdr tree) acc))
                         (cons tree acc))))))
    (awhen (flatten (rec html '()))
      (remove-if #'(lambda (x) (member x *visual-cue-elements*)) it))))

(defun table-layout (table)
  (mapcar #'(lambda (tr) (nreverse (extract-lookahead-all tr #'(lambda (x) (eql x :td)))))
          (extract-trs table)))

(defun table-for-element (form-html element)
  (first
   (extract-lookahead form-html
                      #'(lambda (x) (and (consp x)
                                         (eql (car x) :table)))
                      :test-child #'(lambda (x) (extract-lookahead
                                                 x
                                                 #'(lambda (y)
                                                     (and (consp y)
                                                          (string= (first element) (extract-next-name y)))))))))

(defun label-element-candidates (table)
  (labels ((text-from (row col tds)
             (when (and (> row 0) (> col 0) tds)
               (awhen (nth (1- col) (nth (1- row) tds))
                 (when (eql :text (third it))
                   (fourth it))))))
    (let ((tds '()) (row 0) (col 0) (candidates '()))
      (dolist (tr table)
        (setf col 0)
        (incf row)
        (push (mapcar #'(lambda (td)
                          (incf col)
                          (append (list row col)
                                  (awhen (rest (cleanup-html td))
                                    (if (intersection it '(:select :input :radio :checkbox))
                                        (list (first (remove-if-not #'symbolp it))
                                              (extract-next-name it))
                                      (list :text (apply #'concatenate 'string
                                                         (mapcar #'(lambda (x)
                                                                     (format nil "~A " x))
                                                                 (remove-if-not #'stringp it))))))))
                    tr) tds))
      (setf tds (nreverse tds))
      (dolist (tr tds)
        (dolist (candidate (mapcar #'(lambda (td)
                                       (when (fourth td)
                                       (let ((row (first td))
                                             (col (second td)))
                                         (list (fourth td)
                                               (or (text-from (1- row) col tds)
                                                   (text-from row (1- col) tds))
                                               (third td)))))
                                   (remove-if #'(lambda (td) (or (null (third td)) (eql :text (third td)))) tr)))
          (push candidate candidates)))
      candidates)))

(defun extract-all-records (page &key (class nil) (some :td))
  (extract-lookahead page #'(lambda (x) (and (eql (car x) some)
                                             (or (not class)
                                                 (string= (attval x :class) class))))))

(defun-exported extrecs (page &key (class nil) (some :td))
  (extract-all-records page :class class :some some))

(defun extract-all-links (page &key (class nil))
  (simple-bind (links (extract-all page #'(lambda (x) (eql x :a))))
    (if class
        (remove-if-not #'(lambda (x) (string= (fifth x) class)) links)
        links)))

(defun url-relative (url)
  (subseq url (length (url-hostname url))))

(defun attval (html attribute)
  (second (member attribute html :test #'equal)))

(defun extract-lookahead-class (page &key (class nil) (node nil) (attval nil) (id nil) (att nil) (debug nil) (text nil))
  (let* ((class-matcher
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
              nil))
         (results
          (extract-lookahead page #'(lambda (x)
                              (when (and debug (eql (car x) node)) (print (flatten x)))
                              (and (eql (car x) node)
                                   (or (not class)
                                       (not
                                        (remove-if #'(lambda (y) (funcall y (extract-next-element x :class)))
                                                   class-matcher)))
                                   (or (not id)
                                       (string-equal (extract-next-element x :id) id))
                                   (or (not attval)
                                       (if (stringp (cdr attval))
                                           (string-equal (extract-next-element x (car attval)) (cdr attval))
                                           (funcall (cdr attval) (extract-next-element x (car attval)))))
                                   (or (not text)
                                       (string= (extract-text x) text)))))))
    (if (not att)
        results
        (mapcar #'(lambda (x) (extract-next-element x att)) results))))

(defun-exported split-matcher (v)
  #'(lambda (x)
      (funcall (apply #'and-fns
                      (mapcar #'(lambda (y)
                                  #'(lambda (a)
                                      (cl-ppcre:all-matches (cl-ppcre:create-scanner y :case-insensitive-mode t) a)))
                              (word-punc-splitter v)))
               x)))

(defun-exported matchr (v)
  #'(lambda (x) (cl-ppcre:all-matches-as-strings v x)))

(defun-exported ext (page &key (class nil) (node nil) (attval nil) (id nil) (att nil) (debug nil) (text nil))
  (extract-lookahead-class page :class class :node node :attval attval :id id :att att :debug debug :text text))

(defun-exported extext (html) (extract-text html))

(defun extract-text (html)
  (let ((text '()))
    (if (stringp html)
        (setf text (list html))
        (progn
          (dolist (a (extract-lookahead-class html :node :a))
            (when (stringp (second a))
              (push (second a) text)))
          (dolist (h (extract-lookahead
                      html
                      #'(lambda (x) (and (symbolp (car x)) (cl-ppcre:all-matches-as-strings "^H[0-9]$" (symbol-name (car x)))))))
            (when (stringp (second (car h)))
              (push (second (car h)) text)))
          (dolist (img (extract-lookahead-class html :node :img))
            (push (extract-next-element (car img) :alt) text))
          (dolist (b (extract-lookahead-class html :node :b))
            (when (stringp (second (car b)))
              (push (second (car b)) text)))
          (dolist (span (extract-lookahead-class html :node :span))
            (unless (not (second (first span)))
              (push (second (first span)) text)))
          (dolist (font (ext html :node :font))
            (push (join (remove-if-not #'stringp (rest font)) " ") text))
          (dolist (td (ext html :node :td))
            (when (stringp (second td))
              (push (second td) text)))))
    (join (mapcar #'trim (nreverse text)) " ")))

(defun-exported extimg (page &key (largest t))
  (let ((imgs (mapcar #'(lambda (x) (cons x
                                          (* (parse-integer (or (extract-next-element (car x) :width) "10"))
                                             (parse-integer (or (extract-next-element (car x) :height) "10")))))
                      (ext page :node :img)))
        (noscript (extract page :noscript)))
    (awhen (remove-if
            #'(lambda (img) (noscript noscript (car img)))
            imgs)
      (if largest
          (caar (sort it #'> :key #'cdr))
          it))))

(defun-exported noscript (noscript img)
  (member img
          (ext noscript :node (car (flatten img)))
          :test #'equal))


(defun-exported extract-url-parameters (parameters url)
  (mapcar #'(lambda (param) (extract-url-parameter param url)) parameters))

(defun-exported extract-url-parameter (parameter-name url)
  (aif (cl-ppcre:all-matches-as-strings (format nil "[?&]~A=[^&]+(&|$)" parameter-name) url)
       (unless (string= "" (car it))
         (if (string= "&" (subseq (car it) (1- (length (car it))) (length (car it))))
             (subseq (car it) (+ 2 (length parameter-name)) (1- (length (car it))))
             (subseq (car it) (+ 2 (length parameter-name)) (length (car it)))))))

(defun puri-uri-string (uri)
  (format nil "~A://~A~A~A~A"
          (string-downcase (symbol-name (puri:uri-scheme uri)))
          (puri:uri-host uri)
          (or (puri:uri-port uri) "")
          (puri:uri-path uri)
          (if (puri:uri-query uri)
              (concatenate 'string "/" (puri:uri-query uri))
              "")))

(defun element= (list tag class id)
  (labels ((element-part (element part)
             (aif (member part element)
                  (cadr it)
                  "")))
    (cond ((atom list) nil)
          ((atom (car list)) (and (eql tag (car list))
                                  (string= "" class)
                                  (string= "" id)
                                  list))
          (t (and (eql tag (caar list))
                  (or (string= "" class) (string= (element-part (car list) :class) class))
                  (or (string= "" id) (string= (element-part (car list) :id) id))
                  list)))))

(defun element-p (list)
  (and list
       (not (atom list))
       (or (and (atom (car list)) (symbolp (car list)))
           (not (remove-if #'atom (car list))))
       (cdr list)))

(defun find-html-node (html-list tag &key (class "") (id ""))
  (cond ((null html-list) nil)
        ((element-p html-list)
         (or (element= html-list tag class id)
             (some #'(lambda (part) (find-html-node part tag :class class :id id))
                   (cdr html-list))))
        (t nil)))

(defun remove-html-node-if-not (html-list test)
  (labels ((remove-html-rec (list accumulator)
             (cond ((null list) accumulator)
                   ((element-p list)
                    (if (funcall test list)
                        (cons list accumulator)
                        (let ((result accumulator))
                          (dolist (element (cdr list))
                            (setf result (remove-html-rec element result)))
                          result)))
                   (t accumulator))))
    (nreverse (remove-html-rec html-list '()))))

