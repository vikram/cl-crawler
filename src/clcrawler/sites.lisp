(in-package :com.crawler)

(defun crawler-path (file)
  (format nil "/home/vb/repository/portfolio/crawler/data/crawldb/~A/" file))

(defparameter *sites* (make-hash-table :test 'equal))

(defstruct url (url nil :type sequence) (fetched nil :type symbol) (html () :type list))

(defclass-exported site ()
  ((hostname :initarg :hostname :reader site-hostname)
   (ip-address :initarg :ip-address :initform nil :accessor site-ip-address)
   (url-queue :initarg :url-queue :accessor site-url-queue)
   (throttle :initform 1 :accessor site-throttle)
   (last-accessed-on :initarg :last-accessed-on :initform nil :accessor site-last-accessed-on)
   (crawl-rate :accessor site-crawl-rate :initform 2)
   (robot :accessor site-robot-filter :initform nil)
   (depth :accessor site-depth :initform 5)
   (filter :accessor site-filter :initarg :filter)
   (storer :accessor site-storer :initarg :storer)
   (url-storer :accessor site-url-storer :initarg :url-storer)
   (visited :accessor site-visited :initform (make-hash-table :test 'equal))
   (sitemap :initform nil :accessor site-sitemap)
   (disallowed :initform nil :accessor site-disallowed-urls)
   (allowed :initform nil :accessor site-allowed-urls)
   (allowed-test :initform nil :accessor site-allowed-p)))

(defparameter *crawl-path* nil)
(defparameter *unknown-frontier* nil)

(defun-exported setup-crawl (&optional (time (short-date-time)))
  (clrhash *sites*)
  (setf *crawl-path* (crawler-path time))
  (ensure-directories-exist *crawl-path*)
  (setf *unknown-frontier* (build-queue (format nil "~A/other.links" *crawl-path*))))

(defun-exported site-path (site &optional (file-name ""))
  (url-site-path (site-hostname site) file-name))

(defun-exported url-site-path (url &optional (file-name ""))
  (format nil "~A~A/~A" *crawl-path* (domain (url-hostname url)) file-name))

(defun domain (url)
  (first (cl-ppcre:all-matches-as-strings *domain* (cl-ppcre:regex-replace-all *protocol* (url-hostname url) ""))))

(defun make-storer (site)
  #'(lambda (docid html url)
      (with-open-file (out (site-path site "urls") :if-exists :append :if-does-not-exist :create :direction :output)
        (write-line (format nil "~A ~A" docid url) out)
        (gzip-stream:gzip-string (site-path site docid) html))))

(defun make-url-storer (site)
  #'(lambda (docid url reason attributes)
      (with-open-file (out (site-path site reason) :if-exists :append :if-does-not-exist :create :direction :output)
        (write-line (format nil "~A ~A ~A" docid url (join attributes " ")) out))))

(defun remove-comment (line)
  (subseq line 0 (position #\# line)))

(defun parse-robots-txt (text)
  (let ((lines (remove-if #'(lambda (x) (string= "" x)) (split-sequence:split-sequence #\newline text))))
    (labels ((allowedornot (which)
               (let ((rules nil)
                     (ua nil))
                 (loop for line in lines
                    do (cl-ppcre:register-groups-bind (field value)
                         ("^(.*?):\\s*(\\S+)\\s*" (remove-comment line))
                       (cond
                         ((string-equal field "user-agent")
                          (setq ua value)
                          (let ((u (assoc ua rules)))
                            (when (null u)
                              (setq rules (cons (list ua) rules)))))
                         ((string-equal field which)
                          (let ((rule (assoc ua rules)))
                            (setf (cdr rule) (push value (cdr rule))))))))
                 rules)))
      (values
       (mapcar #'(lambda (x)
                   (cons (car x) (nreverse (cdr x))))
               (nreverse (allowedornot "disallow")))
       (mapcar #'(lambda (x)
                   (cons (car x) (nreverse (cdr x))))
               (nreverse (allowedornot "allow")))))))

(defun add-site->crawlers (site)
  (setf (gethash (make-hash-code (site-hostname site)) *sites*) site))

(defun add-url (url &key (make-site-p nil))
  (let ((host (url-hostname url)))
    (when (and host (valid-url-p url))
      (aif (gethash (make-hash-code host) *sites*)
           (enqueue (normalize-url url) (site-url-queue it))
           (if make-site-p
               (progn
                 (add-site->crawlers (make-site host))
                 (add-url url))
               (add->unknown-frontier url))))))

(defun add->unknown-frontier (url)
  (enqueue (normalize-url url) *unknown-frontier*))

(defun make-site (host &key (new-queue t))
  (let ((site (make-instance 'site :hostname host))
        (robot (if (cl-ppcre:all-matches "^http" host)
                    (join (list host "robots.txt") "/")
                    (join (list "http:/" host "robots.txt") "/"))))
    (print robot)
    (setf (site-storer site) (make-storer site))
    (setf (site-url-storer site) (make-url-storer site))
    (setf (site-url-queue site) (build-queue (site-path site "new-urls") new-queue))
    (ensure-directories-exist (site-path site))
    (ensure-directories-exist (site-path site "extracted/"))
    (ensure-directories-exist (site-path site "cleaned/"))
    (ensure-directories-exist (site-path site "pruned/"))
    (ensure-directories-exist (site-path site "template/"))
    (ensure-directories-exist (site-path site "wrappers/"))
    (setf (site-allowed-p site) #'identity)
    (awhen (fetch robot)
      (urlvisited robot it (make-hash-code robot) site)
      (when (stringp it)
        (multiple-value-bind (disallowed allowed)
            (parse-robots-txt it)
          (setf (site-disallowed-urls site) disallowed)
          (setf (site-allowed-urls site) allowed)
          (setf (site-allowed-p site) (url-allowed-p site)))))
    site))

(defun urlvisited-reason (url docid reason attributes)
  (awhen (url-site url)
    (funcall (site-url-storer it) docid url reason attributes)))

(defun-exported url-site (url)
  (gethash (make-hash-code (url-hostname url)) *sites*))

(defun isUrlVisited (url)
  (awhen (url-site url)
    (gethash (make-hash-code url) (site-visited it))))

(defun-exported urlvisited (url html docid &optional (site nil))
  (awhen (or site (url-site url))
    (funcall (site-storer it) docid (tidy-page html) url)
    (setf (gethash (make-hash-code url) (site-visited it)) (get-universal-time))))

(defun make-site-crawler (host number mbox)
  (let ((site (gensym)))
    (muproc:mumsg-send mbox :tag 'site :msg host)
    (muproc:muproc-spawn site
                         #'site-crawler
                         (list host number mbox)
                         :errorstream *trace-output*)))

(defun site-crawler (seed-url number mbox)
  (let ((site (or (url-site seed-url)
                  (make-site (url-hostname seed-url))))
        (i 0)
        (keep-going t))
    (muproc:mumsg-send mbox :tag 'crawling :msg seed-url)
    (add-site->crawlers site)
    (add-url seed-url)
    (loop
         (muproc:mumsg-receive (from)
           ((fetch) t
            (while (and (> number i) keep-going)
              (sleep 1)
              (case (crawl-next site :mbox mbox)
                (fetched (incf i))
                (nothing-to-fetch (setf keep-going nil))))
            (muproc:mumsg-send from :fetched i :seed-url seed-url))
           ((done) t
            (muproc:muproc-exit :done))))))

(defun build-frontier ()
  (build-queue (format nil "~A/frontier" *crawl-path*)))

(defun find-latest-page (url)
  (awhen (sort
          (directory (make-pathname :name :wild :type :wild :defaults "/home/vb/repository/portfolio/crawler/data/crawldb/2008"))
          #'string> :key (compose #'car #'last #'pathname-directory))
    (dolist (path it)
      (let* ((path (format nil "~A/~A" (pathname->string path) (domain url)))
             (urls (format nil "~A/~A" path "urls")))
        (when (probe-file urls)
          (with-open-file (in urls)
            (do ((line (read-line in nil :eof nil)
                       (read-line in nil :eof nil)))
                ((eql line :eof))
              (when (string= (second (split-sequence:split-sequence #\space line)) url)
                (return-from find-latest-page 
		  (read-html-file 
		   path
		   (first (split-sequence:split-sequence #\space line))))))))))))

(defun pathname->string (pathname)
  (awhen (pathname-directory pathname)
    (when (eql :absolute (first it))
      (apply #'concatenate 'string (mapcar #'(lambda (x) (format nil "/~A" x)) (rest it))))))

(defun-exported read-site-info (url)
  (if (url-site url)
      (url-site url)
      (awhen (sort
              (directory (make-pathname :name :wild :type :wild :defaults "/home/vb/repository/portfolio/crawler/data/crawldb/2008"))
              #'string> :key (compose #'car #'last #'pathname-directory))
        (dolist (path it)
          (when (setup-site-info url path)
            (return (url-site url)))))))

(defun reset-crawl-queue (site-url)
  (let ((q (site-url-queue (url-site site-url))))
    (when (probe-file (file-queue-path q))
      (with-open-file (in (file-queue-path q))
        (setf (file-queue-size q) (file-length in))
        (setf (file-queue-pos q) 0)))))

(defun setup-site-info (url path)
  (let* ((crawl-path (pathname->string path))
         (path (format nil "~A/~A" crawl-path (domain url)))
         (urls (format nil "~A/~A" path "urls")))
    (when (probe-file urls)
      (setf *crawl-path* (if (string= "/" (subseq crawl-path (length crawl-path)))
                             crawl-path
                             (concatenate 'string crawl-path "/")))
      (setf *unknown-frontier* (build-queue (format nil "~A/other.links" *crawl-path*)
                                            (not (probe-file (format nil "~A/other.links" *crawl-path*)))))
      (setf (gethash (make-hash-code (url-hostname url)) *sites*) (make-site (url-hostname url) :new-queue nil))
      (reset-crawl-queue url)
      (with-open-file (in urls)
        (awhile (read-line in nil nil nil)
          (let ((sprd (split-sequence:split-sequence #\space it)))
            (unless (string= "" (first sprd))
              (setf (gethash (first sprd) (site-visited (url-site url)))
                    (second sprd))))))
      t)))

(defun url-allowed-p (site)
  (let ((disallowed-scanner nil)
        (allowed-scanner nil))
    (labels ((make-dis/allow-scanner (parts)
               (cl-ppcre:create-scanner
                (cl-ppcre:regex-replace-all
                 "\\?"
                 (concatenate 'string "(" (join parts "|") ")")
                 "\\?")
                :case-insensitive-mode t))
             (scanner-strings (parts)
               (let ((patterns '()))
                 (dolist (useragent parts)
                   (when (or (string= "*" (first useragent)) (cl-ppcre:all-matches "tellbot" (first useragent)))
                     (setf patterns (cons patterns (rest useragent)))))
                 (remove-empty patterns))))
      (setf disallowed-scanner (make-dis/allow-scanner (scanner-strings (site-disallowed-urls site))))
      (setf allowed-scanner (make-dis/allow-scanner (scanner-strings (site-allowed-urls site))))
      #'(lambda (url)
          (or
           (first (cl-ppcre:all-matches allowed-scanner url))
           (not (first (cl-ppcre:all-matches disallowed-scanner url))))))))

(defun make-site-filter (site-url file)
  (let ((filter (make-filter)))
    (with-read-line (line (site-path (url-site site-url) file))
      (add filter line))
    filter))

(defun consolidate-new-urls-file (site-url)
  (let ((newurls-filter (make-filter :size 100000)))
    (with-open-file (out (site-path (url-site site-url) "new-urls.cmp")
                         :direction :output :if-exists :supersede :if-does-not-exist :create)
      (with-read-line (line (site-path (url-site site-url) "new-urls"))
        (unless (contains newurls-filter line)
          (add newurls-filter line)
          (write-line line out))))
    (rename-file (site-path (url-site site-url) "new-urls.cmp") (site-path (url-site site-url) "new-urls"))
    (reset-crawl-queue site-url)
    newurls-filter))

(defun read-html-file (hostpath name)
  (read-from-string
   (gzip-stream:gunzip->string
    (format nil "~A/~A" hostpath name))))

(defun-exported read-page (url)
  (when-bind (site
              (or (url-site url)
                  (read-site-info url)))
    (let ((path (site-path (url-site url)
                           (make-hash-code url))))
      (when (probe-file path)
	(values
	 (gzip-stream:gunzip->string path)
	 path)))))

(defun read-all-pages (url reader &key (filter #'identity) (skip-until nil))
  (with-read-line (line (site-path (url-site url) "urls"))
    (let* ((docid-url (split-sequence:split-sequence #\space line))
           (docid (first docid-url))
           (url (second docid-url)))
      (when (funcall filter url)
	(when (and skip-until (string= skip-until url))
	  (setf skip-until nil)
	  (print url))
        (unless skip-until
	  (print url)
	  (funcall reader url docid))))))

(defun tidy-page (page)
  (let ((str ""))
    (when (probe-file "/home/vb/repository/portfolio/crawler/data/tidyin.temp")
      (delete-file "/home/vb/repository/portfolio/crawler/data/tidyin.temp"))
    (write-text "/home/vb/repository/portfolio/crawler/data/tidyin.temp" page)
    (sb-ext:run-program "/usr/local/bin/tidy" '("-m")
                        :input "/home/vb/repository/portfolio/crawler/data/tidyin.temp")
    (with-open-file (in "/home/vb/repository/portfolio/crawler/data/tidyin.temp")
      (setf str (make-string (file-length in)))
      (read-sequence str in))
    str))

(defun setup-site (url)
  (progn
    (sb-ext:run-program "/usr/bin/sort" '("-u")
                        :input (url-site-path url "new-urls")
                        :output (url-site-path url "uniq-urls"))
    (add-url url)
    (when (probe-file (url-site-path url "new-urls")) (delete-file (url-site-path url "new-urls")))
    (rename-file (url-site-path url "uniq-urls") (url-site-path url "new-urls"))
    (reset-site-queue-size url)
    (with-open-file (in (url-site-path url "urls"))
      (do ((line (read-line in nil)
                 (read-line in nil)))
          ((null line))
        (when line
          (awhen (split-sequence:split-sequence #\space line)
            (setf (gethash (make-hash-code (second it)) (site-visited (url-site url))) t)
            (print it t)))))))

(defun reset-site-queue-size (url)
  (with-open-file (in (url-site-path url "new-urls"))
    (do ((line (read-line in nil)
               (read-line in nil)))
        ((null line))
      (when line
        (incf (file-queue-size (site-url-queue (url-site url))))))))

(defun sort-new-urls (url)
  (let ((urls '())
        (i 1))
    (ensure-directories-exist (site-path (url-site url) "barrels/"))
    (with-open-file (in (site-path (url-site url) "new-urls"))
      (multiple-value-bind (bucket left-over)
          (floor i 10000)
        (if (zerop left-over)
            (with-open-file (out (site-path (url-site url) (format nil "barrels/new.~A" bucket))
                                 :direction :output :if-exists :supersede :if-does-not-exist :create)
              (write (sort (delete-duplicates urls :test #'string=)
                           #'string>) :stream out)
              (print (cons 'sorting bucket) t)
              (incf i)
              (setf urls nil))
            (do ((line (read-line in nil)
                       (read-line in nil)))
                ((null line))
              (when line
                (incf i)
                (push line urls))))))))

(defun onetime-setup-site (url)
  (progn
    (add-url url)
    (with-open-file (out (url-site-path url "all-urls")
                         :direction :output :if-exists :supersede :if-does-not-exist :create)
      (with-open-file (in (url-site-path url "urls"))
        (do ((line (read-line in nil)
                   (read-line in nil)))
            ((null line))
          (when line
            (awhen (split-sequence:split-sequence #\space line)
              (setf (gethash (make-hash-code (second it)) (site-visited (url-site url))) t)
              (print it t)
              (dolist (link (links-from-page (url-site url) (read-page url)))
                (if (cl-ppcre:all-matches (site-hostname (url-site url)) link)
                    (unless (shouldnt-fetch-url (url-site url) link)
                      (add-url link))
                    (write-line link out))))))))))

(defun shouldnt-fetch-url (site url)
  (or (not (funcall (site-allowed-p site) url))
      (isUrlVisited url)))

(defun print-urls (site-url)
  (read-all-pages
   site-url
   #'(lambda (url docid) (print (cons url docid)))
   ))

(defun throttle (url)
  (if (site-crawl-rate (url-site url))
      (sleep (/ 1 (site-crawl-rate (url-site url))))
      (sleep 1)))

(defun archive-site (host)
  (when (probe-file (url-site-path host))
      (rename-file (url-site-path host)
                   (format nil "~A~A"
                           (STR-BUTLAST (url-site-path host))
                           (get-universal-time)))))

(defun create-site (host)
  (let ((site nil))
    (unless *crawl-path*
      (setup-crawl))
    (archive-site host)
    (setf site (make-site (url-hostname host)))
    (setf (gethash (make-hash-code (url-hostname host)) *sites*) site)
    site))

(defun gather-data (host seed-file filters &key (setup t) (form-urls nil) (seed-urls nil) (debug nil))
  (labels ((gather (line site)
             (unless (and (shouldnt-fetch-url site line) (not (isUrlVisited line)))
               (print line)
               (throttle line)
               (multiple-value-bind (html status-code headers uri)
                   (fetch line)
                 (fetch-extract-url line (make-hash-code line) site
                                    :html html
                                    :form-urls form-urls
                                    :real-url (puri-uri-string uri))
                 (fetch-next-url site filters)))))
  (let ((site nil))
    (setf site (init-or-read-site host :setup setup))
    (print seed-file)
    (with-read-line (line seed-file)
      (when debug (print line))
      (when (funcall filters line)
        (gather line site)))
    (dolist (url seed-urls)
      (when debug (print url))
      (when (funcall filters url)
        (gather url site))))))

(defun-exported init-or-read-site (host &key (setup t))
  (if setup
      (create-site host)
      (progn
        (read-site-info host)
        (url-site host))))

(defun init-site (host)
  (let ((site nil))
    (setup-crawl)
    (setf site (make-site (url-hostname host)))
    (setf (gethash (make-hash-code (url-hostname host)) *sites*) site)))

(defun-exported seeds (seeder)
  (links-from-page
   (url-site seeder)
   (fetch-while-empty
    seeder
    (make-hash-code seeder))
   seeder))

(defun-exported site-crawls (hostname)
  (mapcar
   #'(lambda (path)
       (cons path
             (dttm->string (file-write-date path) :format :ts)))
   (remove-if-not
    #'(lambda (path)
        (cl-ppcre:all-matches hostname (clast (pathname-directory path))))
    (directory (make-pathname :name :wild :type :wild :defaults *crawl-path*)))))

(defmacro defcodeset (name)
  `(progn
     (defvar-exported ,(make-logic-symbol (format nil "*~A*" (symbol-name name))) '())
     (defun-exported ,(make-logic-symbol (format nil "add-to-~A" (symbol-name name))) (name fn)
       (push (cons name fn) ,(make-logic-symbol (format nil "*~A*" (symbol-name name)))))))

(defcodeset crawler)
(defcodeset extracter)
(defcodeset analyzer)
(defcodeset inducers)
(defcodeset pruners)

(defun get-page (url)
  (let ((page (fetch-while-empty url (make-hash-code url))))
    page))
