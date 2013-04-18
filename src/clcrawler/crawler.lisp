(in-package :com.crawler)

(setf chunga:*accept-bogus-eols* t)
(setf puri:*strict-parse* nil) ;;to fix the | in the urls
(setf flexi-streams:*substitution-char* #\space)

(defun start-crawling-site (seed-url)
  (awhen (url-hostname seed-url)
    (let ((site (make-site it)))
      (add-site->crawlers site)
      (add-url seed-url)
      (crawl-next site))))

(defun general-files-to-avoid (url)
  (cl-ppcre:all-matches "\\\.(xls|js|css|doc|pdf|swf|mp[0-9a-z]{1,2})$" url))

(defun next-url-for-site (site filter)
  (let ((url (dequeue (site-url-queue site))))
    (when url
      (if (or (shouldnt-fetch-url site url)
              (not (funcall filter url))
              (general-files-to-avoid url))
          (next-url-for-site site filter)
          (values url (make-hash-code url))))))

(defun fetch (url &key (method :get))
  (handler-case
      (progn
        (puri:parse-uri url)
        (multiple-value-bind (html status-code headers uri)
            (handler-case (drakma:http-request url :user-agent :firefox :close t :keep-alive nil :method method)
              (usocket:socket-error () ""))
          (values html
                  status-code
                  headers
                  uri)))
    (puri::uri-parse-error () nil)))

(defun-exported fetch->lst (url &key (method :get))
  (multiple-value-bind (html code headers uri)
      (fetch url :method method)
    (if (and html (stringp html) (string/= "" (trim html)))
        (translate-fetched html headers)
        nil)))

(defun translate-fetched (html headers)
  (casequal (first (cl-ppcre:all-matches-as-strings "[a-z/\\+]+" (cdr (assoc :content-type headers))))
            ("image/jpeg"
             html)
            ("application/atom+xml"
             (html->list
              (let ((str (make-string (length html))))
                (dotimes (i (length html))
                  (setf (aref str i) (code-char (aref html i))))
                str)))
            ("text/plain"
             (handler-case (json:decode-json-from-string html)
               (simple-type-error () html)))
            ("text/html"
             (if (and html (stringp html))
                 (html->list html)
                 nil))))

(defun fetch-in-session (url &optional (cookie-jar (make-instance 'drakma:cookie-jar)))
  (multiple-value-bind (html status-code headers uri)
      (handler-case (drakma:http-request url
                                         :user-agent :firefox2
                                         :cookie-jar cookie-jar
                                         :accept "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"
                                         :additional-headers '(("Accept-Language" . "en-gb,en;q=0.5")
                                                               ("Accept-Charset" . "ISO-8859-1,utf-8;q=0.7,*;q=0.7")
                                                               ("Keep-Alive" . "300")
                                                               ("Connection" . "keep-alive")))
        (usocket:socket-error () nil))
    (if (and html (stringp html))
        (values html
                status-code
                headers
                uri
                cookie-jar)
        (values nil
                404
                nil
                nil
                cookie-jar))))

(defun-exported links-from-page (site page &optional (main-url nil))
  (let ((links nil))
    (awhen (html->list page)
      (dolist (url (nreverse (generate-urls (site-hostname site)
                                            (extract-all it #'any-link-p)
                                            :url (or main-url (site-hostname site))
                                            :test #'disregard-these-links-p)))
        (push (cleanup-url url) links))
      (when main-url
        (dolist (url (generate-urls-for-this-form it main-url))
          (push (cleanup-url url) links))))
    links))

(defun add-urls-from-page (site page &optional (main-url nil))
  (dolist (link (links-from-page site page main-url))
    (add-url link)))

(defun crawl-next (site &key (filter #'identity) mbox)
  (multiple-value-bind (url docid)
      (next-url-for-site site filter)
    (if url
        (let ((html (fetch url)))
          (when mbox
            (muproc:mumsg-send mbox :tag 'fetching :msg url))
          (urlvisited url html docid)
          (add-urls-from-page site html url)
          'fetched)
        (progn
          'nothing-to-fetch))))

(defun crawl-within (within-url)
  (let ((host (cl-ppcre:create-scanner (cl-ppcre:regex-replace-all "\\?" within-url "\\?"))))
    (crawl-next (url-site within-url) :filter #'(lambda (link) (cl-ppcre:all-matches host link)))))

(defun crawl-some (url &optional (max-pages 50000))
  (let ((i 0))
    (while (and (> max-pages i)
                (> (size (site-url-queue (url-site url))) 0))
      (throttle url)
      (if (eql (crawl-within url) 'nothing-to-fetch)
          (return)))))

;seed-urls
;frontier
;extract-urls->frontier
;store-page
;dequeue-url<-frontier
;crawl-run
;   repository
;    frontier
;    seed-urls
;  robots.txt reader

(defun-exported ensure-page->lst (url docid &optional (count 0))
  (html->list (fetch-while-empty url docid count)))

(defun-exported fetch-while-empty (url docid &optional (count 0))
  (multiple-value-bind (page path) (read-page url)
    (if page
        (values page path)
        (if (> count 5)
            nil
            (progn
              (throttle url)
              (refetch url)
              (fetch-while-empty url docid (1+ count)))))))

(defun refetch (url)
  (print (cons 'refetching url))
  (multiple-value-bind (html status headers uri)
      (fetch url)
    (if html
        (progn
          (urlvisited url html (make-hash-code url))
          (add-urls-from-page (url-site url) html)
          'fetched)
        status)))

;refetch something that wasn't fetched properly the first time

(defun fetch-extract-url (main-url docid site
                          &key (cookie-jar nil)
                               (form-urls nil)
                               (real-url main-url)
                               (html (if cookie-jar
                                         (fetch-in-session main-url cookie-jar)
                                         (fetch main-url))))
  (progn
    (urlvisited main-url html docid)
    (urlvisited-reason main-url docid "redirect-from" (list real-url (make-hash-code real-url)))
    (urlvisited-reason real-url (make-hash-code real-url) "redirect-to" (list main-url docid))
    (add-urls-from-page site html main-url)))

(defun extract-url-folders (url)
  (first
   (cl-ppcre:all-matches-as-strings
    "^http://[a-zA-Z0-9.]+/?([a-zA-Z0-9]+/|)*"
    url)))

(defun fetch-next-url (site filters)
  (while-bind (url-docid (multiple-value-list
                          (next-url-for-site
                           site
                           filters)))
    (if (null (first url-docid))
        (return)
        (when (first url-docid)
          (print (first url-docid))
          (fetch-extract-url (first url-docid) (second url-docid) site)))))

(defun make-seeder (file)
  (let ((last-pos 0))
    #'(lambda ()
        (let ((i 0) (url nil) (done nil))
          (with-open-file (in file)
            (awhile (read-line in nil nil nil)
              (incf i)
              (when (and (not done) (= i last-pos))
                (setf url it)
                (setf last-pos i)
                (setf done t))))
          url))))

(defun analyse-crawl (crawltime &key (refetch-empties t))
  (dolist (host (remove-if-not #'(lambda (www) (cl-ppcre:all-matches "www" (string-downcase www)))
                               (mapcar #'(lambda (path) (car (last (pathname-directory path))))
                                       (directory (make-pathname :name :wild :type :wild :defaults
                                                                 (format nil "/home/vb/repository/portfolio/crawler/data/crawldb/~A/" crawltime))))))
    (let* ((domain (domain host))
           (hostpath (format nil "/home/vb/repository/portfolio/crawler/data/crawldb/~A/~A/" crawltime domain)))
      (print (or domain host))
      (if (probe-file (format nil "~Aurls" hostpath))
          (with-open-file (in (format nil "~Aurls" hostpath))
            (do ((line (read-line in nil :eof nil)
                       (read-line in nil :eof nil)))
                ((eql line :eof))
              (awhen (split-sequence:split-sequence #\space line)
                (let ((len (length (read-html-file hostpath (first it)))))
                  (if (and (zerop len) refetch-empties (disregard-these-links-p (second it)))
                      (refetch (second it))
                      (print (list 'fetched (second it) len)))))))
          (crawl-some (concatenate 'string "http://" host) 10)))))

(defun get-page-text-for-url (url)
  (page-text (fetch-while-empty url (make-hash-code url))))

(defun get-page-text/tokens-for-url (url)
  (join
   (remove-empty (remove-empty-string (flatten (page-text/tokens (fetch-while-empty url (make-hash-code url))))))
   " "))

