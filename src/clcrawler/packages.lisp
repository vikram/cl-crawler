
;; -*- lisp -*-
(in-package :cl-user)

(defpackage :com.crawler
  (:nicknames #:crawler #:scrapper #:indexer)
  (:use :common-lisp :cl-who :hunchentoot)

  (:export

   :defscan
   :scrape
   :$*
   :cleanup-url
   :file-type
   :trim
   :trim-funny
   :replace-funny

   :multiway-in-place-sort-inverter
   :INDEX-PATH->FILE
   :searcher
   :word-tokenizer
   :term-group-indexer
   :isurlvisited

   :make-seeder
   :gather-data
   :url
   :docid
   :wrapper
   :wrapper-page
   :wrapper-type
   :wrapper-pattern
   :wrapper-follows-from
   :wrapper-tuple
   :tuple
   :tuple-table
   :tuple-dbname
   :tuple-fields
   :field
   :field-name
   :field-dbname
   :field-type
   :field-dbtype
   :field-conv-fn
   :MAKE-SCANNER
   :MAKE-SITE
   :MAKE-SITE-CRAWLER
   :NEXT-URL-FOR-SITE
   :RESET-CRAWL-QUEUE
   :SETUP-CRAWL
   :SITE-HOSTNAME
   :SITE-URL-QUEUE
   :SITE-VISITED
   :PARSE-FLOAT
   :WHITESPACEP
   :*CRAWL-PATH*
   :*SITES*

   :ADD-URL
   :BUILD-FRONTIER
   :DEQUEUE
   :DISREGARD-THESE-LINKS-P
   :ENQUEUE
   :FETCH-EXTRACT-URL
   :FETCH-IN-SESSION
   :FILE-QUEUE-SIZE
   :GENERATE-URLS
   :deftemplate

   ))
