;; -*- lisp -*-

(defpackage :com.crawler.system
  (:use :asdf :cl))

(in-package :com.crawler.system)

(defsystem :crawler
    :name "crawler"
    :author "Vikram Bhandoh <enginedriver@telltrains.com>"
    :version "0.2"
    :description "All code for the crawler and the scrapper with site-management"
    :depends-on (:cl-ppcre :md5 :uffi :cl-html-parse :drakma :gzip-stream
                 :cl-muproc :hunchentoot :cl-who :json :closure-html :cxml-stp
                 :split-sequence :acl-compat :s-http-client :ironclad :fiveam)
    :components
    ((:static-file "crawler.asd")
     (:module :clcrawler
              :components
              ((:file "packages")
               (:file "system" :depends-on ("packages"))
               (:file "heap" :depends-on ("packages" "system"))
               (:file "strings" :depends-on ("packages" "system"))
               (:file "lists" :depends-on ("packages" "system" "strings"))
               (:file "debug" :depends-on ("packages" "system"))
               (:file "unix" :depends-on ("packages" "system"))
               (:file "date" :depends-on ("packages" "system"))
               (:file "controls" :depends-on ("packages" "lists" "system"))
               (:file "symbols" :depends-on ("packages" "controls" "system"))
               (:file "bloomfilters" :depends-on ("packages" "system"))
               (:file "filequeue" :depends-on ("packages" "system"))
               (:file "prolog" :depends-on ("packages" "controls" "system"))
               (:file "utilities" :depends-on ("packages" "controls" "system"))
               (:file "functionmacros" :depends-on ("packages" "heap" "lists" "debug" "unix" "date" "strings" "controls"
                                                               "symbols" "system" "utilities" ))
               (:file "conditionals" :depends-on ("packages" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols"
                                                             "system" "utilities" "functionmacros"))
               (:file "compress" :depends-on ("packages" "system" "utilities" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols"
                                                         "system" "bloomfilters" "filequeue" "prolog" "utilities" "functionmacros" "conditionals" "functionmacros"))
               (:file "checkpoints" :depends-on ("packages" "system" "compress" "utilities" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols"
                                                         "system" "bloomfilters" "filequeue" "prolog" "utilities" "functionmacros" "conditionals" "functionmacros"))
               (:file "files" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols"
                                                      "system" "bloomfilters" "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros"))
               (:file "bind" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters"
                                                     "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files"))
               (:file "hash" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind"))
               (:file "elements" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash"))
               (:file "cleanup" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash"))
               (:file "words" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters"
                                                      "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup"))
               (:file "suffix-tree" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters"
                                                      "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words"))
               (:file "ngrams" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words"))
               (:file "indexer" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "ngrams"))
               (:file "similar" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "ngrams"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "indexer"))
               (:file "bayesian" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "ngrams"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "indexer" "similar"))
               (:file "recursive-extracter" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "ngrams"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "indexer" "similar" "bayesian" "elements"))
               (:file "detection" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "ngrams" "recursive-extracter"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "indexer" "similar" "bayesian" "elements"))
               (:file "extracter" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "ngrams" "recursive-extracter"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words"
                                                        "indexer" "similar" "bayesian" "detection" "elements"))
               (:file "selectors" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "ngrams"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "indexer"
                                                        "similar" "bayesian" "detection" "extracter" "elements"))
               (:file "formfiller" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "ngrams"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "indexer"
                                                        "similar" "bayesian" "detection" "selectors" "extracter" "elements"))
               (:file "crawler" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "ngrams"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "indexer"
                                                        "similar" "bayesian" "detection" "selectors" "extracter" "formfiller" "elements"))
               (:file "sites" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "crawler" "ngrams"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "indexer"
                                                        "similar" "bayesian" "detection" "selectors" "extracter" "formfiller" "elements"))
               (:file "instance" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "crawler" "ngrams"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "indexer"
                                                        "similar" "bayesian" "detection" "selectors" "extracter" "formfiller" "elements"))
               (:file "tuplemaker" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "crawler" "ngrams"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "indexer"
                                                        "similar" "bayesian" "detection" "selectors" "extracter" "formfiller" "elements" "instance" "sites"))
               (:file "scrapper" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "ngrams"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "indexer"
                                                        "similar" "bayesian" "detection" "selectors" "extracter" "formfiller" "elements" "crawler" "sites" "instance" "tuplemaker"))
               (:file "wrapper" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "ngrams"
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "indexer"
                                                        "similar" "bayesian" "detection" "selectors" "extracter" "formfiller" "elements" "crawler" "sites" "instance" "tuplemaker" "scrapper"))
               (:file "example" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "ngrams" 
                                                        "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "indexer"
                                                        "similar" "bayesian" "detection" "selectors" "extracter" "formfiller" "elements" "crawler" "sites" "instance" "tuplemaker" "scrapper" "wrapper"))
               (:file "tests" :depends-on ("packages" "compress" "heap" "lists" "debug" "unix" "date" "strings" "controls" "symbols" "system" "bloomfilters" "scrapper" "crawler" "sites" "ngrams"
                                                      "elements" "filequeue" "checkpoints" "prolog" "utilities" "conditionals" "functionmacros" "files" "bind" "hash" "cleanup" "words" "indexer"
                                                      "similar" "bayesian" "detection" "selectors" "extracter" "formfiller" "recursive-extracter" "instance" "wrapper"))
               ))))
