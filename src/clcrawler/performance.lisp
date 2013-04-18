(in-package :com.crawler)

(sb-profile:profile induce-blocks similar-blocks tokenize-html getblocks prune-fluff doc-tokens->df
		    tokens->tf tag-path reasonable-path-match tagblock-similarity collect-paths
		    cosine dot-product agree tokens->features partition-if hash-table->alist tag-html)

(sb-profile:profile write-induced-files write-html induce-wrappers mark-tag-tree ext-html mcar hash-table->alist prune-fluff tag-sameness structural-similarity-degree make-changes-to-page tag-add-class tag-remove-class-if reasonable-path-match average-sims blck-isdifferent)

(time (write-induced-files *imdb-title-page-strings*))

WARNING:
   Function AVERAGE-SIMS has been redefined, so times may be inaccurate.
PROFILE it again to record calls to the new definition.
  seconds  |     consed     |  calls  |  sec/call  |  name  
--------------------------------------------------------------
    86.642 | 14,770,151,408 |       8 |  10.830191 | MAKE-CHANGES-TO-PAGE
    11.356 |    632,853,640 |  46,664 |   0.000243 | PRUNE-FLUFF
     1.739 |    149,325,168 | 888,948 |   0.000002 | REASONABLE-PATH-MATCH
     1.155 |     43,378,640 |  23,332 |   0.000050 | TAG-SAMENESS
     0.792 |     37,647,272 |     682 |   0.001161 | EXT-HTML
     0.788 |     40,265,120 |  23,332 |   0.000034 | STRUCTURAL-SIMILARITY-DEGREE
     0.594 |    153,074,520 |       1 |   0.594423 | INDUCE-WRAPPERS
     0.421 |     25,522,000 |       8 |   0.052659 | MARK-TAG-TREE
     0.376 |        116,168 |     537 |   0.000700 | TAG-REMOVE-CLASS-IF
     0.171 |        593,416 |   2,851 |   0.000060 | TAG-ADD-CLASS
     0.076 |      4,090,664 |       8 |   0.009500 | WRITE-HTML
     0.028 |         12,288 |     537 |   0.000052 | BLCK-ISDIFFERENT
     0.003 |        608,080 |   1,664 |   0.000002 | HASH-TABLE->ALIST
     0.000 |              0 |       1 |   0.000000 | WRITE-INDUCED-FILES
--------------------------------------------------------------
   104.141 | 15,857,638,384 | 988,573 |            | Total

estimated total profiling overhead: 1.25 seconds
overhead estimation parameters:
  0.0s/call, 1.264e-6s total profiling, 3.52e-7s internal profiling

These functions were not called:
 AVERAGE-SIMS MCAR

Evaluation took:
  358.37 seconds of real time
  74.99269 seconds of user run time
  238.63492 seconds of system run time
  [Run times include 112.716 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  60,003,658,320 bytes consed.

Made some changes to class check.

Evaluation took:
  135.008 seconds of real time
  32.13001 seconds of user run time
  80.70504 seconds of system run time
  [Run times include 31.254 seconds GC run time.]
  0 calls to %EVAL
  0 page faults and
  18,262,125,008 bytes consed.

measuring PROFILE overhead..done
  seconds  |     consed     |   calls   |  sec/call  |  name  
----------------------------------------------------------------
    78.287 | 15,933,513,152 |         8 |   9.785907 | MAKE-CHANGES-TO-PAGE
    11.377 |  1,112,759,424 | 6,874,052 |   0.000002 | REASONABLE-PATH-MATCH
     7.830 |    584,038,416 |    55,388 |   0.000141 | PRUNE-FLUFF
     2.857 |    420,006,904 |         1 |   2.856649 | INDUCE-WRAPPERS
     2.382 |     44,781,232 |    27,694 |   0.000086 | TAG-SAMENESS
     1.826 |     26,597,352 |         8 |   0.228263 | MARK-TAG-TREE
     0.871 |     54,576,768 |     4,528 |   0.000192 | TAG-ADD-CLASS
     0.547 |     38,197,656 |    27,694 |   0.000020 | STRUCTURAL-SIMILARITY-DEGREE
     0.536 |        442,344 |     1,388 |   0.000386 | TAG-REMOVE-CLASS-IF
     0.268 |     37,754,560 |       682 |   0.000393 | EXT-HTML
     0.152 |      8,418,544 |        24 |   0.006333 | WRITE-HTML
     0.148 |        145,720 |     1,377 |   0.000107 | AVERAGE-SIMS
     0.040 |         16,560 |     1,388 |   0.000028 | BLCK-ISDIFFERENT
     0.000 |              0 |         1 |   0.000000 | WRITE-INDUCED-FILES
     0.000 |        876,376 |     4,459 |   0.000000 | HASH-TABLE->ALIST
----------------------------------------------------------------
   107.120 | 18,262,125,008 | 6,998,692 |            | Total

estimated total profiling overhead: 5.71 seconds
overhead estimation parameters:
  1.6000001e-8s/call, 8.16e-7s total profiling, 3.2e-7s internal profiling

These functions were not called:
 MCAR
; No value

(time (write-induced-files *imdb-title-pages*))
