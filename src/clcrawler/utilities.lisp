(in-package :com.crawler)

(defun string->hex (str)
  (with-output-to-string (out)
    (dotimes (i (length str))
      (let ((c (char str i)))
        (format out "~2,'0X" (char-code c))))))

(defun hex->string (str)
  (if (evenp (length str))
      (with-output-to-string (out)
        (let ((*read-base* 16))
          (dotimes (i (/ (length str) 2))
            (princ (code-char (read-from-string str  nil nil
                                                :start (* i 2)
                                                :end (+ (* i 2) 2)))
                   out))))
      (error (format nil "odd-length hex string: ~A" str))))

;;;; ********************************************************************************
;;;; Section 4.7, Utility Functions: Symbols and Strings
;;;; ********************************************************************************

(defun make-alist (keys values)
  (let ((results nil))
    (dotimes (x (length keys))
      (push (cons (elt keys x) (elt values x)) results))
    results))

(defun mapcar-filter (fn list predicate)
  (mapcar fn (remove-if-not predicate list)))

(defun page-start-end (members start end)
  (let ((len (length members)))
    (cond ((>= len end) (list (1- start) end))
          ((< len start) (list 0 0))
          (t (list (1- start) len)))))

(defun safe-subseq (seq start end)
  (aif (page-start-end seq start end)
       (subseq seq (first it) (second it))))

(defun memoize (fn name &key (test #'eql))
  (declare (function fn))
  (let ((cache (make-hash-table :test test)))
    (setf (get name 'memo) cache)
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))

(defmacro defmemoize (name test args &body body)
  "Create a function which memoizes its arguments."
  (let ((nm (gensym)))
   `(flet ((,nm ,args ,@body))
      (setf (get ',name 'test) ,test)
      (setf (symbol-function ',name) (memoize (function ,nm) ',name :test ,test)))))

(defun clear-memoize (fn-name)
  (awhen (get fn-name 'memo)
    (clrhash it)))

(defun agree (x y)
  (if (or (null x) (null y))
      t
      (if (equal (car x) (car y))
          (agree (cdr x) (cdr y)))))

(defun assocify (source)
  (labels ((rec (source acc)
             (let ((rest (cddr source)))
               (if (consp rest)
                   (rec rest (cons (cons (car source) (cadr source)) acc))
                   (nreverse (cons (cons (car source) (cadr source)) acc))))))
    (if source (rec source nil) nil)))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun careql (x y)
  (and (consp x) (eql (car x) y)))

(defmacro carin (x &rest args)
  (with-gensyms (g)
    `(let ((,g ,x))
       (and (consp ,g) (in (car ,g) ,@args)))))

(defun carat (x)
  (if (consp x) (car x) x))

(define-modify-macro pushend (obj)
  (lambda (place obj)
    (nconc place (list obj))))

(define-modify-macro merge-into (obj fn)
  (lambda (place obj fn)
    (merge 'list place (list obj) fn)))

(defun rotlist (x)
  (if (cdr x)
      (cons (car (last x)) (butlast x))
      x))

(defun skip (seq &key (step 0))
  (mapa-b #'(lambda (i) (nth i seq)) 0 (1- (length seq)) step))

(defun trec (rec &optional (base #'identity) (leaf #'atom))
  (declare (function rec))   ; LMH
  "Tree traverser: Recurse down a tree.  Rec is a function
   that takes three arguments, the first is the tree,
   the second is the result of the left branch,
   the third is the result of the right branch.  Base is a function called
   or value returned if at a leaf.  Differs from ttrav in that
   it need not traverse the whole tree. "   ; LMH
  (labels
    ((self (tree)
       (if (funcall leaf tree)
           (if (functionp base)
               (funcall base tree)
               base)
           (funcall rec tree
                        #'(lambda ()
                            (self (car tree)))
                        #'(lambda ()
                            (if (cdr tree)
                                (self (cdr tree))))))))
    #'self))

;;; Example for rfind-if:
;;;   (trec #'(lambda (o l r) (or (funcall l) (funcall r)))
;;;         #'(lambda (tree) (and (oddp tree) tree))))

(defmacro atrec (rec &optional (base 'it) (leaf 'atom))
  "Anaphoric tree recursion: current tree is 'it, left subtree is 'left
   right subtree is 'right."   ; LMH
  (let ((lfn (gensym)) (rfn (gensym)))
    `(trec #'(lambda (it ,lfn ,rfn)
               (declare (ignorable it) (function ,lfn ,rfn))   ; LMH
               (#+cltl2 symbol-macrolet #+symbolics clos:symbol-macrolet ((left (funcall ,lfn))
                                 (right (funcall ,rfn)))
                 ,rec))
           #'(lambda (it) (declare (ignorable it)) ,base)
           #'(lambda (it) (declare (ignorable it)) (,leaf it)))))

(defmacro on-trees (rec base leaf &rest trees)
  "Anaphoric tree recursion, for defining named functions"   ; LMH
  `(funcall (atrec ,rec ,base ,leaf) ,@trees))

(defun fint (fn &rest fns)
  (declare (function fn))                       ; LMH
  "Function intersection: a function that is the
   AND of each function called on the argument."   ; LMH
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        (declare (function chain))              ; LMH correct?
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

(defun lrec (rec &optional base)
  (declare (function rec))                      ; LMH
  "Function to define flat list recurser.
   Rec is a function that takes two arguments, the first
   will be the car of the list, the second is a function
   that will produce the cdr.  Base is a function to be called
   or value when the list is empty.
   For example, a length function could be defined as
   (lrec #'(lambda (x f) (1+ (funcall f))) 0)."   ; LMH
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                              #'(lambda ()
                                  (self (cdr lst)))))))
    #'self))

(defmacro alrec (rec &optional base)
  "Anaphoric list recurser (lrec): use `it' to refer to the current
   car of the list, and `rec' to the function rec itself.
   every on #'oddp,
   (alrec (and (oddp it) rec) t) is the equivalent of
   (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)."   ; LMH
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
               (declare (ignorable it) (function ,gfn))   ; LMH
               (#+cltl2 symbol-macrolet #+symbolics clos:symbol-macrolet ((rec (funcall ,gfn)))
                 ,rec))
           ,base)))

(defmacro on-cdrs (rec base &rest lsts)
  "Anaphoric list recursion, for defining named functions,
   e.g., (defun our-every (fn lst) (on-cdrs (and (funcall fn it) rec) t lst))."   ; LMH
  `(funcall (the function (alrec ,rec #'(lambda () ,base))) ,@lsts))   ; LMH the function

(defun find2 (fn lst)
  "Find the first element of lst that satisfies fn, returning
   both the element and result of fn.  Like find, but also returns
   the result of calling fn."   ; LMH
  (declare (function fn))   ; LMH
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  "Test if x occurs before y in lst.
   Returns true if second element doesn't occur at all."   ; LMH
  (declare (function test))   ; LMH
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  "Test if x occurs after y in lst."   ; LMH
  (declare (function test))   ; LMH
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun most (scoring-fn lst)
  "Return the element and the score that returns the highest
   values of the scoring-fn.  Scoring-fn is a function that
   returns a fixnum."   ; LMH
  (declare (function scoring-fn))                       ; LMH
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall scoring-fn wins)))
        (declare (fixnum max))                  ; LMH scoring-fn must return fixnum
        (dolist (obj (cdr lst))
          (let ((score (funcall scoring-fn obj)))
            (declare (fixnum score))            ; LMH scoring-fn must return fixnum
            (when (> score max)
              (setq wins obj
                    max  score))))
        (values wins max))))

(defun best (predicate lst)
  "The element of lst for which the predicate always returns t when called
   with other elements in lst, like (car (sort lst predicate)) but
   potentially more efficient."   ; LMH
  (declare (function predicate))   ; LMH
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall predicate obj wins)
              (setq wins obj)))
        wins)))

(defun mostn (scoring-fn lst)
  "Return a list of all elements and the score that return the highest
   values of the scoring-fn.  Scoring-fn is a function that
   returns a fixnum."   ; LMH
  (declare (function scoring-fn))                       ; LMH
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall scoring-fn (car lst))))
        (declare (fixnum max))                  ; LMH scoring-fn must return fixnum
        (dolist (obj (cdr lst))
          (let ((score (funcall scoring-fn obj)))
            (declare (fixnum score))            ; LMH scoring-fn must return fixnum
            (cond ((> score max)
                   (setq max    score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

(defun freqs (seq &key (test #'eql) (key #'identity))
  "Return an alist of (num . freq) of elements of the SEQ.
The alist is sorted by decreasing frequencies. TEST defaults to `eql'."
  (declare (sequence seq) (type (function (t t) t) test)
           (type (function (t) t) key))
  (unless (null seq)
    (sort
     (reduce (lambda (res el)
               (let ((fi (assoc el res :test test)))
                 (cond (fi (incf (cdr fi)) res) ((acons el 1 res)))))
             seq :key key :initial-value nil)
     #'> :key #'cdr)))

(defun-exported same (lst &key (key #'identity) (test #'eql))
  (if (or (not lst) (not (rest lst)))
      t
      (and (funcall test
                    (funcall key (first lst))
                    (funcall key (second lst)))
           (same (cdr lst) :key key :test test))))

(defun common (lst1 lst2 &key (test #'eql) (acc '()))
  (if (or (not lst1) (not lst2))
      (nreverse acc)
      (if (funcall test (first lst1) (first lst2))
          (common (rest lst1) (rest lst2) :test test :acc (cons (first lst1) acc))
          (nreverse acc))))

(defun group (source &key (key #'car) (rest #'cdr) (test #'eql) (accumulator '()))
  (if (null source)
      accumulator
      (let* ((rest-of-them (remove-if-not #'(lambda (a)
                                              (funcall test
                                                       (funcall key a)
                                                       (funcall key (first source))))
                                          (rest source)))
             (grouped (cons (funcall key (first source))
                            (cons (funcall rest (first source))
                                  (mapcar rest rest-of-them)))))
        (group (set-difference source (cons (first source) rest-of-them))
               :key key :rest rest :test test :accumulator (push grouped accumulator)))))

(defun-exported groupn (source n &optional (acc '()))
  (if (null source)
      (nreverse acc)
      (if (null (nthcdr n source))
          (nreverse (cons source acc))
          (groupn
           (nthcdr n source)
           n
           (cons (subseq source 0 n) acc)))))

(defun groupn-consec (source n &optional (acc '()))
  (if (null source)
      (nreverse acc)
      (if (null (nthcdr n source))
          (nreverse (cons source acc))
          (groupn-consec
           (cdr source)
           n
           (cons (subseq source 0 n) acc)))))

(defun group-by-nth (source n &optional (counter 0) (accumulator '()))
  (if (or (null source)
          (atom source))
      (mapcar #'reverse accumulator)
      (progn
        (aif (nth (mod counter n) accumulator)
             (setf (nth (mod counter n) accumulator) (cons (car source) (nth (mod counter n) accumulator)))
             (push (list (car source)) accumulator))
        (group-by-nth (cdr source) n (+ 1 counter) accumulator))))

(defun split-time (time)
  (declare (simple-string time) (optimize (speed 3) (safety 0)))
  (let* ((len (length time))
         (pos (if (= len 5)
                  2
                  (if (= len 3)
                      1
                      (position #\: time)))))
    (values (subseq time 0 pos) (subseq time (1+ pos) len))))

(defun find-duplicates (sequence &key (key #'identity) (test #'eql))
  (labels ((dupes (item seq acc)
             (if (not seq)
                 acc
                 (progn
                   (when (member (funcall key item) seq :key key :test test)
                     (push item acc)
                     (print item t))
                   (dupes (car seq) (cdr seq) acc)))))
    (dupes (car sequence) (cdr sequence) '())))

#+sbcl
(sb-impl::defmacro-mundanely casetest (keyform test &body cases)
  (sb-impl::case-body 'case keyform cases t test nil nil nil))

(defun longer (x y)
  "Test if list x is longer than list y.  More efficient
   than calling length twice."   ; LMH
  (declare (sequence x y))   ; LMH
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun topn (lst n &optional (acc '()))
  (if (> n 0)
      (aif (cdr lst)
           (topn it (1- n) (cons (car lst) acc))
           (nreverse (cons (car lst) acc)))
      (nreverse acc)))

(defun all-members (items list &key (test #'eql))
  (remove-empty
   (mapcar
    #'(lambda (item) (first (member item list :test test)))
    items)))

(defun remove-adjacent-if-not (predicate sequence &optional (acc '()) (found nil))
  (if (not sequence)
      acc
      (if (funcall predicate (first sequence))
          (remove-adjacent-if-not predicate (rest sequence) (cons (first sequence) acc) t)
          (if found
              acc
              (remove-adjacent-if-not predicate (rest sequence) acc nil)))))

(defun combinations (bag)
  (if (null bag)
      '(())
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (c) (cons e c))
                          (combinations (cdr bag))))
              (car bag))))

(defun permutations (bag)
  (mapcan #'(lambda (y)
              (mapcar #'(lambda (x) (list x y))
                      (remove y bag :test #'equal)))
          bag))

(defun pairs (lst &optional (acc nil))
  (if (null lst)
      acc
      (if (second lst)
          (pairs (rest lst) (cons (cons (first lst) (second lst)) acc))
          acc)))

(defun quicksort (vec l r)
  (let ((i l)
        (j r)
        (p (svref vec (round (+ l r) 2)))) ; pivot
    (while (<= i j)
      (while (< (svref vec i) p) (incf i))
      (while (> (svref vec j) p) (decf j))
      (when (<= i j)
        (rotatef (svref vec i) (svref vec j))
        (incf i)
        (decf j)))
    (if (> (- j 1) 1) (quicksort vec l j))
    (if (> (- r i) 1) (quicksort vec i r)))
  vec)

(defun simple-listp (lst)
  (and (consp lst)
       (null (remove-if-not #'listp lst))))

(defun mapor (fn lst)
  (or (funcall fn (first lst))
      (and (rest lst) (mapor fn (rest lst)))))

(defun remove-quotes (x) (subseq x 1 (1- (length x))))

(defmacro funcall-if (fn arg)
    (once-only (fn)
               `(if ,fn (funcall ,fn ,arg) ,arg)))

(defmacro read-time-case (first-case &rest other-cases)
    "Do the first case, where normally cases are
  specified with #+ or possibly #- marks."
    (declare (ignore other-cases))
    first-case)

(defun rest2 (x)
    "The rest of a list after the first TWO elements."
    (rest (rest x)))

(defun find-anywhere (item tree)
    "Does item occur anywhere in tree?"
    (if (atom tree)
        (if (eql item tree) tree)
        (or (find-anywhere item (first tree))
            (find-anywhere item (rest tree)))))

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun partition-if (pred list)
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

(defun maybe-add (op exps &optional if-nil)
  "For example, (maybe-add 'and exps t) returns
  t if exps is nil, exps if there is only one,
  and (and exp1 exp2...) if there are several exps."
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))

(defun seq-ref (seq index)
  "Return code that indexes into a sequence, using
  the pop-lists/aref-vectors strategy."
  `(if (listp ,seq)
       (prog1 (first ,seq)
              (setq ,seq (the list (rest ,seq))))
       (aref ,seq ,index)))

(defun maybe-set-fill-pointer (array new-length)
  "If this is an array with a fill pointer, set it to
  new-length, if that is longer than the current length."
  (if (and (arrayp array)
           (array-has-fill-pointer-p array))
      (setf (fill-pointer array)
            (max (fill-pointer array) new-length))))

(defun last1 (list)
  "Return the last element (not last cons cell) of list"
  (first (last list)))

(defun sort* (seq pred &key key)
  "Sort without altering the sequence"
  (sort (copy-seq seq) pred :key key))

(defun length=1 (x)
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun rest3 (list)
  "The rest of a list after the first THREE elements."
  (cdddr list))

(defun unique-find-if-anywhere (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-if-anywhere
        predicate
        (first tree)
        (unique-find-if-anywhere predicate (rest tree)
                                 found-so-far))))

(defun find-if-anywhere (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (first tree))
          (find-if-anywhere predicate (rest tree)))))

(defun not-null (x) (not (null x)))

(defun first-or-nil (x)
  "The first element of x if it is a list; else nil."
  (if (consp x) (first x) nil))

(defun first-or-self (x)
  "The first element of x, if it is a list; else x itself."
  (if (consp x) (first x) x))

(defun range1-n (n &optional (step 1) (start 1))
  (map0-n #'(lambda (x) (format nil "~A" (* step (+ x start)))) (- n start)))

(defun range (i j)
  (range1-n j 1 i))

(defun find-array (array value &key (test #'equal))
  (dotimes (i (length array))
    (when (funcall test value (aref array i))
      (return i))))

(defun freq (lst)
  (let ((V (make-counter)))
    (dolist (elem lst)
      (incf-counter v elem))
    V))

(defun nsublist (lst &optional pos0 pos1)
  "Return the part of the list between pos0 and pos1, *destructively*.
The indexing starts from 0, so (nsublist '(1 2 3 4 5) nil 2) ==> (1 2 3)."
  (declare (list lst))
  (when pos1 (let ((cut (nthcdr pos1 lst))) (when cut (setf (cdr cut) nil))))
  (if pos0 (nthcdr pos0 lst) lst))

(defun sort-n (list n &key (filter #'identity) (sort-fn #'>))
  (declare (sequence list))
  (nsublist (remove-if-not filter (sort list sort-fn :key #'cdr)) nil n))

(defun avg (lst)
  (if (null lst)
      0.0
      (float (/ (reduce #'+ lst) (length lst)))))

(defun-exported all-files (folder &key (full nil))
  (mapcar #'(lambda (path)
              (if full
                  path
                  (car (last (pathname-directory path)))))
          (directory (make-pathname :name :wild :type :wild :defaults
                                    folder))))
