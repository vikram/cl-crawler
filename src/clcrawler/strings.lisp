(in-package :com.crawler)

(defun join-with (lst str)
  (if lst
      (reduce #'(lambda (string el) (format nil "~A~A~A" string str el)) lst)
      ""))

(defun join (lst str)
  (join-with lst str))

(defun replaced-string-length (str repl-alist)
  (declare (simple-string str)
           (optimize (speed 3) (safety 0) (space 0)))
    (do* ((i 0 (1+ i))
          (orig-len (length str))
          (new-len orig-len))
         ((= i orig-len) new-len)
      (declare (fixnum i orig-len new-len))
      (let* ((c (char str i))
             (match (assoc c repl-alist :test #'char=)))
        (declare (character c))
        (when match
          (incf new-len (1- (length
                             (the simple-string (cdr match)))))))))

(defun substitute-chars-strings (str repl-alist)
  "Replace all instances of a chars with a string. repl-alist is an assoc
list of characters and replacement strings."
  (declare (simple-string str)
           (optimize (speed 3) (safety 0) (space 0)))
  (do* ((orig-len (length str))
        (new-string (make-string (replaced-string-length str repl-alist)))
        (spos 0 (1+ spos))
        (dpos 0))
      ((>= spos orig-len)
       new-string)
    (declare (fixnum spos dpos) (simple-string new-string))
    (let* ((c (char str spos))
           (match (assoc c repl-alist :test #'char=)))
      (declare (character c))
      (if match
          (let* ((subst (cdr match))
                 (len (length subst)))
            (declare (fixnum len)
                     (simple-string subst))
            (dotimes (j len)
              (declare (fixnum j))
              (setf (char new-string dpos) (char subst j))
              (incf dpos)))
        (progn
          (setf (char new-string dpos) c)
          (incf dpos))))))

(defun substitute-string-for-char (procstr match-char subst-str)
  "Substitutes a string for a single matching character of a string"
  (substitute-chars-strings procstr (list (cons match-char subst-str))))

(defun add-sql-quotes (s)
  (if s
      (substitute-string-for-char s #\' "''")
      ""))

(let ((cache-vec (make-array 10 :adjustable t)) diag)
  (defun edit-distance (s1 s2 &key (test #'eql))
    "The edit (Levenshtein) distance between strings
 (actually, arbitrary vectors).
See <http://www.merriampark.com/ld.htm>
<http://www.cut-the-knot.org/do_you_know/Strings.shtml>."
    (let ((l1 (length s1)) (l2 (length s2)))
      (unless (>= (length cache-vec) l1)
        (adjust-array cache-vec l1))
      (setq diag 0)
      (loop :for i :from 0 :below l1 :do (setf (aref cache-vec i) (1+ i)))
      (loop :for j :from 0 :below l2 :and c2 :across s2 :do
         (loop :for i :from 0 :below l1 :and c1 :across s1
            :for old = (aref cache-vec i) :do
            (shiftf diag (aref cache-vec i)
                (min (if (funcall test c1 c2) diag (1+ diag))
                     (1+ (aref cache-vec i))
                     (1+ (if (zerop i) (1+ j) (aref cache-vec (1- i))))))))
      (aref cache-vec (1- l1)))))

(defun mkstr (&rest args)
  "Make a string out of the printed representations of the arguments."   ; LMH
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun s+ (&rest args)
  (apply #'concatenate 'string args))

(defun str-butlast (str)
  (subseq str 0 (1- (length str))))

(defun string-empty (str)
  (when (stringp str)
    (or
     (string= "" str)
     (string= "" (trim str)))))

