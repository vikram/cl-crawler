(in-package :com.crawler)

(defvar eof (gensym))
; Files

(defmacro with-infile (var fname &rest body)
  (with-gensyms (v c f)
    `(let ((,f ,fname))
       (in-case-error
         (with-open-file (,var ,f :direction :input)
           ,@body)
         (format *error-output* "Error reading from ~s.~%" ,f)))))

(defmacro with-outfile (var fname &rest body)
  (with-gensyms (v c f)
    `(let ((,f ,fname))
       (in-case-error
         (with-open-file (,var ,f :direction :output
                                  :if-exists :supersede
				  :if-does-not-exist :create)
           ,@body)
         (format *error-output* "Error writing to ~s.~%" ,f)))))

(defmacro with-outfiles (pairs &rest body)
  (if (null pairs)
      `(progn ,@body)
      (if (oddp (length pairs))
          (error "Odd length arg to with-outfiles")
          `(with-outfile ,(car pairs) ,(cadr pairs)
             (with-outfiles ,(cddr pairs) ,@body)))))

(defun read-binary (filename byte-size)
  (let ((results '()))
    (with-open-file (in filename :element-type 'unsigned-byte)
      (do ((i (read-byte in nil -1)
	      (read-byte in nil -1)))
	  ((minusp i))
	(declare (fixnum i))
	(push i results)))
    (nreverse results)))

(defun write-binary (filename byte-size words)
  (with-open-file (out filename :direction :output :element-type 'unsigned-byte :if-exists :supersede :if-does-not-exist :create)
    (dolist (i words)
      (write-byte i out))))

#+sbcl
(defun read-line-restart (in eof-error-p)
  (handler-bind ((sb-int:stream-decoding-error
		  #'resync))
    (read-line in eof-error-p)))

#+sbcl
(defun resync (c)
  (declare (ignore c))
  (invoke-restart 'sb-int:attempt-resync))

(defun slurp-textfile (filename)
  "hacked as it doesn't know if the file is unicode or not"
  (with-open-file (stream filename)
    (let* ((seq (make-string (file-length stream))))
      (read-sequence seq stream)
      (subseq seq 0 (position #\Nul seq)))))

(defun slurp (filename)
  (with-open-file (stream filename :element-type 'unsigned-byte)
    (let ((seq (make-array (file-length stream) :element-type 'unsigned-byte :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

(defmacro with-read-line ((line file) &body body)
  (with-gensyms (in)
    `(with-open-file (,in ,file)
       (while-bind (,line (read-line ,in nil nil nil))
	 ,@body))))

(defvar *copy-file-verbose* t "Default verbosity setting for COPY-FILE.")
#+sbcl
(defun-exported copy-file (input output
                  &key (if-exists :error) (verbose *copy-file-verbose*))
  "Produce a copy of INPUT at OUTPUT.  INPUT must be a pathname                 
designator; OUTPUT must be a pathname designator other than a                   
stream.  OUTPUT will be merged with INPUT.  IF-EXISTS controls                  
the disposition of an existing output file.  If VERBOSE is true,                
print a message in the syle of a comment.  At present, COPY-FILE                
will do the wrong thing if INPUT is a sparse file, and will                     
silently ignore failure to preserve metadata."
  (setf output (merge-pathnames output input))
  (when verbose
    (format t "~&; Copying file ~A to ~A.~%" input output))
  (with-open-file (in (pathname input) :element-type '(unsigned-byte 8))
    (with-open-file (out output :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-exists if-exists)
      (let ((buffer (make-array 512 :element-type '(unsigned-byte 8))))
        (loop with end fixnum = 0
              always (plusp (setq end (read-sequence buffer in)))
              do (write-sequence buffer out :end end)))))
  (with-accessors ((mode sb-posix:stat-mode)
                   (owner sb-posix:stat-uid)
                   (group sb-posix:stat-gid)
                   (atime sb-posix:stat-atime)
                   (mtime sb-posix:stat-mtime))
      (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (sb-posix:stat input))
    (handler-case (sb-posix:chmod output (logandc1 sb-posix:s-ifmt mode))
      (sb-posix:syscall-error () nil))
    (handler-case (sb-posix:chown output owner group)
      (sb-posix:syscall-error () nil))
    (handler-case (sb-posix:utimes output atime mtime)
      (sb-posix:syscall-error () nil)))
  (values
   output
   (truename output)))
