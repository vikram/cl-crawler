(in-package :com.crawler)

(defun get-posix-details (file)
  (let ((results nil))
    (with-open-file (in file :direction :input)
      (do ((line (read-line in nil :eof nil)
		 (read-line in nil :eof nil)))
	  ((eql line :eof))
	(when (not (string= "#" (subseq line 0 1)))
	  (push (cl-ppcre:split ":" line) results))))
    results))

(defun get-posix-id (file)
  #'(lambda (string)
      (parse-integer 
       (third
	(car 
	 (remove-if-not #'(lambda (details) (string= string (car details))) (get-posix-details file)))))))

(defun get-posix-user-id (user)
  (funcall (get-posix-id "/etc/passwd") user))

(defun get-posix-group-id (group)
  (funcall (get-posix-id "/etc/group") group))

(uffi:def-function ("gethostname" c-gethostname)
    ((name (* :unsigned-char))
     (len :int))
  :returning :int)

(defun gethostname ()
  "Returns the hostname"
  (uffi:with-foreign-object (name '(:array :unsigned-char 256))
    (if (zerop (c-gethostname (uffi:char-array-to-pointer name) 256))
	(uffi:convert-from-foreign-string name)
	(error "gethostname() failed."))))

