(in-package :com.crawler)

(defstruct file-queue
  "The QUEUE data type."
  (path "/home/vb/repository/portfolio/crawler/data/crawldb/queue" :type sequence)
  (pos 0 :type integer)
  (size 0 :type integer))

(defun build-queue (&optional (path "/home/vb/repository/portfolio/crawler/data/crawldb/queue") (new t))
  (progn
    (when (and new (probe-file path))
      (delete-file path))
    (make-file-queue :path path)))

(defun empty-p (q)
  "Test whether a queue Q is empty."
  (zerop (file-queue-size q)))

(defun size (q)
  "Returns the current number of elements in the queue Q."
  (file-queue-size q))

(defun enqueue (item q)
  (with-open-file (out (file-queue-path q) :direction :output :if-exists :append :if-does-not-exist :create)
    (incf (file-queue-size q))
    (write-line item out)))

(defun dequeue (q)
  (if (empty-p q)
      nil
      (with-open-file (in (file-queue-path q))
	(let ((len (file-length in))
	      (line nil))
	  (unless (>= (file-queue-pos q) len)
	    (file-position in (file-queue-pos q))
	    (setf line (read-line in))
	    (decf (file-queue-size q))
	    (incf (file-queue-pos q) (1+ (length line))))
	  (values line len)))))
