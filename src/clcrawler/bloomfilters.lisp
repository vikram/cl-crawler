(in-package :com.crawler)
;;;;;bloom filters

(defclass bloom-filter ()
  ((bitmap :accessor bitmap :initform 0 :initarg :bitmap)
   (hash-functions :accessor hash-functions :initform nil :initarg :hash-functions)
   (size :accessor bsize :initform nil :initarg :size)))

(defun make-filter (&key (size 1000)
                         (salts '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
                                  "q" "r" "s" "u" "v" "w" "x" "y" "z" ":" "/" "." "0"  "1" "2" "3" "4"  "5" "6" "7" "8" "9" "%" "?" "&")))
  (make-instance 'bloom-filter
                 :size size
                 :hash-functions (make-hashing-functions salts)))

(defmethod add ((self bloom-filter) key)
  "Adds a key to a Bloom filter."
  (setf (bitmap self)
    (logior (bitmap self)
            (make-bitmap self key))))

(defmethod contains ((self bloom-filter) key)
  "Returns T if the specified key is in the Bloom filter."
  (let ((bitmap (make-bitmap self key)))
    (= (logand (bitmap self) bitmap) bitmap)))

(defmethod make-bitmap ((self bloom-filter) key)
  (flet ((subproduct (index hash-seq)
           (reduce #'(lambda (a b) (logior (ash a 8) b))
                   hash-seq
                   :start index :end (+ index 4))))
    (let ((vector 0))
      (dolist (hash-function (hash-functions self))
        (let* ((hash-bytes (funcall hash-function key))
               (hashes (list (subproduct 0 hash-bytes)
                             (subproduct 4 hash-bytes)
                             (subproduct 8 hash-bytes)
                             (subproduct 12 hash-bytes)
                             (subproduct 16 hash-bytes)))
               (combined-hash (apply #'logxor hashes)))
          (let ((index (mod combined-hash (bsize self))))
            (setf (ldb (byte 1 index) vector) 1))))
      vector)))

(defun make-hashing-functions (salts)
  (mapcar
   #'(lambda (salt)
       #'(lambda (seq)
           (ironclad:digest-sequence
            :sha1
            (ironclad:ascii-string-to-byte-array (concatenate 'string salt seq)))))
   salts))

