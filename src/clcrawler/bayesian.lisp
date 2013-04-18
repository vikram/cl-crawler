(in-package :com.crawler)

(defstruct classifier total-outs total-ins features-string name filter domain classes hash)

(defun defclassifier (name classes ins outs feature-fn hostname-url)
  (multiple-value-bind (features total-in total-out) (funcall feature-fn ins outs)
    (make-classifier
     :name name
     :domain hostname-url
     :classes classes
     :features-string (with-output-to-string (out)
			(write-hashtable-stream out features))
     :fn (fn-name feature-fn)
     :total-ins total-in
     :total-outs total-out)))

;;classifier

(defparameter *max-out-score* .4)
(defparameter *min-in-score* .6)

(defstruct (word-feature (:type list)) word (in-count 0) (out-count 0))

(defun intern-feature (word features)
  (or (gethash word features)
      (setf (gethash word features)
            (make-word-feature :word word))))

(defun extract-features (sig features)
  (mapcar #'(lambda (pair) (intern-feature (car pair) features)) sig))

(defun increment-count (feature type)
  (ecase type
    (out (incf (word-feature-out-count feature)))
    (in (incf (word-feature-in-count feature)))))

(defun increment-total-count (type classifier)
  (ecase type
    (out (incf (classifier-total-outs classifier)))
    (in (incf (classifier-total-ins classifier)))))

(defun in-probability (feature classifier)
  "Basic probability that a feature with the given relative
frequencies will appear in a in assuming ins and outs are
otherwise equally probable. One of the two frequencies must be
non-zero."
  (let ((in-frequency (/ (word-feature-in-count feature) (max 1 (classifier-total-ins classifier))))
	(out-frequency (/ (word-feature-out-count feature) (max 1 (classifier-total-outs classifier)))))
    (/ in-frequency (+ in-frequency out-frequency))))


(defun bayesian-in-probability (feature classifier &optional
                                  (assumed-probability 1/2)
                                  (weight 1))
  "Bayesian adjustment of a given probability given the number of
data points that went into it, an assumed probability, and a
weight we give that assumed probability."
  (let ((basic-probability (in-probability feature classifier))
        (data-points (+ (word-feature-in-count feature) (word-feature-out-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

(defun score (features classifier)
  (let ((in-probs ()) (out-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((in-prob (float (bayesian-in-probability feature classifier) 0.0d0)))
          (push in-prob in-probs)
          (push (- 1.0d0 in-prob) out-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher in-probs number-of-probs))) 
	  (s (- 1 (fisher out-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun fisherize-furthest-n (n in-probs out-probs number-of-probs)
  (let ((furthest-n (subseq (sort
			     (mapa-b #'(lambda (i) (- (nth i in-probs) (nth i out-probs)))
				     0 
				     (1- number-of-probs))
			     #'(lambda (a b) (> (abs a) (abs b))))
			    0 (min n number-of-probs))))
    (values
     (- 1 (fisher (mapcar #'(lambda (k) (/ (1+ k) 2)) furthest-n) n))
     (- 1 (fisher (mapcar #'(lambda (k) (- 1 (/ (1+ k) 2))) furthest-n) n)))))
    
(defun untrained-p (feature)
  (and (zerop (word-feature-in-count feature)) (zerop (word-feature-out-count feature))))

(defun fisher (probs number-of-probs)
  "The Fisher computation described by Robinson."
  (inverse-chi-square 
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))

(defun inverse-chi-square (value degrees-of-freedom)
  "Probability that chi-square >= value with given degrees-of-freedom.
Based on Gary Robinson's Python implementation."
  (assert (evenp degrees-of-freedom))
  ;; Due to rounding errors in the multiplication and exponentiation
  ;; the sum computed in the loop may end up a shade above 1.0 which
  ;; we can't have since it's supposed to represent a probability.
  (min 
   (loop with m = (/ value 2)
      for i below (/ degrees-of-freedom 2)
      for prob = (exp (- m)) then (* prob (/ m i))
      summing prob)
   1.0))

(defun classification (score)
  (values
   (cond
     ((<= score *max-out-score*) 'out)
     ((>= score *min-in-score*) 'in)
     (t 'unsure))
   score))

(defun nshuffle-vector (vector)
  "Shuffle a vector in place using Fisher-Yates algorithm." 
  (loop for idx downfrom (1- (length vector)) to 1
        for other = (random (1+ idx))
        do (unless (= idx other)
             (rotatef (aref vector idx) (aref vector other))))
  vector)

(defun shuffle-vector (vector)
  "Return a shuffled copy of vector."
  (nshuffle-vector (copy-seq vector)))

(defun result-type (result)
  (destructuring-bind (&key type classification &allow-other-keys) result
    (ecase type
      (out
       (ecase classification
         (out 'correct)
         (in 'false-positive)
         (unsure 'missed-out)))
      (in
       (ecase classification
         (out 'false-negative)
         (in 'correct)
         (unsure 'missed-in))))))

(defun false-positive-p (result)
  (eql (result-type result) 'false-positive))

(defun false-negative-p (result)
  (eql (result-type result) 'false-negative))

(defun missed-out-p (result)
  (eql (result-type result) 'missed-out))

(defun missed-in-p (result)
  (eql (result-type result) 'missed-in))

(defun correct-p (result)
  (eql (result-type result) 'correct))

(defun analyze-results (results)
  (let* ((keys '(total correct false-positive 
                 false-negative missed-out missed-in))
         (counts (loop for x in keys collect (cons x 0))))
    (dolist (item results)
      (incf (cdr (assoc 'total counts)))
      (incf (cdr (assoc (result-type item) counts))))
    (loop with total = (cdr (assoc 'total counts))
          for (label . count) in counts
          do (format t "~&~@(~a~):~20t~5d~,5t: ~6,2f%~%"
                     label count (* 100 (/ count total))))))

(defun classify-signature (sig features classifier)
  (classification (score (extract-features sig features) classifier)))

(defun classifier-features (classifier)
  (with-input-from-string (in (classifier-features-string classifier))
    (read-hashtable-stream in)))

(defun signature (string)
  (word-stats (make-word-list string)))

(defmemoize mcorrelation #'equal (user nv sig len)
  (declare (ignore user))
  (let ((corr 0) (v (cdr nv)))
    (dotimes (x len)
      (setf corr (dot-product corr sig v x)))
    (* -1 corr)))

(defmemoize mfeature-vector #'equal (user big-signature signature)
  (declare (ignore user))
  (sort 
   (append signature (set-difference big-signature signature :key #'car :test #'string=))
   #'string< :key #'car))

(defun train (signature features type classifier)
  (dolist (feature (extract-features signature features))
    (increment-count feature type))
  (increment-total-count type classifier))
