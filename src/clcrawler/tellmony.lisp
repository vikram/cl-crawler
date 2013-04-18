(in-package :com.crawler)
;;tellmony prototype

(defun simple-iter (guess compound duration)
  (multiple-value-bind (enough? diff)
      (good-enough? guess duration compound)
    (if enough?
        guess
        (simple-iter (improve guess duration diff) compound duration))))

(defun improve (guess duration diff)
  (- guess (/ diff duration)))

(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough? (guess duration compound)
  (awhen (expt (+ 1 guess) duration)
    (values
     (< (abs (- it (+ 1 compound)))
        0.00001)
     (- it (+ 1 compound)))))

(defun simple(compound duration)
  (simple-iter (/ compound duration) compound duration))

(defun return-on (principal interest duration)
  (* principal (expt (+ 1 interest) duration)))

(defun compound (simple duration)
  (- (expt (+ 1 simple) duration) 1))

(defun schedule-deposits (regular-in duration compound &optional (simple (simple compound duration)) (results '()))
  (if (zerop duration)
      (values
       (reduce #'+ results :key #'cdr)
       (nreverse results))
      (let ((installment (return-on regular-in simple duration)))
        (schedule-deposits
         regular-in
         (1- duration)
         nil
         simple
         (cons (cons duration installment) results)))))

(defun reducing-deposit (initial duration compound
                         &optional
                         (regular-out (/ initial duration))
                         (simple (simple compound duration))
                         (interest 0.0))
  (if (zerop duration)
      interest
      (reducing-deposit
       (- initial regular-out)
       (1- duration)
       nil
       regular-out
       simple
       (+ interest (- (return-on (- initial regular-out) simple 1)
                      (- initial regular-out))))))

(defun move-from-savings->regular (amount duration regular-rate savings-rate)
  (+ (reducing-deposit amount duration savings-rate)
     (schedule-deposits (/ amount duration) duration regular-rate)))

(list
 (list :halifax 6000 (move-from-savings->regular 6000 12 .1 .0525) (* 6000 (1+ .0525)))
 (list :abbey 3000 (move-from-savings->regular 3000 12 .1 .0525) (* 3000 (1+ .0525)))
 (list :first-direct 3600 (move-from-savings->regular 3600 12 .08 .0525) (* 3600 (1+ .0525)))
 (list :norwich 3000 (move-from-savings->regular 3000 12 .08 .0525) (* 3000 (1+ .0525)))
 (list :barclays 3000 (move-from-savings->regular 3000 12 .0775 .0525) (* 3000 (1+ .0525))))
