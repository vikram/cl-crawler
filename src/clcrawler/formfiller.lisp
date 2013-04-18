(in-package :com.crawler)

;Page -> [(element, domain) ]

;selection lists, text boxes, text areas, checkboxes, or radio buttons 
;domain is the values associated with the element


(defun form-definition (form-html)
  (let* ((form (extract form-html :form))
	 (method (extract-next-method form))
	 (action (extract-next-action form))
	 (elements (cdr form-html))
	 (partial-elements (append
			    (remove-if 
			     #'not
			     (mapcar #'(lambda (input)
					 (casequal (string-downcase (extract-next-type input))
						   ("submit" nil)
						   ("image" nil)
						   (t (list (extract-next-name input) 							    
							    (if (string= "HIDDEN" (string-upcase (extract-next-type input)))
								(extract-next-name input) 
								(extract-label form-html (extract-next-id input)))
							    (extract-next-value input)
							    (intern (string-upcase (extract-next-type input)) "KEYWORD")))))
				     (extract-all elements #'(lambda (x) (eq x :input)))))
			    (mapcar #'(lambda (select)
					(let ((s (extract select :select)))
					  (list (extract-next-name s)
						(extract-label form-html (extract-next-id s))
						(mapcar #'(lambda (x) (or (extract-next-value x)
									  (extract-next-option x)))
				  (extract-options select))
						:select)))
				    (extract-lookahead-selects elements))))
	 (results '()))
    (dolist (element partial-elements)
      (if (second element)
	  (progn
	    (unless (listp (second element))
	      (setf (second element) (split-sequence:split-sequence #\space (trim-funny (replace-funny (string-upcase (second element)))))))
	    (push element results))
	  (let ((candidates (label-element-candidates (table-layout (table-for-element form-html element)))))
	    (awhen (find (first element) candidates :test #'string= :key #'first)
	      (setf (second element) (split-sequence:split-sequence #\space (trim-funny (replace-funny (string-upcase (second it)))))))
	    (push element results))))
    (values
     method
     action
     (nreverse results))))
	
(defparameter *lvs* '())

(defstruct (label-value (:type vector)) label (domain :infinite) keywords)

(defun make-label-domain (label domain &rest keywords)
  (push (make-label-value :label label
			  :domain domain
			  :keywords (mapcar #'string-upcase keywords))
	*lvs*))

(defun load-property-lvs ()
  (progn
    (make-label-domain :bedrooms (range1-n 10) 
		       "Bedrooms" "Beds" "Bed" "Bedroom")
    (make-label-domain :price (append (range1-n 10 100000) (rest (range1-n 10 1000000)))
		       "Price" "Cost")
    (make-label-domain :tenure '("Cottage" "Flat" "Maisonette" "Terraced" "End Terrace" "Semi-detached" "Detached" "Bungalow")
		       "Type" "Tenure")
    (make-label-domain :receptions (range1-n 10)
		       "Receptions" "Recption" "Rooms" "Room" "Rec.*")
    (make-label-domain :area :infinite
		       "Area" "Regions" "Office")))

(load-property-lvs)

(defparameter *min-keywords* (mapcar #'string-upcase '("Minimum" "Min" "Start" "Starting")))
(defparameter *max-keywords* (mapcar #'string-upcase '("Maximum" "Max" "End" "Ending")))

(defun extract-domain-select (select)
  (awhen (split-sequence:split-sequence #\space (trim-funny (replace-funny (string-upcase (second select)))))
    (mapor #'(lambda (val) (find val *lvs* 
				 :key #'(lambda (lvs) (label-value-keywords lvs))
				 :test #'(lambda (x l) (member x l :test #'string=))))
		      it)))

(defun formdef->params (formdef)
  (let ((hiddens (remove-if-not #'(lambda (input) (eql :hidden (car (last input)))) formdef))
	(texts (remove-if-not #'(lambda (input) (eql :text (car (last input)))) formdef))
	(selects (remove-if-not #'(lambda (input) (eql :select (car (last input)))) formdef)))
    (values
     (subseq
      (apply #'concatenate 'string 
	     (append
	      (mapcar #'(lambda (input)
			  (format nil "&~A=~A" (first input) (third input)))
		      hiddens)
	      (mapcar #'(lambda (input)
			  (when (and (third input) (stringp (third input)))
			    (format nil "&~A=~A" (first input) (third input))))
		      texts)))
      1)
     (mapcar #'(lambda (select)
		 (setf (second select)
		       (if (mapor #'(lambda (val) (member val *max-keywords* :test #'string=)) (second select))
			   :maximum
			   (if (mapor #'(lambda (val) (member val *min-keywords* :test #'string=)) (second select))
			       :minimum
			       nil)))
		 select)
	     selects))))

(defun create-url-from-parameter (url parameter value)
  (if (cl-ppcre:all-matches "[=\\?]" url)
      (format nil "~A&~A=~A" url parameter value)
      (format nil "~A?~A=~A" url parameter value)))

(defun generate-urls-for-this-form (page url)
  (let ((form (extract-lookahead page #'(lambda (x) (and (eql (car x) :form) (string= (third x) (url-relative url))))))
	(urls '()))
    (dolist (select (extract-lookahead form #'(lambda (x) (eql (car x) :select))))
      (let ((name (attval (extract select :select) :name)))
	(dolist (option (cdr select))
	  (when (string/= "" (third (car option)))
	    (push (CREATE-URL-FROM-PARAMETER url name (attval (extract option :option) :value)) urls)))))
    urls))
