(in-package :com.crawler)

(defparameter *elements-can-be-empty*
  '(:br :hr :img :meta :script :link
    :comment :style :object :iframe :!doctype :EMBED :ISINDEX :noscript))

(defparameter *included-link-elements*
  '(
    (:link . "href")
    (:img . "src")
    (:script . "src")
    (:frame . "src")
    (:input . "src")))
          
(defparameter *useful-link-elements*
  '(
    (:a          :href)
    (:area       :href)
    (:blockquote :cite)
    (:del        :cite)
    (:frame      :src)
    (:iframe     :src)
    (:ins        :cite)
    (:layer      :src)
    (:link       :href)
    (:q          :cite)
    (:xmp        :href)
    ))

(defparameter *content-elements*
  '(:title :p :h1 :h2 :h3 :h4 :h5 :h6 :tr :td :th :dt :dd :li :ul :ol :dl
    :dir :menu :pre :center :font :span :blockquote :table :address 
    :b :i :big :small :strong))

(defparameter *maybe-block-elements*
  '(:P :TR :UL :OL :HR :dl))

(defparameter *feature-elements*
  '(:h1 :h2 :h3 :h4 :h5 :h6 :span :b :I :big :small :strong))

(defparameter *definitely-non-content-block-elements*
  '(:script :comment :style :meta :object :form :iframe :link :!doctype :del :EMBED :ISINDEX :MENU :noscript))

(defparameter *maybe-non-content*
  '(:object :form :iframe :del :frame :EMBED :ISINDEX :MENU))

(defparameter *known-elements*
    '(:TT :I :B :BIG :SMALL :EM :STRONG :DFN :CODE :SAMP :KBD :VAR :CITE :ABBR
 :ACRONYM :A :IMG :OBJECT :BR :MAP :Q :SUB :SUP :SPAN :BDO :INPUT :SELECT
 :TEXTAREA :LABEL :BUTTON :FONT :!DOCTYPE :A :ACRONYM :ADDRESS :APPLET :AREA :B :BASE :BASEFONT :BDO :BGSOUND
 :BIG :BLINK :BLOCKQUOTE :BODY :BR :BUTTON :CAPTION :CENTER :CITE :CODE :COL
 :COLGROUP :COMMENT :DD :DEL :DFN :DIR :DIV :DL :DT :EM :EMBED :FIELDSET :FONT
 :FORM :FRAME :FRAMESET :H1 :H2 :H3 :H4 :H5 :H6 :HEAD :HR :HTML :I :IFRAME :IMG
 :INPUT :INS :ISINDEX :KBD :LABEL :LAYER :LEGEND :LI :LINK :LISTING :MAP
 :MARQUEE :MENU :META :MULTICOL :NOBR :NOFRAMES :NOSCRIPT :OBJECT :OL :otpgroup :OPTION
 :P :PARAM :PLAINTEXT :PRE :Q :SAMP :SCRIPT :SELECT :SMALL :SPACER :SPAN :S
 :STRIKE :STRONG :STYLE :SUB :SUP :TABLE :TBODY :TD :TEXTAREA :TFOOT :TH :THEAD
 :TITLE :TR :TT :U :UL :VAR :WBR :XMP :I :B :TT :BIG :SMALL :STRIKE :S :U :EM :STRONG :FONT))

(defparameter *formatting-style-elements*
  '(:TT :I :B :BIG :SMALL :EM :STRONG :DFN :CODE :SAMP :KBD :VAR :CITE :ABBR
    :ACRONYM :FONT :!DOCTYPE :BASEFONT :BLINK :BLOCKQUOTE :CENTER :CODE
    :EM :FONT :I :LAYER :MARQUEE :NOBR :NOFRAMES :PRE :Q :SAMP :SMALL :SPACER :S
    :STRIKE :STYLE :SUB :SUP :TFOOT :TH :THEAD :TT :U :WBR))

(defparameter *form-elements*
  '(:INPUT :SELECT :TEXTAREA :LABEL :BUTTON :FORM :FRAME :otpgroup :OPTION :hidden))

(defvar *tag-similarity-matrix* (hash :test 'equal))

(defun make-these-tags-similar (tags)
  (dolist (t1 tags)
    (dolist (t2 tags)
      (hash-put *tag-similarity-matrix* 
		(cons t1 t2) 
		(if (eql t1 t2)
		    1.0
		    0.9)))))

(defun load-tag-similarity ()
  (progn
    (make-these-tags-similar '(:TT :I :B :BIG :SMALL :EM :STRONG :CENTER :BLINK :BLOCKQUOTE :pre :q :plaintext :BIG :SMALL :spacer :STRIKE :S :U :EM :STRONG :NOBR :P :SPAN)) 
    (make-these-tags-similar '(:FONT :STYLE))
    (make-these-tags-similar '(:BODY :!DOCTYPE :HEAD :HTML :FRAME :FRAMESET))
    (make-these-tags-similar '(:APPLET :OBJECT :IFRAME :NOFRAMES :NOSCRIPT :EMBED))
    (make-these-tags-similar '(:INPUT :SELECT :TEXTAREA :LABEL :BUTTON :otpgroup :OPTION :HIDDEN))
    (make-these-tags-similar '(:FIELDSET :FORM))
    (make-these-tags-similar '(:IMG :MAP ))
    (make-these-tags-similar '(:CAPTION :CITE :CODE ))
    (make-these-tags-similar '(:Q :SUB :SUP :ADDRESS :AREA :BASE :BASEFONT :BGSOUND :DFN :CODE :SAMP :KBD :VAR :CITE :ABBR :ACRONYM))
    (make-these-tags-similar '(:SPAN :P :BR :H1 :H2 :H3 :H4 :H5 :H6 :HR :title))
    (make-these-tags-similar '(:script :comment :style :meta :object :form :iframe :link :!doctype :del :EMBED :ISINDEX :MENU :noscript))
    (make-these-tags-similar '(:P :TR :UL :OL :HR :dl :div :table))
    (make-these-tags-similar '(:h1 :h2 :h3 :h4 :h5 :h6 :span))
    (make-these-tags-similar '(:a :area :blockquote :del :frame :iframe :ins :layer :link :q :xmp))
    (make-these-tags-similar '(:DIR :MENU))
    (make-these-tags-similar '(:COL :COLGROUP :DT :MULTICOL))
    (make-these-tags-similar '(:LI :tr :td :tbody :tfoot :th :thead :table))))

(defun tag-similarity-degree (t1 t2)
  (let ((tag1 (if (stringp t1) (make-keyword t1) t1))
	(tag2 (if (stringp t2) (make-keyword t2) t2)))
    (aif (gethash (cons tag1 tag2) *tag-similarity-matrix*)
	 it
	 (if (eql tag1 tag2)
	     1.0
	     0.1))))
