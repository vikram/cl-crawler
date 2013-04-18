

(defparameter *test-example0*
  (from-example
   "http://www.showcasecinemas.co.uk/listings/showcase_printable.php?wantedSite=lbi"
   '(:record :cinema
     (:name "Showcase Cinemas Leeds")
     (:phone "0871 220 1000")
     (:loop
      (:first (:record :film
                       (:name "Hancock")
                       (:rating "12A")
                       (:duration (:pattern :mins "92 mins"))
                       (:loop
                        (:first (:record :day-listing
                                         (:day (:pattern :day "Thursday"))
                                         (:date (:pattern :date "10th July"))
                                         (:loop
                                          (:first (:record :times
                                                           (:time (:pattern :time "11:30"))))
                                          (:next (:record :times
                                                          (:time (:pattern :time "12:05")))))))
                        (:next (:record :day-listing
                                        (:day (:pattern :day "Friday"))
                                        (:date (:pattern :date "11th July"))
                                        (:as-above))))))
      (:next (:record :film
                      (:name "Kung Fu Panda")
                      (:rating "PG")
                      (:duration (:pattern :mins "92 mins"))
                      (:as-above)))))
   :extracter (make-elmtext)))

(5am:test learn-template
  (5am:is (not
           (find :error
                 (flatten
                  (from-example
                   "http://www.showcasecinemas.co.uk/listings/showcase_printable.php?wantedSite=lbi"
                   *test-example0*
                   :extracter #'elmwrapper))))))

(defparameter *test-example1*
  (from-example
   "http://www.showcasecinemas.co.uk/listings/showcase_printable.php?wantedSite=lbi"
   '(:loop
     (:first (:record :times
              (:time (:pattern :time "11:30"))))
     (:next (:record :times
             (:time (:pattern :time "12:05")))))
   :extracter (make-elmtext)))

(defparameter *test-example2*
  (from-example
   "http://www.showcasecinemas.co.uk/listings/showcase_printable.php?wantedSite=lbi"
   '(:loop
     (:first (:record :day-listing
              (:day (:pattern :day "Thursday"))
              (:date (:pattern :date "10th July"))))
     (:next (:record :day-listing
             (:day (:pattern :day "Friday"))
             (:date (:pattern :date "11th July")))))
   :extracter (make-elmtext)))

(defparameter *test1*
  (get-page "http://www.showcasecinemas.co.uk/listings/showcase_printable.php?wantedSite=lbi"))

(defparameter *test-example01*
  (learn-template
   "http://www.showcasecinemas.co.uk/listings/showcase_printable.php?wantedSite=lbi"
   '(:record :cinema
     (:name "Showcase Cinemas Leeds")
     (:phone "0871 220 1000"))))

(deftemplate showcase5
    (:record :cinema
      (:name "Showcase Cinemas Leeds")
      (:phone "0871 220 1000")
      (:loop
       (:first (:record :film
                        (:name "Hancock")
                        (:rating "12A")
                        (:duration (:pattern :mins "92 mins"))))
       (:next (:record :film
                       (:name "Kung Fu Panda")
                       (:rating "PG")
                       (:duration (:pattern :mins "92 mins"))))))
  "http://www.showcasecinemas.co.uk/listings/showcase_printable.php?wantedSite=lbi")

(deftemplate showcase4
    (:record :cinema
      (:name "Showcase Cinemas Leeds")
      (:phone "0871 220 1000")
      (:loop
       (:first (:record :film
                        (:name "Hancock")))
       (:next (:record :film
                       (:name "Kung Fu Panda")))))
  "http://www.showcasecinemas.co.uk/listings/showcase_printable.php?wantedSite=lbi")

(deftemplate showcase1
    (:RECORD :CINEMA
      (:NAME "Showcase Cinemas Leeds"
       (("html:0" "head:0" "title:0" "text:0") "À - Showtimes"))
      (:PHONE "0871 220 1000"
       (("html:0" "body:1" "center:2" "font:0" "text:2") "Booking Line - Á"))
      (:loop
       (:first (:record :film
                        (:name "Hancock")
                        (:rating "12A")
                        (:duration (:pattern :mins "92 mins"))))
       (:next (:record :film
                       (:name "Kung Fu Panda")
                       (:rating "PG")
                        (:duration (:pattern :mins "92 mins"))))))
  "http://www.showcasecinemas.co.uk/listings/showcase_printable.php?wantedSite=lbi")

(deftemplate showcase2
    (:RECORD :CINEMA
      (:NAME "Showcase Cinemas Leeds"
       (("html:0" "head:0" "title:0" "text:0") "À - Showtimes"))
      (:PHONE "0871 220 1000"
       (("html:0" "body:1" "center:2" "font:0" "text:2") "Booking Line - Á"))
      (:loop
       (:first (:record :film
                        (:name "Hancock")
                        (:rating "12A")
                        (:duration (:pattern :mins "92 mins"))
                        (:loop
                         (:first (:record :day-listing
                                          (:day (:pattern :day "Thursday"))
                                          (:date (:pattern :date "10th July"))))
                         (:next (:record :day-listing
                                         (:day (:pattern :day "Friday"))
                                         (:date (:pattern :date "11th July")))))))
       (:next (:record :film
                       (:name "Kung Fu Panda")
                       (:rating "PG")
                        (:duration (:pattern :mins "92 mins"))))))
  "http://www.showcasecinemas.co.uk/listings/showcase_printable.php?wantedSite=lbi")

(deftemplate showcase3
    (:record :cinema
      (:name "Showcase Cinemas Leeds")
      (:phone "0871 220 1000")
      (:loop
       (:first (:record :film
                        (:name "Hancock")
                        (:rating "12A")
                        (:duration (:pattern :mins "92 mins"))
                        (:loop
                         (:first (:record :day-listing
                                          (:day (:pattern :day "Thursday"))
                                          (:date (:pattern :date "10th July"))
                                          (:loop
                                           (:first (:record :times
                                                            (:time (:pattern :time "11:30"))))
                                           (:next (:record :times
                                                           (:time (:pattern :time "12:05")))))))
                         (:next (:record :day-listing
                                         (:day (:pattern :day "Friday"))
                                         (:date (:pattern :date "11th July"))
                                         (:as-above))))))
       (:next (:record :film
                       (:name "Kung Fu Panda")
                       (:as-above)))))
  "http://www.showcasecinemas.co.uk/listings/showcase_printable.php?wantedSite=lbi")


(deftemplate cineworld-cinema-page2
    (:record :cinema-listing
	     (:name "Wood Green")
	     (:address "Wood Green Shopping City, Off Noel Park Road, Wood, N22 6LU")
	     (:loop
	      (:first (:record :film
			       (:name "CHANGELING")
			       (:Director "Clint Eastwood")
			       (:Starring "Angelina Jolie, John Malkovich")
			       (:description "A woman's struggle to find her missing son is the subject of the latest film from Clint Eastwood.")
			       (:loop 
				(:first
				 (:record :day-listing
					  (:day "Tue 02 Dec")
					  (:loop 
					   (:first (:record :times
							    (:time "14:00")))
					   (:next (:record :times
							   (:time "17:10"))))))
				(:next
				 (:record :day-listing
					  (:day "Wed 03 Dec")
					  (:loop 
					   (:first (:record :times
							    (:time "14:00")))
					   (:next (:record :times
							   (:time "17:10")))))))))
	      (:next (:record :film
			      (:name "FOUR CHRISTMASSES")
			      (:Director "Seth Gordon")
			      (:Starring "Jon Voight, Vince Vaughn, Reese Witherspoon, Jon Favreau")
			      (:description "If you've ever had your Christmas plans wrecked by family obligations, then this is the comedy for you.")))))
  "http://www.cineworld.co.uk/cinemas/70")

(deftemplate cineworld-cinema-page1
    (:record :cinema-listing
	     (:name "Wood Green")
	     (:address "Wood Green Shopping City, Off Noel Park Road, Wood, N22 6LU")
	     (:loop
	      (:first (:record :film
			       (:name "CHANGELING")
			       (:Director "Clint Eastwood")
			       (:Starring "Angelina Jolie, John Malkovich")
			       (:description "A woman's struggle to find her missing son is the subject of the latest film from Clint Eastwood."))
	      (:next (:record :film
			      (:name "FOUR CHRISTMASSES")
			      (:Director "Seth Gordon")
			      (:Starring "Jon Voight, Vince Vaughn, Reese Witherspoon, Jon Favreau")
			      (:description "If you've ever had your Christmas plans wrecked by family obligations, then this is the comedy for you."))))))
  "http://www.cineworld.co.uk/cinemas/70")

(deftemplate cineworld-cinema-page0
    (:record :cinema-listing
	     (:name "Wood Green")
	     (:address "Wood Green Shopping City, Off Noel Park Road, Wood, N22 6LU"))
  "http://www.cineworld.co.uk/cinemas/70")



(deftemplate cineworld-cinema-page
    (:record :cinema-listing
	     (:name "ABERDEEN")
	     (:address "Queens Link Leisure Park, Links Road, Aberdeen, AB24 5EN")
	     (:loop
	      (:first (:record :film
			       (:name "BODY OF LIES")
			       (:director "Ridley Scott")
			       (:starring "Leonardo DiCaprio, Russell Crowe, Golshifte Farahani, Mark Strong")
			       (:description "On the trail of a terrorist in the Middle East, CIA agent Roger Ferris (Leonardo DiCaprio) begins to question whether he can trust the man keeping him alive - his boss Ed Hoffman (Russell Crowe).")
			       (:description-more "Ferris is the Agency's man on the ground in Jordan, tracking an important Al-Qaeda leader who is hiding out in the wilderness. Back-up comes via satellite link-up from CIA division head Hoffman, who pulls the strings necessary to keep Ferris safe. But as Ferris closes in on the target, with the help of Jordanian intelligence officer Hani (Mark Strong) - himself an unknown quantity - he begins to wonder if he can really trust Hoffman. Based on the novel by Washington Post journalist David Ignatius, Body of Lies is a riveting, fast-paced, edge-of-the-seat thriller where nothing is as it seems.")))
	      (:next (:record :film
			      (:name "CHANGELING")
			      (:director "Clint Eastwood")
			      (:starring "Angelina Jolie, John Malkovich")
			      (:description "A woman's struggle to find her missing son is the subject of the latest film from Clint Eastwood.")
			      (:description-more "In 1920s Los Angeles Christine Collins (Angelina Jolie) discovers her nine-year-old son Walter has vanished. A fruitless search ensues, but months later a boy claiming to be Walter is returned to her. Dazed by the swirl of cops, reporters and her own conflicted emotions, Christine allows him to stay. But in her heart, she knows he is not Walter. As she pushes the authorities to keep searching, she learns that women aren't expected to challenge the system if they want to stay alive. Slandered as delusional, Christine finds an ally in activist Reverend Briegleb (John Malkovich), who helps in her desperate search for Walter. Based on an actual incident, Changeling is the tale of a mother's quest to find her son, and of the sinister forces that would stop at nothing to silence her.")))))
  "http://www.cineworld.co.uk/cinemas/1")


