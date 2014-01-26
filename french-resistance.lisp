;FRENCH RESISTANCE

;so this is kind of gross, but I can't really use punctuation except periods. I guess I'll have to write it like Hemingway. 

;import stuff from onlisp? so far I'm only using explode, but I'm sure these might be helpful. do-tuples/o might be useful. 
;(load "~/Projects/utilities/onlisp.lisp")
;(load "~/Projects/utilities/patmatch-Novak.lisp") ;this will be used in conversation system. 
  

;UTILITY FUNCTIONS
;========================================================================================

;Clean up text. Why does this give me (t t) everytime? 
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun print-description (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))
 
;import a dictionary so I won't have to enter synonyms manually, it'll just look it up in the dictionary. will also aid conversation. 
(defmacro when-cond (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
           (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym ;what if ,sym is nil? it'll terminate won't it? 
             (progn
               ,@(cdr cl1) ;is this right? 
               (when-cond ,@(cdr clauses))) 
           (when-cond ,@(cdr clauses))))))) 
  

;this is what I should use in french resistance, I didn't know about anaphorisms then. 
;this is having some problems, when all three clauses are true, it seems to only do two? 
(defmacro awhen-cond (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym ;when I use a when it terminates when the first thing is nil, it only works when its all true. not a problem with when-cond though. hmm.
             (progn
               (let ((it ,sym)) ,@(cdr cl1))
               (awhen-cond ,@(cdr clauses)))
           (awhen-cond ,@(cdr clauses)))))))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

;Doesn't Work! infinite loops. 
(defmacro nwhile (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (while ,sym ;;;I feel like sym is not getting updated by cl1, so that its never being reevaluated again. 
           ,@(cdr cl1)) 
         (nwhile ,@(cdr clauses)))))) 

(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element c)))
                 (symbol-name sym))) 

(defun end (lst)
  (car (last lst)))

(defun random-elt (choices)
  "Choose and element from a list of random"
  (elt choices (random (length choices))))

;=================================================================================================================
;GLOBAL VARIABLES AND STRUCTURES 
;=================================================================================================================

;to deal with numbering maybe start storing things in a hashmap or an array, it would make retrieval quick I think, if there was an array and each index was
;a location array, each of those containing areas. I also don't have it working where one room can lead to another, its merely abstracted and not reflected in code.

;these descriptions are too much about history and thought, not enough about description of actual location.
;1-20-14, maybe organize that map further into quadrants, to restrict access between locations. 
(defparameter *map* '((home
                       (your small apartment in conquered Paris. No longer as comforting as home should be. more like a trap than a dwelling. 
                             they will know right where to find you.)
                       (bedroom (the sun strikes through the window of your room.)) ;and on your dresser a picture of your parents and a girl you once knew.
                       (bathroom (you stand in front of your sink staring into the mirror.)))
                      (apartment
                       (this is your apartment building.) 
                       (stair-case (you stand about halfway up the building. the stairs lead down the street.))
                       (landladys-door (you stand in front of your landladys door.))) ;have the player knock to talk to her. 
                      (eiffel-tower 
                       (symbol of the glory of fallen France. Unconquered by Hitler.) ;description
                       (bottom (you stand under the looming tower.))
                       (top (you stare out at the lit and conquered city below.)))  
                      (cafe
                       (a cozy and small cafe. supposedly there is a resistance organization that meets here.)
                       (main-area (there are tables with candles lit and patrons sipping coffee. 
					 there are less people here than usual and even with conversations happening the place seems hushed.))
                       (bathroom (a typical public restroom.)) ;i wish I could randomize someone being in a stall. 
                       (basement (the basement is lit by lamps showing tables atopped with maps and documents bearing plans.)))
                      (movie-theatre 
                       (a movie theatre still as popular as before. perhaps more so because of the need for escapism. but its not so easy
                          to forget your worries as its just as popular with the occupying soldiers as with the the occupied.)
                       (front (a couple soldiers stand in front speaking in German while some young girls chat and a family walks away from
                                 the theatre.))
                       (lobby ) 
                       (theatre-un ) ;these will each have boring descriptions, but a projector feature in the theatre (hidden from player)
		                     ;will be flipped on when they enter, and will have a function displaying a random movie. 
                       (theatre-deux )
                       (theatre-trois ))
                      (church 
                       (a bleak church in sullen city. here less reverance is paid than before but more prayers are muttered wept or cried.)
                       (sanctuary (there are a few other people in the pews. some with rosaries and some facing a figure of christ or his mother.
                                        this is not the fear of god they feel but the fear of man. the anxiety of being tread upon.))
                       (confession-booth (you stare ahead at the wooden-shine of the door and see in your peripheral the ephermeral glimpse of the
                                              silent priest.)))
                      (hotel 
                       (bar)
                       )
                      (nightclub 
                       (for a while these places were really in a vicegrip by the local authority. The music was condemned as undisciplined kike
                         nigger jungle music. but turns out the german soldiers love jazz as much as everyone else so the government is forced
                         to appease them but also seem strong by making ridiculous decrees that music not exceed a certain tempo and sycopation
                         be limited to only 10 percent of a song.)
                       (bar (you sit at the bar with the stage to your left.))
                       (dining-area (your seat faces stage where you can see the band bouncing as the music is enjoyed by all without animosity.))
                       (bathroom (in the bathroom you can here the muffled music.)) )                      
                      (park
                       )
                      (restaurant 
                       )
		      (museum 
		       )
		      (concert-hall
		       )
		      (bakery 
		       )
		      (bank 
		       )
		      (train-station
		       (telegram-office ())
		       )
                      
                      ;include La Place Blanche café, reserved for germans exclusively. experiment in preventing access to a place. you can only stand
                      ;in front of it. 
                       
                      
                      ;treat this as an area instead of a location. the location will stay and represent which node you are at. 
                      (streets ;this is using a system where all streets are considered one unit, and you just travel between nodes on it. 
                       (a typically oppressed though still lovely street of paris. The boast of the nazis hang red and black over buildings.
                          their harsh language is strewn across buildings while commands bark from posters.)
                       (home (the street is never crowded so when you emerge out of your building you feel yourself stand out. 
                                       Something you never minded before but now you find yourself checking to the left and right for any unwanted scrutiny.)
                                       (cafe south 2) (movie-theatre south-west 3) (eiffel-tower west 10))
;WRITE DESCRIPTIONS FOR EACH STREET
                       (cafe (a few german soldiers sit outside the cafe enjoying weather that should be enjoyed by the french who are too nervous
                                to sit near the boisterous intruders.)
                        (home north 2) (movie-theatre west 2) (eifel-tower west 8))
                       (movie-theatre () (apartment north-east 3) (cafe east 2) (eiffel-twoer west 6))
                       (eiffel-tower () (apartment east 10) (cafe east 8) (movie-theatre east 6))
                       (movie-theatre ())
                       (nightclub ())
                       (chruch ())
                       (hotel ())
                       (park ())
                       (restaurant ())
                             
                      ;I might also want to make a street system where its based on individual roads, which might be more interesting,
                      ;but runs the risk of being too complicated or getting in the way of playing. 
       

                      (street
                       (a typical oppressed though still lovely street of Paris. the boast of the nazis hang red and black
                          over buildings.)
                       (north ?x) ;set these to values based on where you're leaving from. 
                      (south ?x)
                       (east ?x)
                       (west ?x)))))
 
;necessary?
;(defparameter *location-synonyms* nil)

;get-paths is called before the person is created. 
;(defun get-area-synonyms ()
 ; (let ((lst nil))
  ;  (dolist (i (get-paths))
   ;   (cons (second i) lst))
    ;lst)) 

;build this out of the path descriptions. Sometimes I might want to call something different in the path description than in the code. 
(defparameter *entrance-synonyms* '(in inside in-side entrance)) ;this only handles entry right now, not navigating between rooms. 

(defparameter *area-synonyms* nil)

;(setf *area-synonyms* (get-area-synonyms))

  
;1-2-13
;this might work if the main map gets too big. I could keep this global variable which will contain things like the locations in
;paris, and locations on the outskirts, etcetera. 
(defparameter *france* `(,*map*))

;I'm not even using this right now except to show the player the paths, and that could just be an element of description in the map list. 
;This will prove useful by assigning where you can go from each place, which means I'll need a a function to check to make sure the area
;the player decides to go is valid. 
(defparameter *paths* '((home
                         (bedroom (door street) (door bathroom))
                         (bathroom (door bedroom)))
                        (eiffel-tower
                         (bottom (sidewalk street) (elevator top) (stairs top))
                         (top (elevator bottom) (stairway bottom)))
                        (cafe
                         (main-area (door street) (stairs basement))
                         (basement (stairway main-area)))
                        (street
                         (north cafe)
                         (south eifell-tower)
                         (east ?x)
                         (south ?x))))

;these will be like objects that are features of a location, or things that can be interactd with. Are these justified as different from objects? why do they lack descriptions right now? add some. features may also be hidden. 
;first t/nil represents hidden, second represents state. third contains the description of those states, the t and then the nil.
;either may contain no description. the true description will be there all the time, or just when the change happends. For some
;objects I think it makes sense to leave it the same. for others not so much. here's something, if the faucet is left running, 
;should it turn itself off, like in a book if an author forgot to have a character close the refrigerator after opening it, I'm 
;not sure I'd be bothered, or even notice. However in a game this might be expected by some, but others might find it jarring like
;me. Add descriptors after the change-state function.   
;I could add command-names to the function names, so save-game would be (save-game sleep) (set-state turn-on turn-off)
;is change-state implicit? if its a feature you can interact with it. but it isn't necessarily all that you can do.
;it doesn't make sense for a bed to have a state. I guess some features might be static nut still capable of interaction. 
;other interactions should have their own descriptions right? so maybe the state of the bed doesn't matter, but sleeping
;might trigger a description. do I include all the the synonyms in here along with set-state and change-state? I could ` ,@ it in.
;multiple functions can be triggered by a state change, so put them in a list at car instead of just set-state. example. need to
;change the functions so that they do not just the first, but everything in the first. 
;elevator will change player's location when its activated. 
(defparameter *area-features* '((home
				 (bedroom 
				  (bed (there rests your bed in the corner.)
				       (its not a particularly comfortable bed but its a great relief to sleep in
					    even a little comfort. certainly better than the cots or mats most soldiers sleep on.)
				       nil (nil (you lay down onto your easement.) ()) 
				       ((((save-game)) sleep))) ;how will the state-description work with save-game? 
				  (radio (your radio sits ontop of the small bookshelf.)
					 ;used to say silently, but had to change 
					 ;because that doesn't make sense when its
					 ;on. find a way to edit it. 
					 (its very dangerous to be found with any sort of anti-nazi literature on your person.
					      they cant manage to crack down on the radio-listing though since you cant be caught 
					      in possession of airwaves.)
					 nil (nil (the switch snaps and the speakers crack and then the projection fills the room.)
						  (as quickly as it spread out it drops off and your ears refocus on the noise of the 
						      outside world.))
					 ((((set-state 'radio t) (play-radio)) turn-on) 
					  (((set-state 'radio nil) (silence)) turn-off)))) 
				        ;include a function thats searches for a random broadcast, and then plays it. 
				 (bathroom 
				  (faucet ()
					  (a sink. the most conveinent delivery of the most important need. such a fragile infastructure
					     that the nazis likely control whether it flows or not.)
					  nil (nil (water flows from the faucet.) (you stop the flow of water.)) 
					  ((((set-state 'faucet t)) turn-on) (((set-state 'faucet nil)) turn-off)))
				  (bathtub (filling up most of the room is the bathtub.)
					   (the porcelin is not too clean but it seems pointless to clean it. you doubt that youll
						be around to enjoy the soak that many mores times.)
					   nil (nil (the water flows from the spout and rushes against the stop until it juse falls
							 into itself and soon the tub is filled.) (the cascade halts.))
					   ((((set-state 'bathtub t)) turn-on) (((set-state 'bathtub nil)) turn-off)))))
				
				(eiffel-tower 
                                 (bottom 
				  (lift (the empty lift rests ready to move along the tower.) 
					(the lift turns what is a rather long way to climb into a slow and pleasant ascent over the
					     whole of Paris.)
					nil (nil (the lift begins its climb to the top of the tower.) 
						 (the lift sinks down as your stomach rises.)) ;the state reflects moving up or down? 
					((((change-state 'lift) 
					   (activate-lift 'lift 'bottom 'top)) activate))) 
					;so I need a function that can move features between locations/areas. 
				  ;also need an elevator function that'll use change-area1. 
				  (lift-cable (there is the cable suspending the lift.)
						  (with the right kind of tool you could probably cut the cable.)
						  t (t () (the cable is cut rendering the lift useless.
							       its a long climb to the top now. too long for Hitler.) 
						       ((((change-state)) cut)))))
				 
                                 (top (flagpole nil (t (that hated shape of the nazis rides on a sharp red wave in the wind
							 over all Paris.) (the flagpole is bare.)) 
						((((set-state 'flagpole nil)) lower-flag)
						 (((set-state 'flagpole t)) raise-flag))))) )) 
					;these could just be on/off functions, so just abstract it. 
				 ;this can't use the generic though, because it'll have a t value, but with one flag, a nil when
				 ;you lower it, and then a second when the french flag is raised. write a function that can 
				 ;overwrite description, and in the case of a flag, it might have this function which contains
				 ;its own description to be added. so maybe the command raise-flag does more that just set-state. 
				



;I could maybe make paths like for 314. (home (cafe 2) (eiffel-tower 10)). This would get rid of the need for the player to draw
;a map, and would allow a kind of quick travel between locations. it might be hard to create tension that way? perhaps
;each location has multiple ways to get somewhere so (home (cafe (main-st 1) (backroads 2)) (eiffel-twoer (main-st 7) (backraods 12))
;I'm worried this would make roads just like one area though, rather than having roads connect to each other. 
;this will all be handled at the street, so a door will always lead to another area, or to a street which will take you to a new location. 
;so there's essentiall a home-street, cafe-street, etcetera, and each path has a distance, maybe a traffic-rating (changing based on the day?)
;and these numbers will affect the possiblity of encountering resistance. I think certain locations should also have a possibility of being
;occupied, so perhaps you go to a cafe to join the resistance, but there are some soldiers there. Maybe this is what I should keep
;in the street variable, just a list of all entries into the street, and how far they are from each other. This will create a network/graph. 
 

;the three nil values are respectively, can it be equipped, is it hidden, and does it have functions associated with it? 
;1-8-14 !!!! I added a parentheses around the items in an area, so now all object functions are out of whack, but this is necessary
;so that I can add and delete objects from a room, so they must be in a list. and things like if its a known object. 
;change all these ts and nils to (fixed nil) (hidden t) etcetera. Instead of a hidden value maybe I should say what container its in? 
;this might make it easier to keep track of when writing things as a 3rd party. 
(defparameter *object-locations* '((home 
				    (bedroom                     ;why do I have all these in a list? it would be cleaner to just call cdr (bedroom.... and get ((x) (y)) etc.
				     (girl-photo (you wonder what happened to her.
							its been a while since you last saw her so you have you suspicions.)
						  (Ontop of your dresser stands a picture of a girl you once knew.) ;this is a descrition to be used. 
						  nil nil nil) ;this is how to find where to place it. might change to lists if its in
					;multiple places. 
				      (family-photo (you stare at the photo of your deceased parents who loved you and wanted you to live
							 and be happy. two people who could not have imagined this happening. 
							 just like the rest of you. Except lucky enough to have never been proven wrong.)
						    (and beside it stands a photo of your parents.) 
						    nil nil nil)
				      (newspaper (everyone knows the press is compromised and that this is particualry bad 
							   even for propaganda. However it would probably look suspicous to quit reading it.
							   and as of yet you havent worked up the courage to buy one of those underground
							   papers youve heard about. listening to the bbc broadcast each night like everyone
							   has been enough for you so far.)
						 (Over on the table is a newspaper.)
						 nil nil nil)
				      (pistol (a small pistol of your fathers. it seems surreal that you might need it.)
					      (Inside a wooden box towards the back of the drawer lies a pistol.)
					      bedroom home t t ((fire-weapon 6)))
				      (journal (a sparse journal of innocent thoughts you hope they dont find.)
					       (Your journal lies in plain view ontop of your clothes. 
						     You feel like you should always be able to get to it easily even though
						     you rarely ever need to.)
					       t t ((write))) 
				      (bible (your parents bible. a family heirloom you suppose. though you never revered it much. 
						   Its focus is on a people who arent yours in a place youve never been and a time you can hardly comprehend. 
						   In these days of consequence though it feels more relavent.)
					     (tucked in the corner supporting the other books that lay against it is your parents bible.); it feels strage to leave something so fragile
					; out in the open. It is not a living thing but a bundle of paper needing one to stave off the rot.) 
						  t t ((read-text txt))))
				    (bathroom
				     (razor-blade (these still arent difficult to find. but you use it sparingly in case there becomes a scarcity.
						    you consider its usefulness if you were caught. you could not pick a lock with it.
						    and you doubt you could kill more than a single captor with it. 
						    you shudder at needing to use it on yourself. could you?)
					     (a razor-blade lies flat inside the cabinet.) 
					     t t ((cut)))
				      (razor (you havent felt a need to shave lately. but this blade is still fairly fresh.)
					     (next to it is the metal wand which holds them.) 
					     t t ((shave))) ;this is kind of weird, because next to is relative, and the contents can change. 
				      (shaving-cream (you remember it makes a very nice lather. you can wash and soothe your face here at home. and yet in the middle
							  of a battle-zone where war is fought in the shadows. men on the front don't have these comforts.
							  You wonder if you would give it up for a little honest and open resistance. at least in on the battlefield
							  youre not sharing space with the enemy. you dont fall asleep behind enemy lines out on the front.)
						     (in a tube with a flat tail slightly curled is your shaving cream.)
						     t t ((apply)))
				      
				      (cologne (another item you find no need for. you wish you could care about how women thought you smelled.
							but the women you will meet in the resistance probably dont care a bit about that kind of thing anymore.)
					       (in the corner is a bottle of cologne half empty.)
					       t t ((apply)))))))
				      
					     
						   
					     
				    
`



;pattern matching? this causes a problem when there are two similar objects like the photo of the girl and of your parents. otherwise it should just be
;able to find the word the player input and then display that.  
(defparameter *object-synonyms* '((girl-photo (picture of girl) (girls picture) (photo of girl) (girls photo) (photo girl) (picture girl) (girl picture)
                                              (girl photo)))) 

;these are all the objects that you can pickup.
(defparameter *items* nil)

;containers can be inspected for a description, or opened for a list of contents. ending value is if its locked or not. also add one for hidden, like a wall-safe? first
;is now hidden, second is being viewed, third is locked, fourth equippable
;There's some overlap between containers and objects I think. Example: briefcase, it contains
;items, but its also something equippable. add a fourth value for equippable? 
(defparameter *container-locations* '((home 
				       (bedroom
					(dresser (a dresser stands in the corner containing various possessions of yours.)
						 (journal pistol) nil nil nil nil)
					(cabinet (a cabinet is mounted to the wall. its contents dictated to you by the occupiers.) 
					;just one cabinet? not very realistic, but is that how I operate
					;in my kitchen?how does the mind process it? Don't want tedious description
						 (wine baguette butter) nil nil nil nil)
					(bookshelf (a sparse bookself stands not too high.)
						   (bible) nil nil nil nil))
				       
				       (bathroom 
					(cabinet (behind the mirror is a nook for your toiletries. strange to be at war and still have these luxuries.) ;easements?
						 (cologne shaving-cream razor-blade razor) nil nil nil nil)))
				      (cafe (basement
					     (briefcase (sitting under the table is a briefcase.)
							(pamphlets) 
							 nil nil t t)))))
                                         
                                             
(defstruct person
  (name nil)
  (appearance nil)
  (location-description nil)
  (location nil)
  (inventory nil)
  (holding nil) ;this will have a name of what's currently being held. 
  (viewing nil) ;this will be for inspecting a container or a document or something
                ;may have other effects on perception. 
  (loyalty nil) 
  (undercover nil)
  (anxiety 0)
  (suspicion 0)) 

;this will handle health and physical feats. don't deal in hit points, but in conditions. like your arm suffered a fracture, not 10 points of damage. 
;or damage inflicts wounds. 
(defstruct body 
  (sex nil)
  (weight nil)
  (height nil)
  (age nil)
  (head nil)
  (neck nil)
  (torso nil)
  (left-arm nil)
  (right-arm nil)
  (stomach nil)
  (leff-leg nil)
  (right-leg nil))



(defparameter *player* (make-person
                        :name '(Jacques Gallion)
                        :appearance '(your standard frenchman though strungout by the occupation)
                        :location '(bedroom home))) 

(defparameter *commands* '(save time enter exit explore inspect walk back pickup use talk open close consider equip take inventory drop put
			   change-state)) ;consider? this might give you clues when reading memos or something. put in, put on.  

;write a synonym for change-state, or maybe some commands in this list just translate to change-state? or they do if the object is a feature? 

;does open work on doors and containers. does pickup work like inspect on items that can't be inventoried. 

(defparameter *command-synonyms* '((explore (look-around lookaround look search investigate))
                                   (walk (travel move go goto go-to))
                                   (inspect (investigate look-at lookat view check-out checkout check search scan watch see))
                                   (time (watch clock hour minute))
                                   (take (keep pocket store))
				   ))

;this will be used to store the history of a playthrough, and then you can replay a game to that point. useful for testing.
;could also serve as a save function. 
(defparameter *command-history* nil)


(defun do-history (history)
    (dolist (action history)
      (do-command (car action) (second action))))

;work on later. why append no work?
(defun synonyms-list ()
  (mapcar #'append (mapcar #'append (mapcar #'cdr *command-synonyms*))))

(defun synonyms-list1 () 
  (let ((lst nil))
    (dolist (i *command-synonyms*)
      (dolist (j (cdr i))
        (setf lst (append j lst))))
    lst)) 
 
(defparameter *synonyms-list* (synonyms-list1)) 
 

;maybe feature more description about what things you can have in your inventory. there will also be objects that you are holding on your person. 
;start out with some default items like a passport which can be shown to enemy patrols.  
(defparameter *inventory* ())
 
(defun list-inventory ()
  (let ((lst nil))
    (dolist (i *inventory*)
      (push (car i) lst))
    lst))

(defun show-inventory ()
  (if (null (list-inventory))
      (print-description '(you do not have anything on your person.))
    (let ((lst (reverse '(you have in your possession a))))
      (dolist (i (list-inventory))
        (push i lst)
        (when (not (null (member i (list-inventory))))
          (nconc '(and a) lst)))
      (print-description (reverse lst)))))
    

(defparameter *persons* `(,(make-person
                               :name 'barista ;proprieter. Create synonyms for each person. Or just for occupations? 
                               :appearance '(a middle-aged man with a sullen look on his face.)
                               :location-description '(a middle-aged man stands behind the bar.) 
;this can come before or after appearance, so write two functions with a way to determ           
                               :location '(main-area cafe))
                             ,(make-person
                               :name '(Jean Augustin)
                               :appearance '(a man not that much older than you. he appears weathered and this experience seems the source of his confidence.
                                               he seems to know what is required. and sure in what he is capable of.)
                               :location-description '(he sits at a table looking over documents.)
                               :location '(basement cafe))))


(defparameter *days-since-occupation* (+ 150 (random 400))) 
;I don't actually know how many days it should be. maybe the game should have chapters that are years or seasons
;this could help divide the world up, so you're in the country at one part, then maybe you're in paris, and then later the unoccupied zone, etcetera. 


;new events will be created each time you change location. events will have factors like number of people, what they're doing, 
;and will also have stats like how long you have to do something. how to get events to be influenced by others? events will have
;to be saved as well, and triggered in the right order when loading a game. 
(defstruct event 
  )

;===========================================================================================================================================
;TIME and SIMULATION
;==========================================================================================================================================

;this should really be a structure GACK. 
(defstruct  game-time 
  (year 1940)
  (month 'july)
  (date 16)
  (days-since-occupation 0)
  (day 0)
  (hour 12)
  (minute 0)
  (second 0)
  (period 'pm)) 

(defparameter *game-time* (make-game-time))

;(defparameter *days-in-resistance* (fourth *game-time-of-day*)) ;intialize this once the game begins, or perhaps once the player successfully joins/completes a mission. 

(defun hour ()
  (game-time-hour *game-time*))

(defun minute ()
  (game-time-minute *game-time*))

(defun seconds ()
  (game-time-second *game-time*))

(defun period ()
  (game-time-period *game-time*))

(defun day ()
  (game-time-day *game-time*))

(defun inc-game-time (inc-sec inc-min inc-hr inc-day)
  (setf (game-time-second *game-time*) (+ (seconds) inc-sec))
  (setf (game-time-minute *game-time*) (+ (minute) inc-min))
  (setf (game-time-hour *game-time*) (+ (hour) inc-hr))
  (setf (game-time-day *game-time*) (+ (day) inc-day))
  (game-time-rollover))
  
(defun game-time-rollover ()
  (while (> (seconds) 60) 
    (setf (game-time-second *game-time*) (- (seconds) 60))
    (setf (game-time-minute *game-time*) (1+ (minute))))
  (while (> (minute) 60)
    (setf (game-time-minute *game-time*) (- (minute) 60))
    (setf (game-time-hour *game-time*) (1+ (hour))))
  (while (> (hour) 23)
    (setf (game-time-hour *game-time*) (- (hour) 24))
    (setf (game-time-day *game-time*) (1+ (day))))
  (if (< (hour) 12)
      (setf (game-time-period *game-time*) 'am)
    (setf (game-time-period *game-time*) 'pm))) 

(defun rollover-min (min)
  (list (round (/ min 60)) (- min (* 60 (round (/ min 60))))))

(defun rollover-hr (hr)
  (list (round (/ hr 24)) (- hr (* 24 (round (/ hr 24)))))) 

;have this return an error if it gets a destination that isn't a location. 
(defun get-distance (destination)
  (dolist (i (get-street-routes))
    (if (equal (car i) destination)
        (return (third i))))) 
  
(defun distance-time (destination) 
  ;I get a frequent error here when destination is passed in as nil. It usually reveals an error some where else, but maybe I should add a check here? 
  ;previously the error was caused because do-command wasn't checking walk (location) it was just checking walk, then chaging location to obj, thus telling 
  ;change-location1 to change the location to (entrance), and thus screw up get-distance. ITS OVERLOADING THE BUFFER!?!?!
  (inc-game-time 0 (* 12 (get-distance destination)) 0 0))   

;every action takes a minute? I think this gives the player game-time to imagine everything that happens. this might be too short for reading a letter, 
;planting a bomb and such. it might be too long for opening a door, reloading a gun, etcetera. 
(defun action-game-time ()
  (inc-game-time 30 0 0 0))

(defun convert-hour ()
  (cond ((> (hour) 12)
         (- (hour) 12))
        ((zerop (hour)) 12)
        (t (hour))))

(defun show-time ()
  (if (zerop (minute))
      (print-description `(It is ,(convert-hour) ,(period)))
    (print-description `(It is ,(convert-hour) ,(minute) ,(period))))) 

;this will modify the description of rooms. I'll need some really expert pattern matching to do anything too sophisticated. right now all I can imagine
;is adding something like "the sun shines through the windows" or the moonlight hit the pitcher. Things that normally are not worth mentioning in the description
;but that the game-time of day highlights. It would be more interesting though if I could also change the description of certain items, or even persons. 
;its also worth considering moving objects around based on the game-time of day, like someone might have a kettle on in the morning, but at night would have some
;candles lit. 
(defun describe-time () ) 

(defun intro ()
  `(it has been ,*days-since-occupation* days since the horrible machine had quaked through the countryside and split your country in two.
       burdening your people with draconian law and tyranny. taking their goods from them to feed what ails them. the land stripped of its bounty.
       its people stripped of their freedom. and you stripped of your honor. but not your pride.
       It is time for resistance. It is time for combot. It is time for liberation. It is time for a free France.)) 
;the three papers I recall being published were resistance, liberation, and combat. 
       

;==============================================================================================================================================================
;DESCRIPTION and NAVIGATION FUNCTIONS
;===============================================================================================================================================================

;1-3-14 rewrite a lot of the functions for moving the player. you should be prompted where to go, but say where to go. walk to the cafe. run to the eiffel tower
;sneak to the movie-theatre. 
         

;this will have to include synonyms for street.
(defun list-areas () 
  (let ((lst nil))
    (dolist (i (cdr (cdr (assoc (current-location) *map*))))
      (push (car i) lst))
    (push 'streets lst)
    (push 'street lst)))  ;this pushes streets onto the list because every location will have a way out of it. 

;might be a too simplistic system, what if there are multiple entrances. 
(defun entrance ()
  (car (reverse (list-areas))))

(defun list-locations ()
  (let ((lst nil))
    (dolist (i *map*)
      (push (car i) lst))
    lst))
     
;GET AREA OR LOCATION 

(defun current-location ()
  (second (person-location *player*)))

(defun current-area ()
  (car (person-location *player*)))
       
(defun person-loc (person)
  (second (person-location person)))

(defun person-area (person)
  (car (person-location person)))

(defun get-area () 
  (assoc (car (person-location *player*)) (cdr (get-location)))) 
;  (assoc (car (person-location *player*)) (cdr (assoc (second (person-location *player*)) *map*))))
 
(defun get-location ()
  (assoc (second (person-location *player*)) *map*)) 

(defun describe-location ()
  (second (get-location))) 

;this could use the player-location instead of area, just always describe where they are. 
(defun describe-area ()
  (second (get-area)))  

;MOVE PLAYER FUNCTIONS

;changing location should by default change the area. to the first one everygame-time, or depending on the path? on the path I think. 
;have it print the description of the location here only. 
(defun change-location ()
  (setf (second (person-location *player*)) (read))
  (change-area1 (car (third (assoc (current-location) *map*)))))

(defun change-location1 (location)
  (distance-time location)
  (setf (second (person-location *player*)) location))
 ; (print-description (describe-location)))
;  (change-area1 (car (third (assoc (current-location) *map*)))))

(defun change-area ()
  (setf (first (person-location *player*)) (read))
  (if (on-street)
      (on-street)
    (display-room)))

(defun change-area1 (area)
  (setf (first (person-location *player*)) area)
;  (silence) 
  (if (on-street)
      (on-street)))
;    (display-room)) )

;being on the street will trigger traveling to another location. 
(defun on-street ()
  (or (equal (first (person-location *player*)) 'streets)
      (equal (first (person-location *player*)) 'street))) 

;change this so that the street is randomized each visit. might have a patrol coming through, might be merchants (though this should be more static)
;and pedestrians. a person you recognize might even appear (which could be a problem if they're an enemy who recongizes you). 
(defun describe-street ()
  (list (describe-location)
  (second (assoc 'streets *map*))
  (second (assoc (current-location) (cdr (assoc 'streets *map*))))))
       
(defun get-street-routes ()
  (cdr (cdr (assoc (current-location) (cdr (assoc 'streets *map*))))))

;this is sloppy. in the future make it so everything in one direction is printed, and in in order of distance. 
(defun show-route (route)
  `(the ,(car route) is ,(third route) kilometers ,(second route)))

(defun modify-street () )

;write a function so the user can input a direction command and it'll go to the closest destination in that direction. 
(defun closest-along-route (route)
  (let ((min 100))
    (let ((closest nil))
      (dolist (i (get-street-routes))
        (when (and (equal (second i) route) 
                   (< (third i) min))
            (setf min (third i))
            (setf closest (car i))))
      closest)))  
 
;have the player enter something like go east, walk north. this will create a list of possible directions they could go, then the above function will
;find where the closes location along that route is. that's what will be passed to object. player won't be able to just say north, east, etcetera.   
(defun directions-list ()
  (let ((lst nil))
    (dolist (i (get-street-routes))
      (push (second i) lst))
    lst)) 

;---------------------------------------
;PATHS
;---------------------------------------
(defun get-paths ()
  (rest (assoc (current-area) (cdr (assoc (current-location) *paths*)))))

;this is going to be awfully repetitive I'm afraid. Maybe paths should have their own descriptions. Perhaps their is a door
;that leads somewhere unknown to the player.
(defun describe-path (path) 
  (let ((decon (explode (car path))))
    (if (equal (end decon) 's)
        `(there are ,(first path) going to the ,(second path) from here.) 
    `(there is a ,(first path) going to the ,(second path) from here.)) )) 

;adapt this so if there are multiple paths it'll print out "there's a door leading... and a hallway leading...
(defun describe-paths ()
  (apply #'append (mapcar #'describe-path (get-paths))))


(defun append-area ()
    (append (describe-location)
           (describe-area)
           (show-persons (current-area) (current-location))
           (describe-paths)))

;==================================================================================================================================
;FEATURES
;==================================================================================================================================
;REWRITE THESE because area-features has been expanded with descriptions. they'll now be more similar to objects, but often with states. 
;somethings don't seem like they're different from objects. like a morse-code machine, it would have a change state from sending/not-sending
;but why wouldn't that be a function of something it could do. Well I think it would be, so maybe features are just things that have states. 


;write variants for all of these that take in location and area rather than using the current one. I can't think of a use right
;now, but it'll help in the future when introducing more simulation elements. 

;if I edit object-locations, I can combine this with hiddenp and not need two functions. woohoo. 
(defun hidden-feature-p (feature)
  (nth 2 (cdr (assoc feature (get-features)))))

(defun get-features ()
  (cdr (assoc (current-area) (cdr (assoc (current-location) *area-features*)))))


(defun list-features ()
  (let ((lst nil))
    (dolist (i (get-features))
      (push (car i) lst))
    (reverse lst))) 

(defun describe-feature (feature)
  (car (cdr (assoc feature (get-features))))) 

;some features will be nil, things that don't really deserve a whole mention except in the general area description perhaps? 
;but that isn't really kosher. maybe do this rarely, like with the faucet. its a nice feature to allow at least, even if its ugly. 
(defun describe-features ()
  (let ((lst nil))
    (dolist (i (list-features))
      (if (and (describe-feature i) 
	       (not (hidden-feature-p i)))
	  (push (describe-feature i) lst)))
    (apply #'append (reverse lst)))) 

;gets list of the state and the two descriptors attached to the t and nil state-values. 
(defun get-descriptors (feature)
  (nth 3 (cdr (assoc feature (get-features))))) 

;--------------------------------------------------
;Feature Function and Command retrieval
;--------------------------------------------------
;make sure that a feature can have multiple functions that can be accessed.

;make this a more general change-state function? where will the effects of it come from? it could reveal items,
;areas/locations, characters,complete a mission, destroy someting. maybe objects/features like this will have a t/nil value
;which describes their state, and then a description of what happends when they're activated. This doesn't cover all things
;that features might do, but it covers a lot of binary cases. One critical area here is to create appropiate synonym for 
;how the state is changed. This is necessary for description and for allowing the player to describe the appropiate actions. 
;does this entail a permanent change, where its always on, or maybe its on for a duration? or just on then off? 
(defun describe-state (feature)
  (let ((descr (get-descriptors feature)))
    (cond ((and (second descr) (car descr)) (second descr))
	  ((and (third descr) (not (car descr))) (third descr)))))
  

;write a macro for this so i don't have to write all the assocs and setf. 
(defun change-state (feature)
  (setf (car (nth 3 (cdr (assoc feature (cdr (assoc (current-area) (cdr (assoc (current-location) *area-features*))))))))
	(null (car (nth 3 (cdr (assoc feature (cdr (assoc (current-area) (cdr (assoc (current-location) *area-features*))))))))))
  (if (describe-state feature)
       (print-description (describe-state feature))))

(defun set-state (feature state)
  (setf (car (nth 3 (cdr (assoc feature (cdr (assoc (current-area) (cdr (assoc (current-location) *area-features*))))))))
	state)
  (if (describe-state feature)
       (print-description (describe-state feature))))

;this will only work if player is in the room with the feature. REWRITE
(defun get-feature-functions/commands (feature)
  (nth 4 (cdr (assoc feature (get-features))))) 

(defun list-all-functions ()
  (let ((lst nil))
    (dolist (i (list-features))
      (dolist (fun (get-feature-functions/commands i))
	(push fun lst)))
    lst))

;also create a list of synonyms. this will grab things like "sleep" from save-game function, and add that to the command
;rather than something like change-state. 
(defun get-feature-commands (feature)
  (let ((lst nil))
    (dolist (fun (get-feature-functions/commands feature))
      (dolist (cmd (cdr fun))
	(push cmd lst)))
    lst))

;this doesn't associate it with a command though :(
(defun get-feature-functions (feature)
  (let ((lst nil))
    (dolist (i (get-feature-functions/commands feature))
      (push (car i) lst))
    lst))
  

;maybe remove duplicates? I'm not sure it matters much though. Use union to remove duplicates. 
(defun list-all-feature-commands () 
  (let ((lst nil))
    (dolist (feat (list-features))
      (push (get-feature-commands feat) lst))
    (apply #'append lst)))

(defun add-feature-commands ()
  (dolist (feat (list-features))
    (if (not (hidden-feature-p feat))
	(dolist (cmd (get-feature-commands feat))
	  (push cmd *commands*)))))

(defun remove-feature-commands ()
  (dolist (i *commands*)
    (if (member i (list-all-feature-commands))
	(setf *commands*
	      (remove i *commands*)))))


;add these to *commands* as long as the player is in the room with the feature. put this in play?
(defun access-feature-functions (feature) 
  (dolist (i (list-features))
    (if (not (hidden-feature-p i))
	(dolist (fun (get-feature-functions/commands i))
	  (push fun *commands*)))))

(defun restrict-feature-functions (feature)
  (dolist (i *commands*)
    (if (member i (list-all-functions))
	(setf *commands*
	      (remove i *commands*))))) 

;this will make sure that the command fits the feat, if it doesn't then nothing happens (or trigger an error msg). 
;include triggering the command here? 
(defun command-match-feature (cmd feat)
  (if  (member cmd (get-feature-commands feat))
       t
       nil)) 

;and do the commands. 
(defun match-command-feature-function (cmd feat)
  (dolist (fun (get-feature-functions/commands feat))
    (if (member cmd fun)
	(dolist (f (car fun))
	  (eval f))))) 

;--------------------------------------------------
;FEATURE FUNCTIONS
;--------------------------------------------------

;this has the deficit of only being allowed to have one noise, not simeltaneous noises.
(defparameter *noise* nil)


(defparameter *stations* '(
			   (Radio-Londres 
			    (a sombre melody issues forth.)
			    (a jazzy tune bops about the room.)
			    (you are notified by the announcer that John has a long
			     mustache.)
			    (you hear a calm and matter of fact announcement that
			     the insurance agency is on fire.))))
;include messages and news reports that only play on certain days. do the same thing
;to the newspaper object. 	
		    

(defun describe-noise ()
  (when (equal (current-area) (car *noise*))
    (play-radio)
    (second *noise*))) ;this is so it'll randomly play different tunes. it probably changes too often, and isn't written in a simulation style. 

;RADIO. 
;it will need to continue playing after the first 
(defun play-radio ()
  (let ((audio 
	 (random-elt (cdr (assoc 'radio-londres *stations*)))))
    (setf *noise* audio)
    (setf *noise* (list (current-area) *noise*))))


(defun silence-radio ()
  (setf *noise* nil))

;this will rely on a feature function that tells you where a feature is. if its on first, go to second, etcetera. what if its multiple? 
(defun activate-lift (feat first second) ;add an &optional floors lst to switch to the appropiate floor. 
  (let* ((location-lst (get-feature-loc+area feat))
	(area (car location-lst))
	(loc (second location-lst)))
    (cond ((equal first area)
	   (move-feature-area feat second)
	   (change-area1 second))
	  (t 
	   (move-feature-area feat first)
	   (change-area1 first)))))

;==================================================================================================================================
;OBJECTS
;===================================================================================================================================
;this checks to see if an item can be equipped. 
(defun equipp (obj)
  (nth 4 (cdr (assoc obj (get-objects))))) ;this is pretty gross though, this makes objects rather rigid. 

(defun in-containerp (obj)
  (dolist (i (list-all-contents))
    (if (member obj (second i))
	(return t))))

(defun in-viewing-containerp (obj)
  (if (member obj
	      (second (assoc (person-viewing *player*) (list-all-contents))))
      t
      nil))

;if there is one function it'll return it as a symbol, if there are multiple functions it'll return a list of all of them. 
;WARNING. this will only work with objects on the map, not those in the inventory. add a check to see. 
(defun object-functions (obj)
  (cond ((assoc obj (get-objects))
         (nth 6 (cdr (assoc obj (get-objects)))))
        ((assoc obj *inventory*)
         (nth 6 (cdr (assoc obj *inventory*))))))

;will need to create a synonyms list of these so that the player can use them effectively when they're added to commands. 
(defun function-names (obj)
  (let ((lst nil))
    (dolist (i (object-functions obj))
      (push (car i) lst))
    lst)) 
        

;this checks to see if an item is in a container or otherwise hidden. 
(defun hiddenp (obj) 
  (nth 3 (cdr (assoc obj (get-objects))))) 


;this will be used in describing hidden objects. if you come across something like your father's pistol, its natural that you'd think
;of it then and there, not only when you pick it up. I'm worried that creates a schism between descriptions of places. Right now this is stupid and doesn't work, I haven't even added the correct
;t/nil variable to objects. 
(defun known-objp (obj)
  (nth 5 (cdr (assoc obj *inventory*)))) 


(defun take (obj) 
  (when (and (or (equipp obj) (container-equipp obj))
	     (or (not (hiddenp obj)) (in-viewing-containerp obj)))
    (cond ((assoc obj (get-objects))
	   (push (assoc obj (get-objects)) *inventory*)
	   (remove-obj obj))
	  ((assoc obj (get-containers))
	   (push (assoc obj (get-containers)) *inventory*)
	   (remove-container obj)))
    (print-description `(you take the ,obj and keep it with you.)))) 


;this will work, equip/pickup do the same thing, but equip with something in inventory, and pickup with something in the map. whenever something is
;in hand you have access to its functions. I'm not sure I like this, maybe I should just give access to all items in inventory, and let the player
;imagine pulling them out. after all, they'll just be in your pocket or slung around your shoulder or something. but what about when you have
;a backpack, maybe there are some items that have to be equipped like a bomb, and others like a pistol that can just be drawn and used. 
(defun equip (obj)
  (when (assoc obj *inventory*)
    (when (person-holding *player*)
      (unequip (person-holding *player*))
      (setf (person-holding *player*) obj))
    (object-function-access obj) 
    (print-description `(you hold the ,obj in your hand.))))

;call this whenever an item is currently equipped, and a new one is called. 
(defun unequip (obj)
  (function-no-access obj))  

;does this need to check to see if an item. let this happen to items that can't be equipped, so you just leave it behind when you move,
;or drop it when you pick something else up. 
(defun pickup (obj)
  (print-description `(you pick up the ,obj)) 
  (object-function-access obj))

(defun drop (obj)
  (function-no-access obj))


;will also need a function to remove these commands though. 
(defun object-function-access (obj)
  (nconc *commands* (function-names obj))) 

;this might cause a problem if there are commands that have the same name, or perhaps a synonym. 
(defun function-no-access (obj)
  (dolist (i (function-names obj))
    (delete i *commands*)))
  

;modified this to second now that objects are in a list, so its just one list of them, not a list of a list of them. 
(defun get-objects ()
  (cdr (assoc (current-area) (cdr (assoc (current-location) *object-locations*))))) 
;  (second (assoc (current-area) (cdr (assoc (current-location) *object-locations*))))

(defun get-object-names ()
  (let ((lst nil))
    (dolist (i (get-objects))
      (push (car i) lst))
    (reverse lst)))

(defun inspect-object (obj)
  (print-description (second (assoc obj (get-objects))))
  (fresh-line))

(defun unhide-object (obj)
  (setf (nth 5 (cdr (assoc obj (get-objects)))) nil))

(defun hide-object (obj)
  (setf (nth 5 (cdr (assoc obj (get-objects)))) t))

(defun unhide-objects (objects)
  (dolist (i objects)
    (unhide-object i))) 

(defun hide-objects (objects)
  (dolist (i objects)
    (hide-object i)))

;I'm going to try writing it like this. append each descritpion of the object in the room. this will assume that the descriptions are accurate 
;I'm not treating things like tables and such as objects. I really want a robust system where each object has a description, and objects ontop of it,
;or objects in it. a gun would have bullets in it. I could have show-surface `(there is a ,x and ,y and ,z on the ,surface). what I'm doing now allows for
;more narrative construction/variation, but limits players ability to interact with things like table, dresser, etcetera. the player won't be able to
;kick over a table, or hide in a closet. 
(defun show-objects ()
  (let ((lst nil))
    (dolist (i (get-objects))
      (if (not (hiddenp (car i))) 
          (push (third i) lst)))
    (apply #'append (reverse lst)))) 

;================================================================================================================================
;CONTAINERS
;================================================================================================================================

(defun get-containers ()
  (cdr (assoc (current-area) (cdr (assoc (current-location) *container-locations*)))))

(defun container-equipp (container)
  (nth 7 (cdr (assoc container (get-containers)))))

(defun list-containers ()
  (let ((lst nil))
    (dolist (i (get-containers))
      (push i lst))
    lst))

(defun describe-containers ()
  (let ((lst nil))
    (dolist (i (get-containers))
      (push (second i) lst))
    (apply #'append (reverse lst))) )

;rewrite these list functions to use this, and similar 
(defun list-containers ()
  (mapcar #'car (get-containers))) 

(defun list-all-contents ()
  (let ((contents nil))
    (dolist (i (list-containers))
      (push (list (list i (list-contents i))) contents))
    (apply #'append contents)))

(defun list-contents (container)
  (second (cdr (assoc container (get-containers)))))

(defun get-contents (container)
  (let ((lst nil))
    (dolist (i (list-contents container))
      (push (assoc i (get-objects)) lst))
    lst))

;this will go through each object in the container, call describe-object or just say where it is? open will print that description. 
(defun show-contents (container)
  (let ((lst nil))
    (dolist (i (get-contents container))
      (push (third i) lst))
    (apply #'append lst)))
  

;the problem with this is it should keep showing open containers. this could be included in describe-containers, so that if any of them are open
;they display contents. 
(defun open-> (container) 
  (setf (person-viewing *player*) container)
;  (unhide-objects (list-contents container)) ;this puts all the objects into the regular description
  ;which I'm not sure I want... and when you first open it. YUCK. 
  (show-contents container))

(defun close-> (container)
  (setf (person-viewing *player*) nil))
 ; (hide-objects (list-contents container)))

;1-18-14
;NOW I desparately need a function that lets me hide objects when someone moves away from
;a container. 
(defun describe-contents ()
  (show-contents (car (assoc (person-viewing *player*) (get-containers)))))

;====================================================================================
;DESCRIBING THE AREA
;====================================================================================

;so this can be revised to not reiterate so much. since I have a history, a more complex version can check to see when things
;change, and then show that information. 
(defun list-area ()
  (let ((lst nil))
    (awhen-cond ((describe-noise) (push it lst)) ;this makes sense, you'd hear something as you approach before you see it. 
		((describe-contents) (push it lst))
		((describe-area) (push it lst))
		((on-street) 
		 (dolist (i (describe-street)) (push i lst))
		 (dolist (i (get-street-routes)) (push (show-route i) lst)))  
		((show-persons) (push it lst))
		((describe-features) (push it lst))
		((describe-containers) (push it lst))
		((show-objects) (push it lst))
		((describe-paths) (push it lst)))
    (reverse lst))) 
      


;why does this produce nil between certain things? 
(defun print-room ()
  (dolist (i (list-area)) (print i))) 

;why does it cut the list short? 
(defun display-room ()
  (mapcar #'print-description (list-area)))

  
(defun get-all-hidden ()
  (let ((lst nil))
    (dolist (i (get-features))
      (push i lst))
    (dolist (i (get-objects))
      (when (hiddenp i)
	(push i lst)))
 ;   (dolist (i (get-containers))
  ;    (when (hidden-containerp i)
;	(push i lst)))
    (reverse lst)))

;PLAYER ACTIONS 
;========================================================================================================

;use read-line to get input, then search through it to see if any words in it are commands, and then search it for a likely
;object. Do I need to create a separate list where commands are associated with their synonyms. 
;maybe store the player's actions in a list that can be analyzed. 

;have the player use search to unveil hidden things, or they search specifically for it. maybe they randomly discover 1 hidden item based on a role if just
;blank search. I'll have to have search that takes into account containers, objects, and features. It should also work by taking in an object that's a loc/area
;so that the user can say "search room" rather than "search for a hairbrush." assuming that any hidden items will be found in the process. whereas if you
;search for something specific, it'll reveal hidden items until you find that thing. there might be more though. 
;have search rely on other factors to determine success. that what it'll do, the room will have a search difficult (or each item does and the sum is the room's). 

;this will change the hidden-nature, or just show it? for some I think it makes more sense to show it all the game-time, for others, like a button under a counter
;you wouldn't necessarily see it. 
(defun search-area ()
) 

;(defun search-for (thing) 
;)

;this will take you back to the previous area/location. maybe it should function as a command, and as an object. back would change area.
;go back to the church would change locations. 
(defun back ()
)

;have this write *command-history* to a file. 
(defun goto-sleep ()
  (save-game)) 

(defun save-game () 
  (with-open-file (stream "~/Projects/french-resistance/save-game.txt" :direction :output
			  :if-exists :supersede)
    (let ((history (reverse *command-history*)))
      (dolist (cmd history)
	(format stream "~S~%" cmd)))))


(defun load-history ()
  (with-open-file (stream "~/Projects/french-resistance/save-game.txt")
    (let ((game nil))
      (loop for line = (read-line stream nil :eof) 
	 until (eq line :eof) do
	   (push (read-from-string line) game))
    (reverse game))))

;when I get an item it prints it out. That's a little strange, but at the same game-time I like it because it provides a summary, just a generic one though. ea
(defun load-game ()
  (setf *command-history (reverse (load-history)))
  (do-history (load-history)))

;NPCs and CONVERSATION
;======================================================================================================
(defun show-persons1 (area location)
  (dolist (i *persons*)
    (if (and (equal area (person-area i)) 
             (equal location (person-loc i)))
        (return (person-appearance i))))) 

;1-19-14
;this isn't working too hot right now, its calling on the wrong part of description. 
(defun show-persons ()
  (dolist (i (list-persons))
    (return (person-appearance i)))) ;watch this: it might not work for printing multiple persons. 

(defun list-persons ()
  (let ((lst nil))
    (dolist (i *persons*)
      (if (equal (person-location *player*)
                 (person-location i))
          (push i lst)))
    lst))

(defun list-person-names ()
  (let ((lst nil))
    (dolist (i (list-persons))
      (push (person-name i) lst))
    lst))

;write a function that generates a group of persons. maybe nazi soldiers, or maybe 

;GAMEPLAY
;=======================================================================================================

;this is where it'll handle detecting if the player is caught or not. Handle combat with the enemy. 

;COMBAT

;player will have a total number of bullets, and depending on what they shoot with, that clipsize will be sent to this function. 
(defun fire-weapon (ammo)
  (random (1+ ammo))) 

;EVENTS
;events will happen on the street, will have their own description added, and may add persons to the area. likelihood of certain events increases based on game-time of day, 
;length of occupation, and length of distance being traveled. 


;SYSTEM
;=======================================================================================================


;include checks here for access to object-functions, and do feature-commands!. don't actually need to with objects
;beccause its added when put in inventory, or if being held. 
(defun play ()
;  (print-description (describe-location))
  (add-feature-commands) ;more efficient if I added this to change-area...  
  (display-room)
  (format t "~%- ")
  (let ((read (parse (read-list))))
    (when (not (equal (car read) 'quit))
      (let ((cmd (find-command read)))
        (let ((obj nil))
          (cond ((find-area read) (setf obj (find-area read)))
                ((find-location read) (setf obj (find-location read)))
                ((find-entrance read) (setf obj (entrance)))
                ((find-object read) (setf obj (find-object read)))
                ((find-direction read) (setf obj (closest-along-route (find-direction read))))
                ((find-inventory read) (setf obj (find-inventory read)))
                ((find-container read) (setf obj (find-container read)))
		((find-feature read) (setf obj (find-feature read))))
          (do-command cmd obj)))
      (play)))) 
  
  

;stupid parser. probably as stupid as can be. this should return the command and the object if there is one. I'll have a do-command function
;should this be expanded to feature commands with and, like pick up the bottle and smash him on the head? or shoot that guy and that guy? 
(defun parse (phrase)
  (let ((cmd nil))
    (dolist (i phrase)
      (if (or (equal i 'quit)
              (member i *commands*)
              (member i *synonyms-list*)
              (member i (list-person-names))
              (member i (get-object-names))
              (member i (person-inventory *player*))
              (member i (directions-list)) 
              (member i (list-inventory))
              (member i (list-containers))
	      (member i (list-features))
              ;write one for paths. opening a door should be the same as going to that area, so list-area handles things like go to bathroom, but not
              ;open the door to the bathroom. 
              (member i (list-locations))
              (member i *entrance-synonyms*)
              (member i (list-areas)))
          (push i cmd)))
    (reverse cmd)))

;Write a function to act follow up questions. if you're going between rooms and there are stairs going up, going down, and a door, you should be able to
;say "stairs" and then it'll ask you: which ones? 

(defmacro find-in-phrase (phrase set)
  `(dolist (i ,phrase)
     (if (member i ,set)
         (return i))))

;this has quit working for some reason!wtf!
(defun find-command (parsed-phrase)
  (loop named commands for i in *commands* do
        (if (member i parsed-phrase)
            (return-from commands (car (member i parsed-phrase))))
        (loop named synonyms for i in *command-synonyms* do
              (loop for j in parsed-phrase do
                    (if (member j (car (cdr i)))
                        (return-from commands (car i)))))))
 
(defun find-area (parsed-phrase)
  (find-in-phrase parsed-phrase (list-areas)))

(defun find-location (parsed-phrase)
  (find-in-phrase parsed-phrase (list-locations)))

(defun find-entrance (parsed-phrase)
  (find-in-phrase parsed-phrase *entrance-synonyms*))

(defun find-direction (parsed-phrase)
  (find-in-phrase parsed-phrase (directions-list)))

(defun find-object (parsed-phrase)
  (find-in-phrase parsed-phrase (get-object-names)))

(defun find-inventory (parsed-phrase)
  (find-in-phrase parsed-phrase (list-inventory))) 

(defun find-container (parsed-phrase) 
  (find-in-phrase parsed-phrase (list-containers)))

(defun find-feature (parsed-phrase)
  (find-in-phrase parsed-phrase (list-features)))

;should I add some error messages in here? 
(defun do-command (cmd obj)
  (push (list cmd obj) *command-history*) ;this probably isn't the best place to do it as somegame-times cmd will be nil. 
  (cond ((and (not (on-street)) (equal cmd 'walk)) (change-area1 obj))
        ((and (not (on-street)) (null cmd) (member obj (list-areas))) (change-area1 obj))
        ((and (on-street) (equal cmd 'walk) (member obj (list-locations))) (change-location1 obj))
        ;this handles saying go inside, or walk inside which is essentially a synonym of enter, find another way to treat it as such? 
        ((and (on-street) (equal cmd 'walk) (equal obj (entrance))) (change-area1 obj)) ;need to check to see if obj is in, inside, in-side, or something else? 
        ((and (on-street) (null cmd)) (change-location1 obj));this is bad cause there's no way to check
        ((and (on-street) (equal cmd 'enter) (null obj)) (change-area1 (entrance))) ;enter is a distinct command from walk, but i'm not sure it should be
        ((and (on-street) (equal cmd 'enter) (equal obj (current-location))) (change-area1 (entrance))) 
        ((and (on-street) (equal cmd 'enter) (equal obj (entrance))) (change-area1 obj)) ;this one is error prone, must have a check to make sure that obj is a list of areas.
        ;need to create a clear idea of where a building can be entered so player isn't temped to try other entrances. Don't make it too static though. 
        ;I also need to be able to say something like Enter through the front, or go inside. 
        ((equal cmd 'time) (show-time))
        ((equal cmd 'take) (take obj))  
        ((equal cmd 'equip) (equip obj)) 
        ((equal cmd 'open) (open-> obj))
        ((equal cmd 'inventory) (show-inventory))

	((command-match-feature cmd obj) (match-command-feature-function cmd obj))

        ((equal cmd 'inspect) (inspect-object obj)))) ;I'm not sure that I want on-street to handle the descriptive part.  

;With features, I add all the recognized commands to the list, but those commands need to be converted to the appropriate 
;set/change-state command. so turn-on might be passed to parser, but should be converted to (set-state t)?


;this will prompt the player for a line, turn it into a list of chars, go through the whole list and as long as a space is not encountered
;cons that letter into a list; once a space is encountered it'll coerce that list of chars into a string, read-from-string on it and put it
;onto the list. PROBABLY INEFFICIENT, but its a read function so that probably doesn't matter. 
(defun read-list ()
  (let ((phrase (read-line))
        (lst nil)
        (word nil))
    (dolist (i (coerce phrase 'list))
      (if (not (equal i #\Space))
          (push i word)
        (progn 
          (push (coerce (reverse word) 'string) lst)
          (setf word nil)))) 
    (push (coerce (reverse word) 'string) lst)
    (mapcar #'read-from-string (reverse lst)))) 

        

;write a save/sleep function. This will have to write to a document. will need to save the instance of player
;strutcture. So should I write other variables, or just add to that strucure. I'll need to include missions that have been
;completed, persons that have died, and also changes to the environment (such as cutting cable of eiffel tower). 


;load function, will read from a document. 


;=================================================================================================================
;CREATION FUNCTIONS this is how a user can use the engine.   
;=================================================================================================================
(defun create-location ()
  (push (list (name-location) (create-area)) *map*)) ;I need to make sure that this saves the map. 
  
(defun name-location ()
  (read)) 

(defun write-location ()
  (read-list))

;these might be useful to trigger after certain events happen.
(defun rewrite-location (location)
  (setf (car (cdr (assoc location *map*))) (read-list)))

;write this so you can create multiple areas within a location. 
(defun create-area ()
  (let ((area (list (name-area) (write-area))))
    (let ((continue nil))
      (format t "would you like to add another area to this location? y/n ")
      (setf continue (read))
      (if (equal continue 'y)
          (while (equal continue 'y)
            (append area (list (name-area) (write-area)))
            (format t "would you like to add add another area to this location? y/n ")
            (setf continue (read))))
    area))) 

;this might be useful to use for hidden areas of the game. they won't be in the map file until something is triggered, and then this add area will be called. 
(defun add-area (location name description)
  (nconc (cdr (assoc location *map*)) (list name description))) ;uses nconc to preserve order of map so that first area is always the entrance. 
 
(defun name-area ()
  (read))

(defun write-area ()
  (read-list)) 

(defun rewrite-area (location area)
  (setf (second (cdr (assoc area (cdr (assoc location *map*))))) (read-list)))


;--------------------------------------------------
;--------------------------------------------------
;--------------------------------------------------
;ADDING/REMOVING Objects and Containers.
;--------------------------------------------------
;--------------------------------------------------
;--------------------------------------------------


;OBJECTS/ITEMS. these will allow objects to be removed from a container or area, and inserted into inventory. I need a way to hide
;items and locations from the player without deleting them from the map. maybe they have a t or nil value at end. this is a general function.
; I can write a more specific one for use in gameplay where an object will only be removed while the player is in its area. 
(defun remove-object (obj loc area)
  (if (member obj (car (get-objects))) ;but this means it'll only work if player is in area right? 
      (setf (cdr (assoc area (cdr (assoc loc *object-locations*))))
            (cdr (second (assoc area (cdr (assoc loc *object-locations*))))))
;    (setf (second (assoc area (cdr (assoc loc *object-locations*))))
    (delete (assoc obj (second (assoc area (cdr (assoc loc *object-locations*)))))
            (second (assoc area (cdr (assoc loc *object-locations*)))))))
                   
; must also remove from contents of a container. 1-19-14
(defun remove-from-container (obj)
  (dolist (i (list-all-contents)) ;this will find what container the object is in. 
    (when (member obj (second i))
      (let ((container (car i)))
	(setf (cdr (cdr (assoc container (get-containers))))
	      (remove obj (cdr (cdr (assoc container (get-containers))))))))))
	
;GACKGACKGACK 
(defun remove-obj (obj)
  (remove-from-container obj) 
  (if (member obj (car (get-objects)))
      (setf (cdr (assoc (current-area) (cdr (assoc (current-location) *object-locations*))))
            (cdr (cdr (assoc (current-area) (cdr (assoc (current-location) *object-locations*))))))
    (delete (assoc obj (cdr (assoc (current-area) (cdr (assoc (current-location) *object-locations*)))))
            (cdr (assoc (current-area) (cdr (assoc (current-location) *object-locations*)))))))
            

;WRITE ADD OBJECT FUNCTIONS. one for designing the game, another for adding an object that's already written into the gamespace. 
;this will be done for things like serving the player a cup of coffee, or handing him a letter. 


;write some general functions that'll give me a particular part of a list such as *map*. THis'll help clean up the code
;and make this more of an engine than a particular game, because it has to be easy to read if its gonna be reused. 

;this is for blowing up a safe or something. Gameplay version. Write a separate creating version.
(defun remove-container (container) 
  (if (member container (car (get-containers)))
      (setf (cdr (assoc (current-area) 
			   (cdr (assoc (current-location) *container-locations*))))
	    (cdr  (cdr (assoc (current-area) 
				 (cdr (assoc (current-location) *container-locations*))))))
      (delete 
       (assoc container (cdr (assoc (current-area) 
				       (cdr (assoc (current-location) *container-locations*)))))
       (cdr (assoc (current-area) 
		      (cdr (assoc (current-location) *container-locations*))))))) 

;--------------------------------------------------
;--------------------------------------------------
;--------------------------------------------------
;FEATURE MANAGMENT
;--------------------------------------------------
;--------------------------------------------------
;--------------------------------------------------

;add feature to the front. 
(defun add-feature (feature area loc)
  (push feature (cdr (assoc area (cdr (assoc loc *area-features*))))))
	

;delete feature
(defun delete-feature (feature)
  (let* ((lst (get-feature-loc+area feature))
	 (area (car lst))
	 (loc (second lst)))
    (setf (cdr (assoc area (cdr (assoc loc *area-features*))))
	  (remove (get-feature feature) (cdr (assoc area (cdr (assoc loc *area-features*)))))))) 

;move feature area
(defun move-feature-area (feature dest)
  (let* ((feat (get-feature feature))
	 (location-lst (get-feature-loc+area feature))
	 (area (car location-lst))
	 (loc (second location-lst)))
    (delete-feature feature)
    (add-feature feat dest loc)))

;move feature location
(defun move-feature-area+loc (feature dest-area dest-loc)
  (let* ((feat (get-feature feature))
	 (location-lst (get-feature-loc+area feature))
	 (area (car location-lst))
	 (loc (second location-lst)))
    (delete-feature feature)
    (add-feature feat dest-area dest-loc)))
    
;horribly inefficient. would be cheaper to include a location list on all features. but then why organize them by feature? for easy viewing? 
(defun get-feature-loc+area (feature)
  (let ((location nil))
    (block nested-loop 
      (dolist (loc *area-features*)
	(dolist (area (cdr loc))
	  (dolist (feat (cdr area))
	    (when (equal (car feat) feature)
	      (setf location (list (car area) (car loc)))
	      (return-from nested-loop location))))))))

(defun get-feature (feature)
  (let ((location nil))
    (block nested-loop 
      (dolist (loc *area-features*)
	(dolist (area (cdr loc))
	  (dolist (feat (cdr area))
	    (when (equal (car feat) feature)
	      (return-from nested-loop feat)))))))) 
