;FRENCH RESISTANCE
 
;so this is kind of gross, but I can't really use punctuation except periods. I guess I'll have to write it like Hemingway. 

;import stuff from onlisp? so far I'm only using explode, but I'm sure these might be helpful. do-tuples/o might be useful. 
(load "~/Projects/onlisp.lisp")
(load "~/Projects/patmatch-Novak.lisp") ;this will be used in conversation system. 

;import a dictionary so I won't have to enter synonyms manually, it'll just look it up in the dictionary. will also aid conversation. 

;GLOBAL VARIABLES AND STRUCTURES 
;======================================================================================
(defparameter *map* '((home
                       (your small apartment in conquered Paris. No longer as comforting as home should be. more like a trap than a dwelling. 
                             they will know right where to find you.)
                       (bedroom (there rests your bed as the sun strikes through the window to shine on it.)) ;and on your dresser a picture of your parents and a girl you once knew.
                       (bathroom (you stand in front of your sink and stare into the mirror.)))
                      (apartment
                       (this is your apartment building.)
                       (stair-case (you stand about halfway up the building. the stairs lead down the street.))
                       (landladys-door (you stand in front of your landladys door.))) ;have the player knock to talk to her. 
                      (eiffel-tower 
                       (symbol of the glory of fallen France. Unconquered by Hitler.) ;description
                       (bottom (you stand undr the looming tower))
                       (top (you stare out of the lit and conquered city below.))) 
                      (cafe
                       (a cozy and small cafe. supposedly there is a resistance organization that meets here.)
                       (main-area (there are tables with candles lit and patrons sipping coffee. there are less people here than usual 
                                         and even with conversations happening the place seems hushed.))
                       (bathroom (a typical public restroom.)) ;i wish I could randomize someone being in a stall. 
                       (basement (the basement is lit by lamps showing tables atopped with maps and documents bearing plans.)))
                      (movie-theatre 
                       (a movie theatre still as popular as before. perhaps more so because of the need for escapism. but its not so easy
                          to forget your worries as its just as popular with the occupying soldiers as with the the occupied.)
                       (front (a couple soldiers stand in front speaking in German while some young girls chat and a family walks away from
                                 the building down the street.))
                       (lobby )
                       (theatre-1 )
                       (theatre-2 )
                       (theatre-3 ))
                      (church 
                       (a bleak church in sullen city. here less reverance is paid than before but more prayers are muttered wept or cried.)
                       (sanctuary (there are a few other people in the pews. some with rosaries and some facing a figure of christ or his mother.
                                        this is not the fear of god they feel but the fear of man. the anxiety of being tread upon.))
                       (confession-booth (you stare ahead at the wooden-shine of the door and see in your peripheral the ephermeral glimpse of the
                                              silent priest.)))
                      (hotel 
                       ()
                       )
                      (nightclub 
                       )
                      (park
                       )
                       
                      
                      ;treat this as an area instead of a location. the location will stay and represent which node you are at. 
                      (streets ;this is using a system where all streets are considered one unit, and you just travel between nodes on it. 
                       (a typically oppressed though still lovely street of paris. The boast of the nazis hang red and black over buildings.
                          their harsh language is strewn across buildings while commands bark from posters.)
                       (apartment (cafe 2) (movie-theatre 3) (eiffel-tower 10))
                       (cafe (apartment 2) (movie-theatre 2) (eifel-tower 8))
                       (movie-theatre (apartment 3) (cafe 2) (eiffel-twoer 6))
                       (eiffel-tower (apartment 10) (cafe 8) (movie-theatre 6)))
                      ;I might also want to make a street system where its based on individual roads, which might be more interesting,
                      ;but runs the risk of being too complicated or getting in the way of playing. 
       

                      (street
                       (a typical oppressed though still lovely street of Paris. the boast of the nazis hang red and black
                          over buildings.)
                       (north ?x) ;set these to values based on where you're leaving from. 
                       (south ?x)
                       (east ?x)
                       (west ?x))))

;1-2-13
;this might work if the main map gets too big. I could keep this global variable which will contain things like the locations in
;paris, and locations on the outskirts, etcetera. 
(defparameter *france* `(,*map*))

;I'm not even using this right now except to show the player the paths, and that could just be an element of description in the map list. 
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

;I could maybe make paths like for 314. (home (cafe 2) (eiffel-tower 10)). This would get rid of the need for the player to draw
;a map, and would allow a kind of quick travel between locations. it might be hard to create tension that way? perhaps
;each location has multiple ways to get somewhere so (home (cafe (main-st 1) (backroads 2)) (eiffel-twoer (main-st 7) (backraods 12))
;I'm worried this would make roads just like one area though, rather than having roads connect to each other. 
;this will all be handled at the street, so a door will always lead to another area, or to a street which will take you to a new location. 
;so there's essentiall a home-street, cafe-street, etcetera, and each path has a distance, maybe a traffic-rating (changing based on the day?)
;and these numbers will affect the possiblity of encountering resistance. I think certain locations should also have a possibility of being
;occupied, so perhaps you go to a cafe to join the resistance, but there are some soldiers there. Maybe this is what I should keep
;in the street variable, just a list of all entries into the street, and how far they are from each other. This will create a network/graph. 

;1-2-14 I'm not sure if this is the best way to handle it, or I should have a list of items, each followed by their stats/functions
;and then have a list of their locations. 
(defparameter *location-objects* '((home
                                    (bedroom 
                                     (drawer (gun bible))
                                     (cabinet (wine cigarettes))))
                                   (eiffel-tower
                                    (top
                                     (elevator-cable . cut))))) ;the . represents an action that can be performed. 

(defparameter *object-locations* '((home (bedroom 
                                         (girl-photo (you wonder what happened to her.
                                                            its been a while since you last saw her so you have you suspicions.)
                                                       (Ontop of your dresser stands a picture of a girl you once knew) ;this is a descrition to be used. 
                                                       bedroom home) ;this is how to find where to place it. might change to lists if its in
                                                      ;multiple places. 
                                         (family-photo (you stare at the photo of your deceased parents who loved you and wanted you to live
                                                            and be happy. two people who could not have imagined this happening. 
                                                            just like the rest of you. Except lucky enough to have never been proven wrong.)
                                                       (and beside it stands a photo of your parents.) 
                                                       bedroom home)
                                         (newspaper (everyone knows the press is compromised and that this is particualry bad 
                                                              even for propaganda. However it probably looked suspicous to quit reading it.
                                                              and as of yet you havent worked up the courage to buy one of those underground
                                                              papers youve heard about. listening the bbc broadcast each night like everyone
                                                              has been enough for you so far.)
                                                    (Over on the table is a newspaper.)
                                                    bedroom home)))))

;pattern matching? this causes a problem when there are two similar objects like the photo of the girl and of your parents. otherwise it should just be
;able to find the word the player input and then display that.  
(defparameter *object-synonyms* '((girl-photo (picture of girl) (girls picture) (photo of girl) (girls photo) (photo girl) (picture girl) (girl picture)
                                              (girl photo))))

;these are all the objects that you can pickup.
(defparameter *items* nil)

(defparameter *container-locations* '((home (bedroom
                                             (dresser (your dresser. Containing the bible you used to consider quaint. something feels more 
                                                            relavant and comforting about it now. But no less dubious. 
                                                            next to it rests the  pistol you never thought would be needed.
                                                            then there is your sparse and innocent journal which you hope they dont find.)
                                                      (bible pistol) bedroom home))))) 
                                         
                                 
                        
;different way of doing of objects. I think I should combine this with the organization by location as above.  
(defparameter *objects* '((friend-photo (you wonder what happened to them.
                                        its been so long since you last saw them so you have you suspicions.)
                                        (ontop of the dresser) ;this is a descrition to be used. 
                                        bedroom home) ;this is how to find where to place it. might change to lists if its in
                                                      ;multiple places. 
                          (family-photo (you stare at the photo of your deceased parents who loved you and wanted you to live
                                             and be happy. two people who could not have imagined this happening. 
                                             just like the rest of you. Except lucky enough to have never been proven wrong.)
                                        (ontop of the dresser) 
                                        bedroom home)
                          (newspaper (everyone knows the press is compromised and that this is particualry bad 
                                               even for propaganda. However it probably looked suspicous to quit reading it.
                                               and as of yet you havent worked up the courage to buy one of those underground
                                               papers youve heard about. listening the bbc broadcast each night like everyone
                                               has been enough for you so far.)
                                     (ontop of the dining table)
                                     bedroom home)))
   
   
;these items won't have general descriptions I'm assuming, just description of contents.                                   
(defparameter *containers* '((dresser (your dresser. Containing the bible you used to consider quaint. something feels more 
                                            relavant and comforting about it now. But no less dubious. 
                                            next to it rests the  pistol you never thought would be needed.
                                            then there is your sparse and innocent journal which you hope they dont find.)
                                      (bible pistol) bedroom home))) ;fourth and fifth acess area and location. 


                                         

(defstruct character
  (name nil)
  (appearance nil)
  (location nil)
  (inventory nil)
  (loyalty nil)
  (undercover nil)
  (anxiety 0)
  (suspicion 0))

(defparameter *player* (make-character
                        :name '(Jacques Gallion)
                        :appearance '(your standard frenchman though strungout by the occupation)
                        :location '(bedroom home))) 

(defparameter *commands* '(explore inspect walk pickup use talk open shoot consider)) ;consider? this might give you clues when reading memos or something.
;does open work on doors and containers. does pickup work like inspect on items that can't be inventoried. 

(defparameter *command-synonyms* '((explore (look-around lookaround look search investigate))
                                   (walk (travel move go goto go-to))
                                   (inspect (investigate look-at lookat view check-out checkout check search scan watch see))))

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




(defparameter *inventory* ())

(defparameter *characters* `(,(make-character
                              :name 'barista ;proprieter. Create synonyms for each character. Or just for occupations? 
                              :appearance '(a middle-aged man stands behind the bar)
                              :location '(main-area cafe))))


(defparameter *days-since-occupation* 187) ;I don't actually know how many days it should be. maybe the game should have chapters that are years or seasons
;this could help divide the world up, so you're in the country at one part, then maybe you're in paris, and then later the unoccupied zone, etcetera. 

(defparameter *days-in-resistance* 0) ;intialize this once the game begins, or perhaps once the player successfully joins/completes a mission. 

(defun intro ()
  `(it has been ,*days-since-occupation* days since the horrible machine had quaked through the countryside and split your country in two.
       burdening your people with draconian law and tyranny. taking their goods from them to feed what ails them. the land stripped of its bounty.
       its people stripped of their freedom. and you stripped of your honor. but not your pride.
       It is time for resistance. It is time for combot. It is time for liberation. It is time for a free France.)) 
;the three papers I recall being published were resistance, liberation, and combat. 
       

;DESCRIPTION and NAVIGATION FUNCTIONS
;=====================================================================================================

;1-3-14 rewrite a lot of the functions for moving the player. you should be prompted where to go, but say where to go. walk to the cafe. run to the eiffel tower
;sneak to the movie-theatre. 
         

;this will have to include synonyms for street.
(defun list-areas () 
  (let ((lst nil))
    (dolist (i (cdr (cdr (assoc (current-location) *map*))))
      (push (car i) lst))
    (push 'streets lst)
    (push 'street lst)))  ;this pushes streets onto the list because every location will have a way out of it. 

(defun list-locations ()
  (let ((lst nil))
    (dolist (i *map*)
      (push (car i) lst))
    lst))
     
;GET AREA OR LOCATION 

(defun current-location ()
  (second (character-location *player*)))

(defun current-area ()
  (car (character-location *player*)))
       
(defun character-loc (character)
  (second (character-location character)))

(defun character-area (character)
  (car (character-location character)))

(defun get-area () 
  (assoc (car (character-location *player*)) (cdr (get-location)))) 
;  (assoc (car (character-location *player*)) (cdr (assoc (second (character-location *player*)) *map*))))
 
(defun get-location ()
  (assoc (second (character-location *player*)) *map*)) 

(defun describe-location ()
  (second (get-location))) 

;this could use the player-location instead of area, just always describe where they are. 
(defun describe-area ()
  (second (get-area)))  

;MOVE PLAYER FUNCTIONS

;changing location should by default change the area. to the first one everytime, or depending on the path? on the path I think. 
;have it print the description of the location here only. 
(defun change-location ()
  (setf (second (character-location *player*)) (read))
  (change-area1 (car (third (assoc (current-location) *map*)))))

(defun change-location1 (location)
  (setf (second (character-location *player*)) location)
  (change-area1 (car (third (assoc (current-location) *map*)))))

(defun change-area ()
  (setf (first (character-location *player*)) (read))
  (if (on-street)
      (on-street)
    (display-room)))

(defun change-area1 (area)
  (setf (first (character-location *player*)) area)
  (if (on-street)
      (on-street)))
;    (display-room)) )

;being on the street will trigger traveling to another location. 
(defun on-street ()
  (or (equal (first (character-location *player*)) 'streets)
      (equal (first (character-location *player*)) 'street))) 

;change this so that the street is randomized each visit. might have a patrol coming through, might be merchants (though this should be more static)
;and pedestrians. a character you recognize might even appear (which could be a problem if they're an enemy who recongizes you). 
(defun describe-street ()
  (print-description (second (assoc 'streets *map*))))
      
;PATHS

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
           (show-characters (current-area) (current-location))
           (describe-paths)))

;OBJECTS

(defun get-objects ()
  (cdr (assoc (current-area) (cdr (assoc (current-location) *object-locations*)))))

(defun get-object-names ()
  (let ((lst nil))
    (dolist (i (get-objects))
      (push (car i) lst))
    (reverse lst)))

(defun inspect-object (obj)
  (print-description (second (assoc obj (get-objects))))
  (fresh-line))

;I'm going to try writing it like this. append each descritpion of the object in the room. this will assume that the descriptions are accurate 
;I'm not treating things like tables and such as objects. I really want a robust system where each object has a description, and objects ontop of it,
;or objects in it. a gun would have bullets in it. I could have show-surface `(there is a ,x and ,y and ,z on the ,surface). what I'm doing now allows for
;more narrative construction/variation, but limits players ability to interact with things like table, dresser, etcetera. the player won't be able to
;kick over a table, or hide in a closet. 
(defun show-objects ()
  (let ((lst nil))
    (dolist (i (get-objects))
      (push (third i) lst))
    (apply #'append (reverse lst)))) 

;(defmacro when (condition &rest body)
 ; `(if ,condition (progn ,@body)))
;(when-cond ((test1 do1)) ((test2 do2))) this will work where multiple things might be true, and if they are they should be done. 
;so clauses will be a list ((t1) (d2))
(defmacro when-cond (clauses))
  

;quite inelegant. should this also list the objects? 
(defun list-area ()
  (let ((lst nil))
    (when (describe-area)
      (push (describe-area) lst))
    (when (on-street)
      (push (describe-street) lst))
    (when (show-characters)
      (push (show-characters) lst))
    (when (show-objects)
      (push (show-objects) lst))
    (when (describe-paths)
      (push (describe-paths) lst))
;    (push (describe-area) lst)
    (reverse lst))) 
      


;why does this produce nil between certain things? 
(defun print-room ()
  (dolist (i (list-area)) (print i))) 

;why does it cut the list short? 
(defun display-room ()
  (mapcar #'print-description (list-area)))

  

;PLAYER ACTIONS 
;========================================================================================================

;use read-line to get input, then search through it to see if any words in it are commands, and then search it for a likely
;object. Do I need to create a separate list where commands are associated with their synonyms. 
;maybe store the player's actions in a list that can be analyzed. 


;NPCs and CONVERSATION
;======================================================================================================
(defun show-characters1 (area location)
  (dolist (i *characters*)
    (if (and (equal area (character-area i)) 
             (equal location (character-loc i)))
        (return (character-appearance i)))) ) 

(defun show-characters ()
  (dolist (i (list-characters))
    (return (character-appearance i)))) ;watch this: it might not work for printing multiple characters. 

(defun list-characters ()
  (let ((lst nil))
    (dolist (i *characters*)
      (if (equal (character-location *player*)
                 (character-location i))
          (push i lst)))
    lst))

(defun list-character-names ()
  (let ((lst nil))
    (dolist (i (list-characters))
      (push (character-name i) lst))
    lst))

;write a function that generates a group of characters. maybe nazi soldiers, or maybe 

;GAMEPLAY
;=======================================================================================================

;this is where it'll handle detecting if the player is caught or not. Handle combat with the enemy. 

;COMBAT

;player will have a total number of bullets, and depending on what they shoot with, that clipsize will be sent to this function. 
(defun roll-shot (ammo)
  (random (1+ ammo)))

      


;SYSTEM
;=======================================================================================================


(defun play ()
  (print-description (describe-location))
  (display-room)
  (format t "~%- ")
  (let ((read (parse (read-list))))
    (when (not (equal (car read) 'quit))
      (let ((cmd (find-command read)))
        (let ((obj nil))
          (cond ((find-area read) (setf obj (find-area read)))
                ((find-location read) (setf obj (find-location read)))
                ((find-object read) (setf obj (find-object read))))
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
              (member i (list-character-names))
              (member i (get-object-names))
              (member i (character-inventory *player*))
              ;write one for paths. opening a door should be the same as going to that area, so list-area handles things like go to bathroom, but not
              ;open the door to the bathroom. 
              (member i (list-locations))
              (member i (list-areas)))
          (push i cmd)))
    (reverse cmd)))

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

(defun find-object (parsed-phrase)
  (find-in-phrase parsed-phrase (get-object-names)))

(defun do-command (cmd obj)
  (cond ((and (not (on-street)) (equal cmd 'walk)) (change-area1 obj))
        ((and (on-street) (equal cmd 'walk)) (change-location1 obj))
        ((equal cmd 'inspect) (inspect-object obj)))) ;I'm not sure that I want on-street to handle the descriptive part. 

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
;completed, characters that have died, and also changes to the environment (such as cutting cable of eiffel tower). 

;load function, will read from a document. 

;remove parens. 

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
            (t (cons (char-downcase item) (tweak-text rest nil nil))) ) ) ) )

(defun print-description (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))


;write some general functions that'll give me a particular part of a list such as *map*. THis'll help clean up the code
;and make this more of an engine than a particular game, because it has to be easy to read if its gonna be reused. 
