;FRENCH RESISTANCE

;so this is kind of gross, but I can't really use punctuation except periods. I guess I'll have to write it like Hemingway. 

;import stuff from onlisp? so far I'm only using explode, but I'm sure these might be helpful. do-tuples/o might be useful. 
(load "~/Projects/onlisp.lisp")
(load "~/Projects/patmatch-Novak.lisp") ;this will be used in conversation system. 

;GLOBAL VARIABLES AND STRUCTURES 
;======================================================================================
(defparameter *map* '((home
                       (your small apartment in conquered Paris. No longer as comforting as home should be.)
                       (bedroom (there rests your bed and on your dresser a picture of your parents and a girl you once knew.))
                       (bathroom (you stand in front of your sink and stare into the mirror.)))
                      (eiffel-tower 
                       (symbol of the glory of fallen France) ;description
                       (bottom (you stand undr the looming tower))
                       (top (you stare out of the lit and conquered city below))) 
                      (cafe
                       (a cozy and small cafe. supposedly there is a resistance organization that meets here.)
                       (main-area (there are tables with candles lit and patrons sipping coffee. there are less people here than usual and even with conversations happening the place seems hushed.))
                       (basement (the basement is lit by lamps showing tables atopped with maps and documents bearing plans.)))
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

;1-2-14 I'm not sure if this is the best way to handle it, or I should have a list of items, each followed by their stats/functions
;and then have a list of their locations. 
(defparameter *location-objects* '((home
                                    (bedroom 
                                     (drawer (gun bible))
                                     (cabinet (wine cigarettes))))
                                   (eiffel-tower
                                    (top
                                     (elevator-cable . cut))))) ;the . represents an action that can be performed. 
                        
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
                                      (bible pistol) bedroom home)))
                                         

(defstruct character
  (name nil)
  (appearance nil)
  (location nil)
  (inventory nil)
  (suspicion 0))

(defparameter *player* (make-character
                        :name '(Jacques Gallion)
                        :appearance '(your standard frenchman though strungout by the occupation)
                        :location '(bedroom home))) 

(defparameter *commands* '(explore walk pickup use talk))

(defparameter *inventory* ())

(defparameter *characters* `(,(make-character
                              :name 'barista
                              :appearance '(a middle-aged man stands behind the bar)
                              :location '(main-area cafe))))


;DESCRIPTION and NAVIGATION FUNCTIONS
;=====================================================================================================
;(defun character-locations ()
 ; (let ((lst nil))
  ;  (dolist (i *characters*)
   ;   (setf lst (cons (character-location i) lst)))
   ; lst))
              
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

;changing location should by default change the area. to the first one everytime, or depending on the path? on the path I think. 
;have it print the description of the location here only. 
(defun change-location ()
  (setf (second (character-location *player*)) (read))
  (setf (first (character-location *player*)) (car (third (assoc (current-location) *map*))))
  (print-description (describe-location)) )

(defun change-area ()
  (setf (first (character-location *player*)) (read)))

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

(defun show-characters (area location)
  (dolist (i *characters*)
    (if (and (equal area (character-area i)) 
             (equal location (character-loc i)))
        (return (character-appearance i)))) ) 
  
(defun show-object (obj) 
  `(there is a ,(car obj) of interest)) ;watch out for this first, it's to avoid displaying contents of an item. 

;this will just show the whole contents, how do I make it only show the interactable object and not the contents? 
(defun show-objects (area location)
  (mapcar #'show-object (cdr (assoc area (cdr (assoc location *location-objects*))))) )

(defun append-area ()
    (append (describe-location)
           (describe-area)
           (show-characters (current-area) (current-location))
           (describe-paths)))

;quite inelegant. 
(defun list-area ()
  (if (show-characters (current-area) (current-location))
      (list 
       ;(describe-location)
       (describe-area)
       (show-characters (current-area) (current-location))
       (describe-paths))
    (list 
     ;(describe-location)
          (describe-area)
          (describe-paths))))


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


;NPC and CONVERSATION
;======================================================================================================


;GAMEPLAY
;=======================================================================================================

;this is where it'll handle detecting if the player is caught or not. Handle combat with the enemy. 


;SYSTEM
;=======================================================================================================

;gack, get rid of soon.
(defun play ()
  (let ((cmd (read)))
    (if (not (equal cmd 'quit))
        (progn 
          (if (equal cmd 'look)
              (display-room))
          (play))     )))

;write a save function

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
