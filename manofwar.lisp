;MAN-OF-WAR

(defun make-map ()
  (make-array '(16 16)))

;ooooo, change it to hexadecimal instead? datd b so l33t
(defun set-map (map)
  (dotimes (i (array-dimension map 0))
    (dotimes (j (array-dimension map 1))
      (setf (aref map i j) (cons i j)))))

;this display map function will need to change the current space to be a *. an ! for the enemy. 
;this gets really ugly and imbalanced in double digits, so put some character formatting in. 
(defun display-map (map ship1 ship2)
  (format t "~%")
  (dotimes (i (array-dimension map 0))
    (dotimes (j (array-dimension map 1))
      (if (and (not (equal (manofwar-location ship1) (aref map i j)))
               (not (equal (manofwar-location ship2) (aref map i j))))
          ;also check for drones. how to compute illusory ships? 
          (format t "~10S " (aref map i j))
        (cond ((equal (manofwar-location ship1) (aref map i j)) (format t "~10S " '(d_____7)))
              ((equal (manofwar-location ship2) (aref map i j)) (format t "~10S " '(<_____b) )))))
;include a check for drones in more conds

    (format t "~%~%~%")))

;this isn't going to be adequite enough though because your shield power, and type of shield aren't the same,
;just like your hull might be damaged, but its a certain type of damaged hull. 
;use assoc on things like engine to find the proper part, and then use second on that list to modify it. 
(defstruct manofwar
  (captain nil)
  (name nil)
  (ship-class nil);can I create substructs that are classes of ships? 
  (power 100) ;when selecting power-system, depending on which they choose just set this to a number. 
  (engine '((max-speed 4) (acceleration 0))) ;should I include reverse thrusters? 
  (speed 0)
  (course nil) ;this will be a coordinate I think. 
  (location nil)
  (hull 0)
;'((integrity 0) (durability 0)))
  (shield 0)
;'((energy 0) (resistance nil) (weakness nil)))
  (shield-power nil)
;add cooldown. 
  (weapons '((nuke (hull-dmg 40) (shield-dmg 25) (power 8) (range 3) (range-penalty -10))
             (missiles (hull-dmg 5) (shield-dmg 1) (power 1) (range 2) (range-penalty -15))
             (gauss (hull-dmg 15) (shield-dmg 5) (power 10) (range 5) (range-penalty -10))
             ;particle beam will have additional power drain for each dmg point increase, and does extra damage. When its fired (damage-inc) is 
             ;added, and that adds to the 2 * inc to the power cost. Also needs to be changed so it is maintained, instead of a one-shot. 
             (particle-beam (hull-dmg 1) (shield-dmg 10) (damage-inc 0) (power 5) (range 100) (range-penalty 0))
             ;will have more drain the longer charged and do more damage. charge adds 5 * charge to dmg, and 2 * charge to power. 
             ;need to set it up so this doesn't fire automatically, but charges, and then you choose to fire it. 
             (ion-cannon (hull-dmg 0) (shield-dmg 15) (charge 0) (power 10) (range 4) (range-penalty -15))
             (cannons (hull-dmg 10) (shield-dmg 5) (power 5) (range 2) (range-penalty -20)))) 
  (computer nil)
  (access nil)
  (hacked nil)
  (drones '((drone1 nil) (drone2 nil)))
  (weapon-location nil)
  (shield-location nil)
  (engine-location nil)
  (drones-location nil)
  (computer-location nil)
  ;5 sections, engine goes in oneplace, computer/you somewhere, power, weapons, shield. give 20 health each. 
  (layout '((bow nil) (lowbow nil) (port nil) (starboard nil) (stern nil))))


;each drone has a type. It can be espionage: spy on enemies settings. sabotage: disrupt and disable systems
;deceive: mess with systems to show false data. attack: these will follow the ship and attack it. 
(defstruct drone 
  (type nil)
  (location nil)
  (move-pattern nil) ;this will be set to stationary, follow, evade, and path.
  (range 0)
  (hull 10)
  (speed 1)
)

(defun christen (ship)
  (format t "This ship hereby is recognized as the property of Knight-Captain: ")
  (setf (manofwar-captain ship) (read))
  (format t "~%And shall be known to the world as thus christened by him: ")
 ; (format t "~% and christened by him, thus known to the world from this point on as: ")
  (setf (manofwar-name ship) (read)))

;allowing you to program scripts for your ship would just be so fucking incredible. 

(defun backstory ()
  (format t "Here is a simulation of the future where interstellar war is fought using highly technical and dynamic engines of destruction, needing only the lightning-wit of a human captain to be operated. You cast yourself out into orbit, drift, propel, and zap your way between matter and through the void, encased in the cold weld-work of the day, you tend to the electric fire of its operations and work your will upon and through your man-of-war."))

(defun begin-battle ()
  (format t "Hark! There appears a heat signature out in the chill of space, a steel sun, a burning engine. The operator is surely aware of you as well; like two knights with shimmering armor, you each set your course towards the other."))

;need a function calculuate distance between ships. 

;COMMANDS OVER BASIC SYSTEMS: SHIELD, SPEED. 

;need to tweak shield and speed so that you can't exceed the upper bound. or maybe you can put as much into
;shields as you can spend. when the shields get knocked down does the power comeback? 
;Need to set this so shields can't drop below 0. same for speed. 
;shld
(defun set-shield (ship)
  (format t "Shields are currently at ~S% power. " (manofwar-shield ship))
  (format t "Set shield % to: ")
  (let ((set (read)))
    (setf (manofwar-shield ship) set)
    (when (> set 100) ;this doesn't check to make sure you have enough power anymore. To do that I'd have to set the shield and then set-power. 
      ;the problem then is it displays current shields at a really high value, even though you don't have enough power for that. 
      (format t "Error, you do not have enough power.~%")
      (set-shield ship))
    (setf (manofwar-shield ship) set)))

;  (set-power ship))

;spd
(defun set-speed (ship)
  (format t "Current speed: ~S " (manofwar-speed ship))
  (format t "Set speed to: ")
  (setf (manofwar-speed ship) (read))
  (set-power ship))

;this should be changed so that you can't enter a wrong course. 
;crs. Change it to put parens around coordinates automatically.
(defun set-course (ship)
  (format t "Current course is set to ~S." (manofwar-course ship))
  (format t "Change course to: ")
  (setf (manofwar-course ship) (read)))

;COMMANDS weapon systems. 
(defun weapons (ship target)
  (format t "~%Select which weapon to fire: ~%[1]nuke [2]missiles [3]particle-beam~%
[4]gauss [5]ion-cannon [6]cannons~%")
  (if (manofwar-hacked ship)
      (setf wpn (+ 1 (random 5)))
    (setf wpn (read)))
;  (let ((wpn (read)))
    (cond ((and (equal wpn 1) (check-power ship 'nuke)) (fire-weapon ship 'nuke target 0))
          ((and (check-power ship 'missiles) (equal wpn 2)) (fire-weapon ship 'missiles target 0))
          ;Here's a problem: it checks power before getting the pwr increase.need to set damage-inc of particle beam so it works with cost. 
          ((and (check-power ship 'particle-beam) (equal wpn 3)) (format t "Enter power-increase: ")
           (setf (second (assoc 'damage-inc (rest (assoc 'particle-beam (manofwar-weapons ship))))) (read)) ;sets damage-inc
           (setf mod (second (assoc 'damage-inc (rest (assoc 'particle-beam (manofwar-weapons ship))))));sets mod to damage-inc
           (particle-beam-cost ship) (fire-weapon ship 'particle-beam target mod))
          ;set up gauss to target a specific location on the ship to disable the system. 
          ((and (check-power ship 'gauss) (equal wpn 4)) (fire-weapon ship 'gauss target 0))
          ((and (check-power ship 'ion-cannon) (equal wpn 5)) (fire-weapon ship 'ion-cannon target 0))
          ((and (check-power ship 'cannons) (equal wpn 6)) (fire-weapon ship 'cannons target 0))
          (t (format t "Error, not enough power or invalid entry."))))

;returns t if there's enough power to use weapon. assumes that weapon variable is a list (weapon () () ()...)
;include this in the check for wpn with an int. 
(defun check-power (ship wpn)      
  (let ((weapon (assoc wpn (manofwar-weapons ship))))
    (if (> (manofwar-power ship) (second (assoc 'power (rest weapon))))
        t
      nil)))

;okay, this is weird, whenenver ship2 uses it, it sets ship1s power to like 69/2 or 310/33 or something. 
;change this so that if the damage-inc costs more power than you have, it's reduced until you have enough power. 
(defun particle-beam-cost (ship)
  (let ((power-inc (second (assoc 'damage-inc (rest (assoc 'particle-beam (manofwar-weapons ship)))))))
    (if (> (* 2 power-inc) (manofwar-power ship))
        (setf power-inc (/ (manofwar-power ship) 2))) ;is this right? 
    (setf (manofwar-power ship) (- (manofwar-power ship) (* 2 power-inc)))))

(defun ion-cannon-drain (ship)
  )

;make sure this works! DONT NEED THIS ANYMORE I DONT THINK
(defun restore-power (ship)
  (if (<= (manofwar-shield ship) 0)
      (setf (manofwar-power ship) (+ (manofwar-power ship) 20))
  (if (equal (manofwar-speed ship) 0)
      (setf (manofwar-power ship) (+ (manofwar-power ship) 40))
    (if (equal (manofwar-speed ship 1))
        (setf (manofwar-power ship) (+ (manofwar-power ship) 20))))))

;doesn't work particularly well right now. I'm having trouble setting it back to normal. 
;maybe I should change it so power changes within setting commands. 
(defun change-power(ship)
  (cond ((> (manofwar-shield ship) 50) (setf (manofwar-power ship) (- (manofwar-power ship) 20)))
        ((> (manofwar-shield ship) 0) (setf (manofwar-power ship) (- (manofwar-power ship) 10)))
        ((equal (manofwar-shield ship) 0) (setf (manofwar-power ship) (+ (manofwar-power ship) 10)))))

;I don't think the wpn-power is necessary, I have a separate thing to check weapon power. Set up a boolean, that if it's true then the player
;is currently hacking, and then reduce power by a certain amount if that's true. 
(defun set-power (ship)
  (setf (manofwar-power ship) (- 100  (* 3 (round(/ (manofwar-shield ship) 10.0)))
                                 (* (manofwar-speed ship) 20))))
;                                 wpn-power)))

;msg. should messages be able to send a trojan horse into the enemies system? seems too complex. 
(defun send-message ()
  (format t "Enter your message in quotes: ")
  (write-to-string (read)))
;  (read))

;HACKING!!!! right now it doesn't take any power from you, and you can really wreck the other player. 
;should probably include a roll for this too to see how effective it is? 

(defparameter *ship1Access* nil)
(defparameter *ship2Access* nil)

;this gives access to the player to hack other systems. Maybe this takes in a ship and target instead, and changes access there, then
;every round it checks (manofwar-access ship1) and if t, then it allows you to pick a hack. If not it says "get-info." One you have info
;it should print it out every round next to your info. 
(defun hack-info (ship target)
  (format t "Here is the enemies status: ")
  (print-status target t 0)
  (setf (manofwar-access ship) t))

;this will use scrambled status, but it needs to supercede the normal update. 
(defun scramble-info (target)
  
)

;this will make it so that whatever they enter for their pick, it'll choose something else. Maybe even numbers? 
(defun hack-pick (target)
  (format t "Enemy's next move will be random.~%")
  (setf (manofwar-hacked target) t))
  

;make hacking info a prerequisite of this? seems like hacking info is pretty useless if hacking a system gives you the info. 
(defun hack-power (target)
  (format t "Target's current power is at: ~S~%" (manofwar-power target))
  (format t "Drain power by how much?")
  (setf (manofwar-power target) (- (manofwar-power target) (read))))

(defun hack-speed (target)
  (format t "Target's current speed is ~S" (manofwar-speed target))
  (setf (manofwar-speed target) 0)
  (format t "Target is dead in the water."))

(defun hack-shield (target)
  (format t "Target's shield is currently at ~S% power.~%" (manofwar-shield target))
  (format t "Reduce it by: ")
  (setf (manofwar-shield target) (- (manofwar-shield target) (read))))

;might want to include something with this where the course is locked in for a few turns. 
(defun hack-course (target)
  (format t "Target's current heading is: ~S~%" (manofwar-course target))
  (format t "Set it to: ")
  (setf (manofwar-course target) (read)))

;create a function that creates a list of systems the player has access to on the enemy ship. 

(defun pick-hack (ship target)
  (if (not (manofwar-access ship))
      (progn
        (format t "Requesting access to ~S..............~%" (manofwar-name target))
        (format t "..forcing entry...%8.........%39...........%76.....%97~%")
        (format t ".....authentification retrievied......DONE..............Access granted.~%")
        (format t "Welcome lord ~S.~%" (manofwar-captain ship))
        (hack-info ship target) )
    (progn
      (format t "Select system to hack: ")
      (let ((pick (read)))
        (cond ((equal pick 'power) (hack-power target))
              ((equal pick 'speed) (hack-speed target))
              ((equal pick 'shield) (hack-shield target))
              ((equal pick 'course) (hack-course target))
              ((equal pick 'system) (hack-pick target))
              (t (format t "Error. Uncomputable entry.")))))))



;NAVIGATION  
;if the space is behind then the location will be negative, and if its to the right it'll also be neg
;ahead and to the left will be positive. I should reverse this so its easier to set location. NO
;I'll just subtract current distance from destination, and so if its negative it'll be added. 
(defun calculate-distance (loc1 loc2)
  (cons (- (car loc1) (car loc2))
        (- (cdr loc1) (cdr loc2))))

;I should divide the car by the cdr to get a single number. division won't work cause then a distance of 3 3
;means it'd be recorded as a distance of 1, when it should be 3. what about 10 3, is that only 3 away? that 
;isn't only 3 away. subtract it? get the absolute value. 7 spaces away is reasonable for 10 3 distance. 
(defun ship-distance (ship1 ship2)
  (let ((dist (calculate-distance (manofwar-location ship1) (manofwar-location ship2))))
    (if (equal (abs (car dist)) (abs (cdr dist)))
        (abs (car dist))
      (abs (- (abs (car dist)) (abs (cdr dist))))) ))


;this will return how many spaces away something is. so (1 2) (4 5) is a distance of (3 3)
;and if you can move 2 spaces per turn, and set your coordinate to (4 5) then you'll move (1 1) every turn
;how do I divide this up into spaces per turn. 
(defun sum-coordinates (loc)
  (+ (car loc) (cdr loc)))

;include a conditional here to check to see if distance is greater than the speed, and if it is move only by speed, if its less than then move by distance. problem is a negative distance will always be less than, so check for that
;YUCK. change to cond, and maybe set variables so I have less typing to do. 
(defun move (ship)
  (let ((dist (calculate-distance (manofwar-location ship) (manofwar-course ship))))
    (print dist)
    (print (manofwar-location ship))
;USING SPEED
    (if (and (>= (car dist) (manofwar-speed ship))
             (> (car dist) 0))
        (setf (manofwar-location ship) (cons (- (car (manofwar-location ship)) (manofwar-speed ship)) 
                                             (cdr (manofwar-location ship)))))
    (if (and (< (car dist) 0)
             (>= (- (manofwar-speed ship)) (car dist)))
        (setf (manofwar-location ship) (cons (+ (car (manofwar-location ship)) (manofwar-speed ship))
                                             (cdr (manofwar-location ship)))))
    (if (and (>= (cdr dist) (manofwar-speed ship))
             (> (cdr dist) 0))
        (setf (manofwar-location ship) (cons (car (manofwar-location ship))
                                             (- (cdr (manofwar-location ship)) (manofwar-speed ship)))))
    (if (and (< (cdr dist) 0)
             (>= (- (manofwar-speed ship)) (cdr dist)))
        (setf (manofwar-location ship) (cons (car (manofwar-location ship))
                                             (+ (cdr (manofwar-location ship)) (manofwar-speed ship)))))

;USING DISTANCE
    (if (and (< (car dist) (manofwar-speed ship))
             (> (car dist) 0))
        (setf (manofwar-location ship) (cons (- (car (manofwar-location ship)) (car dist))
                                             (cdr (manofwar-location ship)))))
    (if (and (< (car dist) 0)
             (< (- (manofwar-speed ship)) (car dist)))
        (setf (manofwar-location ship) (cons (+ (car (manofwar-location ship)) (car dist))
                                             (cdr (manofwar-location ship)))))
    (if (and (< (cdr dist) (manofwar-speed ship))
             (> (cdr dist) 0))
        (setf (manofwar-location ship) (cons (car (manofwar-location ship))
                                             (- (cdr (manofwar-location ship)) (cdr dist)))))
    (if (and (< (cdr dist) 0)
             (< (- (manofwar-speed ship)) (cdr dist)))
        (setf (manofwar-location ship) (cons (car (manofwar-location ship))
                                             (+ (cdr (manofwar-location ship)) (cdr dist)))))))
;write a function to transform this into a cond. 

;DESTRUCTION. returns t if hull is below 0. 
(defun destruction (ship)
  (if (<= (manofwar-hull ship) 0)
      t
    nil))
    
;rewrite this to work with new values? 
(defun take-damage (ship damage)
  (if (> (manofwar-shield ship) 0)
      (setf (manofwar-shield ship) (- (manofwar-shield ship) damage))
    (setf (manofwar-hull ship) (- (manofwar-hull ship) damage))))

;WEAPONS.
(defun accuracy-roll (modifier)
  (+ modifier 50 (random 50))) ;the + 25 ensures damage will never be less than 25 %

;returns the damge dealt. 
(defun damage-penalty (damage mod)
  (let ((roll (accuracy-roll mod)))
    (if (> roll 0) ;if the modifier is < -100 then the roll will return negative damage, and thus boost health
        ;so this has it return 0 if the weapon is too far out of range. 
        (round (* damage (/ roll 100.0)))
      0)))

;need to adapt this to work with ion-cannon and particle beam? YES
(defun fire-weapon (ship weapon target dmg-inc)
  (let* ((wpn (rest (assoc weapon (manofwar-weapons ship1))))
         (dist (ship-distance ship target))
         (mod (* (- dist (second (assoc 'range wpn))) (second (assoc 'range-penalty wpn)))))
    (if (< (manofwar-shield target) 0)
      (let ((damage (+ dmg-inc (second (assoc 'hull-dmg wpn)))))
        (take-damage target (damage-penalty damage mod)))
      (let ((damage (+ dmg-inc (second (assoc 'shield-dmg wpn)))))
        (take-damage target (damage-penalty damage mod))))))


;USER INTERFACE, crudimentry. 
;keep asking for input until they enter a valid shortcut for all functions. 

;here, write a function prompting the user for a command, display how many commands left in chain. 

;print these out really nicely formated. 
(defun print-status (ship stream depth) ;ignore depth, but its required for some reason. 
  (format stream "Starship ~S~%" (manofwar-name ship))
  (format stream "NAVIGATION ~%")
  (format stream "Location: ~S~%" (manofwar-location ship))
  (format stream "Course: ~S~%" (manofwar-course ship))
  (format stream "Speed: ~S~%" (manofwar-speed ship))
  (format stream "POWER MANAGEMENT~%")
  (format stream "Power ~S~%" (manofwar-power ship))
  (format stream "Shield ~S~%" (manofwar-shield ship))
  (format stream "Hull ~S~%" (manofwar-hull ship)))

;set it up to randomly rearrange letters also. how to set it up so it'll print this instead? include a conditional in to see if a certain
;function is t, if it is and the status has been hacked, then tell show-round to print this instead of the other. 
(defun scramble-status (ship)
  (format t "Starship ~S~%" (random 1000000000000000))
  (format t "NAVIGATION ~%")
  (format t "Location: ~S~%" (random 1000000000000000))
  (format t "Course: ~S~%" (random 1000000000000000))
  (format t "Speed: ~S~%" (random 1000000000000000))
  (format t "POWER MANAGEMENT~%")
  (format t "Power ~S~%" (random 1000000000000000))
  (format t "Shield ~S~%" (random 1000000000000000))
  (format t "Hull ~S~%" (random 1000000000000000)))

;GAME
(defun start-game()
  (setf map (make-map))
  (set-map map)
  (setf ship1 (make-manofwar :location '(0 . 0)
                             :course '(0 . 0) ;this is to make sure course is never nil in game run. 
                             :speed 2
                             :hull 100
                             :shield 0))
  (setf ship2 (make-manofwar :location '(15 . 15)
                             :course '(15 . 15)
                             :speed 2
                             :hull 100
                             :shield 0)) 
  (display-map map ship1 ship2)
  (show-round map ship1 ship2 0))

;this lets you each customize your ship. 
(defun custom-game ()
)

(defun choose-game ()
  (backstory)
  (format t "~%Would you like to customize your ship before departing? (y / n)" )
  (let ((choice (read)))
    (if (equal choice 'n)
        (start-game)
      (custom-game))))
  
;maybe write these as smaller functions. 
(defun show-round(map ship1 ship2 count)
  (set-power ship1)
  (set-power ship2)
  (format t "Round Count: ~S~%" count)
  (when (not (equal (manofwar-location ship1)
                    (manofwar-course ship1)))
    (move ship1))
  (when (not (equal (manofwar-location ship2)
                    (manofwar-course ship2)))
    (move ship2))
  (display-map map ship1 ship2)
  (if (manofwar-hacked ship1)
      (progn
       (setf (manofwar-hacked ship1) nil) ;this resets so that their next choice won't be random. 
       (random-command ship1 ship2))
    (get-command ship1 ship2))
  (if (manofwar-hacked ship2)
      (progn
       (setf (manofwar-hacked ship2) nil)
       (random-command ship2 ship1))
    (get-command ship2 ship1))
  (cond ((destruction ship1) (format t "~S was destroyed." (manofwar-name ship1)))
        ((destruction ship2) (format t "~S was destroyed." (manofwar-name ship2)))
        (t (show-round map ship1 ship2 (+ 1 count)))))

(defun resume-game (map ship1 ship2 count)
  (show-round map ship1 ship2 count))
             
(defun get-command (ship target)
  (print-status ship t 0)
  (format t "[spd] [crs] [shld] [wpn] [hck] [drn]~%")
  (format t "Enter your command: ")
  (let ((cmd (read)))
    (cond ((equal cmd 'spd) (set-speed ship))
          ((equal cmd 'crs) (set-course ship))
          ((equal cmd 'shld) (set-shield ship))
          ((equal cmd 'wpn) (weapons ship target))
          ((equal cmd 'hck) (pick-hack ship target))
          (t (format t "Error, reenter command.") (get-command ship target)))))

(defun random-command (ship target)
  (print-status ship t 0)
  (format t "[spd] [crs] [shld] [wpn] [hck] [drn]~%")
  (format t "Enter your command: ")
  (setf choice (read))
  (format t "~%Error, wrong!~%")
  (let ((pick (random 5)))
   (cond ((and (equal pick 0) (not (equal choice 'spd))) (set-speed ship))
          ((and (equal pick 1) (not (equal choice 'crs))) (set-course ship))
          ((and (equal pick 2) (not (equal choice 'shld))) (set-shield ship))
          ((and (equal pick 3) (not (equal choice 'wpn))) (weapons ship target))
          ((and (equal pick 4) (not (equal choice 'hck))) (pick-hack ship target)))))

