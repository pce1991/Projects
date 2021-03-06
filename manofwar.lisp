;MAN-OF-WAR

;Call (start-game) to begin a match. Currently only supports two human players. 

(defparameter *round* 0) ;write a delayed fire function that executes once round reaches a certain point. 

(defun make-map ()
  (make-array '(16 16)))

;ooooo, change it to hexadecimal instead? datd b so l33t
(defun set-map (map)
  (dotimes (i (array-dimension map 0))
    (dotimes (j (array-dimension map 1))
      (setf (aref map i j) (cons i j)))))

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
    (format t "~%~%"))) 

(defstruct manofwar
  (captain nil)
  (name nil)
  (ship-class nil);can I create substructs that are classes of ships? 
  (max-power 100)
  (power 100) ;when selecting power-system, depending on which they choose just set this to a number. 
  (drain 0)
  (engine '((max-speed 2) (acceleration 0))) ;should I include reverse thrusters? 
  (max-speed 2)
  (speed 0)
  (course nil) ;this will be a coordinate I think. 
  (location nil)
  (hull 0)
;'((integrity 0) (durability 0)))
  (shield 0)
  (max-shield 100)
  (shield-drain 0) ;unused right now. 
;'((energy 0) (resistance nil) (weakness nil)))
  (shield-power nil)
;add cooldown. 
  ;YIKES, why does changing a stat in one of these change it for all other instances of this variable. when it doesn't for other things. is this a problem
  ;with particle beam? I think it is. 
  (weapons '((nuke (hull-dmg 50) (shield-dmg 30) (power 25) (range 2) (range-penalty -10))
             (missiles (hull-dmg 4) (shield-dmg 1) (power 20) (range 2) (range-penalty -15))
             (gauss (target-dmg 20) (hull-dmg 10) (power 20) (range 3) (range-penalty -20)) 
             ;particle beam will have additional power drain for each dmg point increase, and does extra damage. When its fired (damage-inc) is 
             ;added, and that adds to the 2 * inc to the power cost. Also needs to be changed so it is maintained, instead of a one-shot!
             ;problem, damage-inc will damage the hull also, even the hull-dmg is 0. 
             (particle-beam (hull-dmg 0) (shield-dmg 30) (power 10) (range 10) (range-penalty 0)) 
             ;will have more drain the longer charged and do more damage. charge adds 5 * charge to dmg, and 2 * charge to power. 
             ;need to set it up so this doesn't fire automatically, but charges, and then you choose to fire it. Have ion cannon
             ;do damage to max-shield rather than do some power drain.
             (ion-cannon (hull-dmg 0) (shield-dmg 15) (power-damage 15) (power 5) (range 10) (range-penalty -15)) ;damage power.max-power - 5 * charge
             (cannons (hull-dmg 15) (shield-dmg 5) (power 5) (range 2) (range-penalty -20)))) 
  ;particle-beam
  (damage-inc 0)
  ;ion-cannon
  (charging nil)
  (charge 0)
  (init 0) 
  (weapons-enabled 6) ;this will work when weapons location is destroyed, the enabled systems will decrease. 
  (computer t) ;have this be required t to be able to hack. 
  (access nil)
  (hacked nil)
  (scrambled nil)
  (power-hacked 0) ;set this to a number of rounds that will decrement in the apply-hacks function. 
  (shield-hacked 0)
;  (drones '((drone1 nil) (drone2 nil)))
  (bow 20) ;put something like (weapons . 20) here. 
;  (lowbow nil)
  (port 20)
  (starboard 20)
  (stern 20)
;  (weapons-location nil) ;put here something like (bow . 20), this shouldn't be shared between manofwars, but be careful. 
  (shield-location nil)
  (engine-location nil)
  (power-location nil)
  (computer-location nil) ;make sure that if this is destroyed all hacks in process stop. 
  ;ADD drones here! I think they should be primarily defensive. I think they might also work as a distraction. so that maybe
  ;the enemy sees two players and might pick the wrong target. I'll need to implement target selection too. 
)

(defun christen (ship)
  (format t "This ship hereby is recognized as the property of Knight-Captain: ")
  (setf (manofwar-captain ship) (read))
  (format t "And shall be known to the world as thus christened by him: ")
  (setf (manofwar-name ship) (read))
  (format t "~%"))

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
    (when (or (> set 100) (> (/ set 100.0) (manofwar-power ship))) ;this doesn't check to make sure you have enough power anymore. To do that I'd have to set the shield and then set-power. 
      ;the problem then is it displays current shields at a really high value, even though you don't have enough power for that. 
      (format t "Error, you do not have enough power.~%")
      (set-shield ship))
    (if (> set (manofwar-max-shield ship))
        (setf (manofwar-shield ship) (manofwar-max-shield ship))
      (setf (manofwar-shield ship) set))))

;12-31 I've reordered this around so hopefully max-shield won't reset after being destroyed by gauss. 
(defun check-shield (ship)
  (when (> (manofwar-shield ship) (manofwar-max-shield ship))
    (setf (manofwar-shield ship) (manofwar-max-shield ship)))
  (when (< (manofwar-shield ship) 0)
    (setf (manofwar-shield ship) 0)
    (setf (manofwar-max-shield ship) 0))
  (when (and (> (manofwar-shield ship) 0) (< (manofwar-shield ship) 100))
    (setf (manofwar-max-shield ship) (manofwar-shield ship)))) ;this will prevent the player from just recharging their shield everytime they take dmg

;  (when (> (manofwar-shield ship) (manofwar-max-shield ship))
;    (setf (manofwar-shield ship) (manofwar-max-shield ship)))) 
;  (set-power ship))

;spd
;change this so it checks speed against assoc 'max-speed rest assoc engine
(defun set-speed (ship)
  (format t "Current speed: ~S " (manofwar-speed ship))
  (format t "Set speed to: ")
  (let ((spd (read)))
    (if (> spd 2)
        (setf spd 2))
    (setf (manofwar-speed ship) spd)
    (set-power ship)))

(defun check-speed (ship)
  (if (< (manofwar-max-speed ship)
         (manofwar-speed ship))
      (setf (manofwar-speed ship) (manofwar-max-speed ship))))

;this should be changed so that you can't enter a wrong course. 
;crs. Change it to put parens around coordinates automatically.
;allow the player to select the enemy's position as a destination. maybe allow them to vollow the enemy and so course keeps updating. 
(defun set-course (ship)
  (format t "Current course is set to ~S." (manofwar-course ship))
  (format t "Change course to: ")
  (let ((crs (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (setf (manofwar-course ship) crs)))

;COMMANDS weapon systems.  
(defun weapons (ship target)
  (format t "Target distance: ~S~%" (ship-distance ship target))
  (format t "~%Select which weapon to fire: ~%[1]nuke [2]missiles [3]particle-beam~%
[4]gauss [5]ion-cannon [6]cannons~%")
  (if (manofwar-hacked ship)
      (setf wpn (+ 1 (random 5)))
    (setf wpn (read)))
;  (let ((wpn (read)))
    (cond ((and (equal wpn 1) (check-power ship 'nuke)) (fire-weapon ship 'nuke target 0))
          ((and (check-power ship 'missiles) (equal wpn 2)) ;doesn't drain power correctly, but I might just not worry about this. 
           (format t "Firing missiles 1 through 12...") (setf number (read)) (dotimes (i 12) (fire-weapon ship 'missiles target 0)))
          ;Here's a problem: it checks power before getting the pwr increase.need to set damage-inc of particle beam so it works with cost. 
          ((and (check-power ship 'particle-beam) (equal wpn 3)) (format t "Enter power-increase: ") ;change this so you enter power you want to spe;nd. This would get rid of the unecessary variable in particle-weapon which is making this quite ugly. 
           (setf (manofwar-damage-inc ship) (read)) ;sets damage-inc
           (particle-beam-cost ship)
           (setf mod (manofwar-damage-inc ship));sets mod to damage-inc
           (fire-weapon ship 'particle-beam target mod))
          ;set up gauss to target a specific location on the ship to disable the system. 
          ((and (check-power ship 'gauss) (equal wpn 4)) (target-location ship target)) ;(fire-weapon ship 'gauss target 0))
          ((and (check-power ship 'ion-cannon) (equal wpn 5)) (format t "Enter how long to charge: ")
           (setf (manofwar-charging ship) t)
           (setf charge (read)) (charge-ion ship target *round* charge))
          ((and (check-power ship 'cannons) (equal wpn 6)) (fire-weapon ship 'cannons target 0))
          (t (format t "Error, not enough power or invalid entry. Enter selection again.~%") (weapons ship target))))


;start will be the current round, fire will be that round + 1 through 5. This is something that'll have to be called every round update. 
;HOW am I going to get init and fire so that I can continue to call this in the main method. I could always make the cost immediate, but I need this to fire it.  
;New error, it's subtracting 15 at the outset, and never again. And I think that it takes 10 away at each charge instead of 5.  

(defun charge-ion (ship target init fire) 
  (setf (manofwar-init ship) init) 
  (if (and (< *round* (+ init fire)) (not (zerop fire)) (not (zerop init)))
      (progn
        (setf (manofwar-charge ship) fire)
        (ion-cannon-drain ship))
    ;this should mean that they're equal. can't think of another circumstance
    (if (and (not (zerop fire)) (not (zerop init)))
        (progn 
          (fire-ion ship target))))) 
;          (setf (second (assoc 'charge (cdr (assoc 'ion-cannon (manofwar-weapons ship))))) 0))))) 
   
(defun update-ion (ship target)
  (if (manofwar-charging ship)
      (charge-ion ship target (manofwar-init ship) 
                  (manofwar-charge ship))))

(defun ion? (ship target)
  (if (manofwar-charging ship)
      (update-ion ship target))) 

;GACK. This is a little imbalanced right now, the target's power will never return.
(defun fire-ion (ship target)
  (if (> (manofwar-shield ship) 0) 
      (take-damage target (+ (second (assoc 'shield-damage (cdr (assoc 'ion-cannon (manofwar-weapons ship)))))
                           (* 5 (manofwar-charge ship))))) 
    (progn
      ;should this maybe use drain instead of max-power? 
      (setf (manofwar-max-power target) (- 100  (+ (second (assoc 'power-damage (cdr (assoc 'ion-cannon (manofwar-weapons ship)))))
                           (* 5 (manofwar-charge ship))))) 
      (setf (manofwar-drain ship) (- (manofwar-drain ship)  (* 5 (manofwar-charge ship)))) 
      (setf (manofwar-charge ship) 0)
      (setf (manofwar-charging ship) nil) 
      (setf (manofwar-init ship) 0))) 

;This won't quite work because drain won't be able to reset once its fired. Also, init needs to go up each time. 
;this seems to only be happening once. 
(defun ion-cannon-drain (ship) 
;  (format t "~S drain at: ~S~%" (manofwar-name ship) (manofwar-drain ship)) 
  (if (> (manofwar-charge ship) 0)
      (setf (manofwar-drain ship) (+ (manofwar-drain ship) 5)))
;  (format t "~S drain at: ~S~%" (manofwar-name ship) (manofwar-drain ship))
) 
;      (setf (second (assoc 'charge (cdr (assoc 'ion-cannon (manofwar-weapons ship))))) 0)


(defun target-location (ship target)
  (format t "Select target: bow[1] port[2] starboard[3] stern[4]~%") 
  (let ((pick (read)))
    (cond ((equal pick 1) (fire-gauss ship target 'bow)) 
;          ((equal pick 2) (fire-gauss ship target (manofwar-lowbow target)))
          ((equal pick 2) (fire-gauss ship target 'port))
          ((equal pick 3) (fire-gauss ship target 'starboard))
          ((equal pick 4) (fire-gauss ship target 'stern))
          (t (format t "Error, unspecificied target.~%") (target-location ship target)))))


;reduce the damage by a certain amount if they have shields up. this might not be necessary, could just use damage-area in addition to
;fire-weapon, because I want it to deal damage to the system and to the overall hull. 
;make sure to use this when selecting weapons. 
(defun fire-gauss (ship target area) 
  ;make sure that this damages the hull as well as the targeted system. 
  (let ((mod (* (- (ship-distance ship target) (second (assoc 'range (cdr (assoc 'gauss (manofwar-weapons ship))))))
                (second (assoc 'range-penalty (cdr (assoc 'gauss (manofwar-weapons ship))))))))
    (damage-area target area (damage-penalty (second (assoc 'target-dmg (cdr (assoc 'gauss (manofwar-weapons ship))))) mod)) ;there is no mod for gauss
    ;just like in fire-weapon I need to decide if it does hull-dmg or shld-dmg. actually, I don't want to, I'd rather it pierce through shield
    (take-damage target (damage-penalty (second (assoc 'hull-dmg (cdr (assoc 'gauss (manofwar-weapons ship))))) mod)) ))
 

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

;this is for the gauss cannon. REWRITE THIS, changed the structure. WARNING! damages both ships??? 12-31!!!
(defun damage-area (ship area damage) ;area is going to be a quoted symbol. 
  (when (> (manofwar-shield ship) 0)
      (setf damage (- damage 10))) ;this is hardcoded in and gross, maybe I should write a penalty for gauss cannon. 
  (cond ((equal area 'bow)  (setf (manofwar-bow ship) (- (manofwar-bow ship) damage)) 
         (area-destroyed ship 'bow (manofwar-bow ship)))
        ((equal area 'port) (setf (manofwar-port ship) (- (manofwar-port ship) damage)) 
         (area-destroyed ship 'port (manofwar-port ship)))
        ((equal area 'starboard) (setf (manofwar-starboard ship) (- (manofwar-starboard ship) damage)) 
         (area-destroyed ship 'starboard (manofwar-starboard ship)))
        ((equal area 'stern) (setf (manofwar-stern ship) (- (manofwar-stern ship) damage))
        (area-destroyed ship 'stern (manofwar-stern ship)))))
;  (area-destroyed ship area)) ;is this necessary? I think i just need it when the player picks their command. 

;set it up so only random weapons will go down if its a weapons system. power will only drop to 33. 
;Still need to implement for weapons. Don't know how I'll deactivate weapons though. Use activated-weapons in structure, and randomly
;assign them to certain symbols which will be checked for when selecting a weaopn, where it'll display disabled systems. 
;HAVE THIS also print a message saying that an area has been destroyed.
;WARNING! This won't work anymore because it can't take car of area, This needs to check locations. 
(defun area-destroyed (ship area health) 
  (if (<= health 0) 
      (cond ((equal (manofwar-engine-location ship) area) (setf (manofwar-max-speed ship) 0))
            ((equal (manofwar-shield-location ship) area) (setf (manofwar-max-shield ship) 0)) 
            ((equal (manofwar-power-location ship) area) (setf (manofwar-max-power ship) 0)) 
 ;THIS GETS RESET! is that okay? 
            ((equal (manofwar-computer-location ship) area) (setf (manofwar-computer ship) nil))))) 
      


(defun print-destroyed-systems (ship area health) 
  (if (<= health 0)
      (format t "WARNING! ~S has suffered critical damage.~%" area))) ;this doesn't work. Should print out the weapon system located there. 

;bad name? 
(defun systems-status (ship)
  (print-destroyed-systems ship 'bow (manofwar-bow ship))
  (print-destroyed-systems ship 'stern (manofwar-stern ship))
  (print-destroyed-systems ship 'starboard (manofwar-starboard ship))
  (print-destroyed-systems ship 'port (manofwar-port ship))) 



;WEAPONS.
(defun accuracy-roll (modifier)
  (+ modifier 75 (random 25))) ;the + 25 ensures damage will never be less than 25 %

;returns the damge dealt. 
(defun damage-penalty (damage mod)
  (let ((roll (accuracy-roll mod)))
    (if (> roll 0) ;if the modifier is < -100 then the roll will return negative damage, and thus boost health
        ;so this has it return 0 if the weapon is too far out of range. 
        (round (* damage (/ roll 100.0)))
      0)))

;write a separate fire weapon for ion-cannon. 

;need to adapt this to work with ion-cannon and particle beam? YES
(defun fire-weapon (ship weapon target dmg-inc)
  (let* ((wpn (rest (assoc weapon (manofwar-weapons ship1))))
         (dist (ship-distance ship target))
         (mod (* (- dist (second (assoc 'range wpn))) (second (assoc 'range-penalty wpn))))) ;does this cause extra damage?
    (if (< (manofwar-shield target) 0)
      (let ((damage (+ dmg-inc (second (assoc 'hull-dmg wpn)))))
        (take-damage target (damage-penalty damage mod)))
      (let ((damage (+ dmg-inc (second (assoc 'shield-dmg wpn)))))
        (take-damage target (damage-penalty damage mod))))))

;returns t if there's enough power to use weapon. assumes that weapon variable is a list (weapon () () ()...)
;include this in the check for wpn with an int. 
(defun check-power (ship wpn)      
  (let ((weapon (assoc wpn (manofwar-weapons ship))))
    (if (> (manofwar-power ship) (second (assoc 'power (rest weapon))))
        t
      nil)))

;change this so that if the damage-inc costs more power than you have, it's reduced until you have enough power. 
(defun particle-beam-cost (ship)
  (let ((power-inc (manofwar-damage-inc ship)))
    (if (> (* 2 power-inc) (manofwar-power ship))
        ;this ensures that if you don't have enough power to increase your damage by that much, then damage-inc is reset to half of power, since
        ;clearly you want to go all out. 
        (setf  (manofwar-damage-inc ship) (/ (manofwar-power ship) 2)) ;is this right? 
    (setf (manofwar-power ship) (- (manofwar-power ship) (* 2 power-inc))))))


;I don't think the wpn-power is necessary, I have a separate thing to check weapon power. Set up a boolean, that if it's true then the player
;is currently hacking, and then reduce power by a certain amount if that's true. 
(defun set-power (ship)
  (setf (manofwar-power ship) (- (- (manofwar-max-power ship) (manofwar-drain ship))  (* 3 (round(/ (manofwar-shield ship) 10.0)))
                                 (* (manofwar-speed ship) 20))))
;                                 wpn-power)))

;msg. change this so that it sends  the message to the enemie's console. 
(defun send-message (recipient)
  (format t "Enter your message: ")
  (read-line)) 


;-----------------------------HACKING----------------------------------
;HACKING!!!! right now it doesn't take any power from you, and you can really wreck the other player. 
;should probably include a roll for this too to see how effective it is? 

;this gives access to the player to hack other systems. Maybe this takes in a ship and target instead, and changes access there, then
;every round it checks (manofwar-access ship1) and if t, then it allows you to pick a hack. If not it says "get-info." One you have info
;it should print it out every round next to your info.
(defun hack-info (ship target)
  (format t "Here is the enemies status: ~%")
  (print-status target t 0)
  (setf (manofwar-access ship) t))

;this will use scrambled status, but it needs to supercede the normal update.
(defun hack-status (target)
  (format t "scrambling enemy status read-out....~%.....................scramble finished")
  (setf (manofwar-scrambled target) t))

;Deactivate a system for two rounds. seems broken if its power, useless if its course, speed, or shield. 
;have it so it'll deactivate a particular weapon, or perhaps a random 3. 
(defun null-weapon (target)
  )

;this will make it so that whatever they enter for their pick, it'll choose something else. Maybe even numbers? 
(defun hack-pick (target)
  (format t "Enemy's next move will be random.~%")
  (setf (manofwar-hacked target) t))
  
;this must be updated to work with the redefined structure. 
(defun hack-location (target)
  (format t "Enter the system you'd like the location of.~% ")
  (format t "[shld] [comp] [eng] [pow]~%") 
  (let ((pick (read)))
    (cond ((equal pick 'wpn) (format t "The weapons system is located at ~S.~%" (manofwar-weapons-location target)))
          ((equal pick 'shld) (format t "The shield generator is located at ~S.~%" (manofwar-shield-location target)))
          ((equal pick 'comp) (format t "The computer system is located at ~S.~%" (manofwar-computer-location target)))
          ((equal pick 'eng) (format t "The engine is located at ~S.~%" (manofwar-engine-location target)))
          ((equal pick 'pow) (format t "The power core is located at ~S.~%" (manofwar-power-location target))))))


;this probably won't work until I have it display on different windows. 
(defun hack-radar (target)
)

;make hacking info a prerequisite of this? seems like hacking info is pretty useless if hacking a system gives you the info. 
;this might not work for player2, because as soon as their round is over, everyone's power resets. 
(defun hack-power (target)
  (format t "Target's current power is at: ~S~%" (manofwar-power target))
  (format t "Power dump initiated, 5% power leaked over 6 ")
  (setf (manofwar-power-hacked target) 6)) 
;  (format t "Power drained by ~S.~%" (round (/ (manofwar-power target) 2.0)))
;  (setf (manofwar-drain target) (round (/ (manofwar-power target) 2.0)))) 
;  (format t "Drain power by how much? ")
;  (setf (manofwar-drain target) (/ (read) 2)))
;  (setf (manofwar-power target) (- (manofwar-power target) (read))))

;need to set this up so it comes back after 1 round. 
(defun hack-speed (target)
  (format t "Target's current speed is ~S" (manofwar-speed target))
  (setf (manofwar-speed target) 0)
  (format t "Target is dead in the water."))

;set this so it drains max-shield by a certain amount each round. 
(defun hack-shield (target)
  (format t "Target's shield is currently at ~S% power.~%" (manofwar-shield target))
  (format t "Shield drain initiilized..... diverting energy away from shield integrity.")
  (setf (manofwar-shield-hacked target) 4)) ;think this is causing problems.  

;  (format t "Shield drained by half energy. Now at: ~S%~%" (round (/ (manofwar-shield target) 2.0)))
;  (setf (manofwar-shield target) (round (/ (manofwar-shield target) 2.0))))
;  (format t "Reduce it by: ")
;  (setf (manofwar-shield target) (- (manofwar-shield target) (read))))

;might want to include something with this where the course is locked in for a few turns. 
(defun hack-course (target)
  (format t "Target's current heading is: ~S~%" (manofwar-course target))
  (format t "Set it to: ")
  (setf (manofwar-course target) (read)))

;create a function that creates a list of systems the player has access to on the enemy ship. 

(defun pick-hack (ship target)
  (if (not (manofwar-access ship))
      (progn
        (format t "Request denied. Would you like to get access: ")
        (read)
        (format t "~%Requesting access to ~S..............~%" (manofwar-name target))
        (format t "..forcing entry...%8.........%39...........%76.....%97~%")
        (format t ".....authentification retrievied......DONE..............Access granted.~%")
        (format t "Welcome lord ~S.~%" (manofwar-captain ship))
        (hack-info ship target) )
    (progn
      (format t "Select system to hack: ") ;print out options? 
      (let ((pick (read)))
        (cond ((equal pick 'power) (hack-power target))
              ((equal pick 'readout) (hack-status target))
              ((equal pick 'speed) (hack-speed target))
              ((equal pick 'shield) (hack-shield target))
              ((equal pick 'course) (hack-course target))
              ((equal pick 'system) (hack-pick target))
              ((equal pick 'location) (hack-location target))
              (t (format t "Error. Uncomputable entry.~%") (pick-hack ship target)))))))

;should it be setup so that these stats revert after the rounds are expired? otherwise players will just run out of juice. 
(defun apply-hacks (ship enemy)
  ;this is to reset power drain when the hack goes away. This will cause an error if there is another drain going on like charging ion. do I need this for shield? 
  ;this should break off hacking if the enemy's computer has been destroyed. 
  (if (not (manofwar-computer enemy))
      (progn
        (setf (manofwar-power-hacked ship) 0)
        (setf (manofwar-shield-hacked ship) 0)))
  (when (zerop (manofwar-power-hacked ship))
    (setf (manofwar-drain ship) 0))
  (when (> (manofwar-power-hacked ship) 0) 
         (setf (manofwar-drain ship) (+ 5 (manofwar-drain ship)))
         (setf (manofwar-power-hacked ship) (1- (manofwar-power-hacked ship))))
  (when (> (manofwar-shield-hacked ship) 0)
         (setf (manofwar-max-shield ship) (- (manofwar-max-shield ship) 15))
         (setf (manofwar-shield-hacked ship) (1- (manofwar-shield-hacked ship)))))

  

;------------------------------NAVIGATION-------------------------------
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
;12-31 Not sure how well this works, could use some tweaking. Seems to say you're 6 away most of the time. 
;if they're top and bottom instead of corners, distance becomes much greater. They can be further away and be 8 away, but
;top and bottom is 10 away even if they're nearly on the same column. 3 . 2 and 12 . 13 records a distance of 2. SO WRONG
;(sqrt (- x2 x1) + (- y2 y1))
(defun ship-distance (ship1 ship2) 
  (let ((dist (calculate-distance (manofwar-location ship1) (manofwar-location ship2))))
    (sqrt (+ (abs (car dist)) (abs (cdr dist))))))

;    (if (equal (abs (car dist)) (abs (cdr dist)))
 ;       (abs (car dist))
  ;    (abs (- (abs (car dist)) (abs (cdr dist))))) ))


;include a conditional here to check to see if distance is greater than the speed, and if it is move only by speed, if its less than then move by distance. problem is a negative distance will always be less than, so check for that
;YUCK. change to cond, and maybe set variables so I have less typing to do. 
(defun move (ship)
  (let ((dist (calculate-distance (manofwar-location ship) (manofwar-course ship))))
;    (print dist)
;    (print (manofwar-location ship))
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




;USER INTERFACE, crudimentry. 
;keep asking for input until they enter a valid shortcut for all functions. 

;here, write a function prompting the user for a command, display how many commands left in chain. 

;print these out really nicely formated. 
(defun print-status (ship stream depth) ;ignore depth, but its required for some reason. 
  (format stream "Round: ~S~%" *round*)
  (format stream "Starship ~20S Captain: ~S~%" (manofwar-name ship) (manofwar-captain ship))
  (systems-status ship)
  (format stream "NAVIGATION ~%")
  (format stream "~10S: ~S~%" 'Location (manofwar-location ship)) 
  (format stream "Course: ~S~%" (manofwar-course ship))
  (format stream "Speed: ~S~%" (manofwar-speed ship))
  (format stream "POWER MANAGEMENT~%")
  (format stream "Power: ~S~%" (manofwar-power ship))
  (format stream "Power-drain: ~S~%" (manofwar-drain ship))
  (format stream "Shield ~S~%" (manofwar-shield ship))
  (format stream "Hull ~S~%" (manofwar-hull ship)))

;set it up to randomly rearrange letters also. how to set it up so it'll print this instead? include a conditional in to see if a certain
;function is t, if it is and the status has been hacked, then tell show-round to print this instead of the other. 
(defun scrambled-status (ship)
  (format stream "Round: ~S~%" *round*)
  (format t "Starship ~S~%" (random 1000000000000000))
  (systems-status ship)
  (format t "NAVIGATION ~%")
  (format t "Location: ~S~%" (random 1000000000000000))
  (format t "Course: ~S~%" (random 1000000000000000))
  (format t "Speed: ~S~%" (random 1000000000000000))
  (format t "POWER MANAGEMENT~%")
  (format t "Power ~S~%" (random 1000000000000000))
  (format t "Power drain: ~S~%" (random 1000000000000000)) 
  (format t "Shield ~S~%" (random 1000000000000000))
  (format t "Hull ~S~%" (random 1000000000000000)))

(defun which-status (ship)
  (if (manofwar-scrambled ship)
      (scrambled-status ship)
    (print-status ship t 0)))

;GAME GAME GAME GAME GAME GAME
(defun start-game()
  (setf map (make-map))
  (set-map map)
  (setf locations (random-starting-locations))
  (setf ship1 (make-manofwar :name 'player1
                             :location (first locations) ;think of a way to randomize this, but make it correspond with the enemy so they aren't too close. 
                             :course (first locations) ;this is to make sure course is never nil in game run.
                             :speed 2
                             :hull 100
                             :shield 0)) 
  (default-system-locations ship1)
  (setf ship2 (make-manofwar :name 'player2
                             :location (second locations)
                             :course (second locations)
                             :speed 2
                             :hull 100
                             :shield 0)) 
  (default-system-locations ship2)
  (display-map map ship1 ship2)
  (show-round map ship1 ship2 0)) 
 

(defun random-starting-locations ()
  (setf lst nil)
  (if (equal (random 2) 0)
      (setf lst (cons (cons 0 (1+ (random 6))) lst))
    (setf lst (cons (cons (1+ (random 6)) 0) lst)))
  (setf lst (cons (cons (- 15 (car (car lst))) (- 15 (cdr (car lst)))) lst))
  (reverse lst))

;this lets you each customize your ship. 
(defun custom-game ()
  (setf map (make-map))
  (set-map map)
  (setf ship1 (make-manofwar :location '(0 . 0)
                             :name 'ship1
                             :course '(15 . 15) ;this is to make sure course is never nil in game run. 
                             :speed 2
                             :hull 100
                             :shield 0
                             :max-shield 100)) 
  (setf ship2 (make-manofwar :location '(15 . 15)
                             :name ship2
                             :course '(0 . 0)
                             :speed 2
                             :hull 100
                             :shield 0
                             :max-shield 100)) 
  (christen ship1)
  (christen ship2)
  ;let the player assign where each of their systems is.  
) 
 

(defun set-system-locations (ship)
  (format t "Select where to place each system.~%")
  (format t "[bow] [port] [starboard] [stern]~%")
  (format t "Install computer at: ")
  (setf (manofwar-computer-location ship) (read))
  (format t "~%Install the engine at: ")
  (setf (manofwar-engine-location ship) (read))
  (format t "~%Install power unit at: ")
  (setf (manofwar-power-location ship) (read))
  (format t "~%Install shield generator at: ")
  (setf (manofwar-shield-location ship) (read))) 


;rewrite this so it creates health for these. REWRITE THIS TO ALTER LOCATIONS
(defun default-system-locations (ship)
  (setf (manofwar-computer-location ship) 'bow)
  (setf (manofwar-power-location ship) 'starboard)
  (setf (manofwar-engine-location ship) 'stern)
  (setf (manofwar-shield-location ship) 'port)) 


;unfinished. REWRITE. make a list of unassigned systems. 
(defun random-system-locations (ship)
  (let ((health 20)
        (rand (random 4)))
    (cond ((equal rand 0) (setf (manofwar-bow ship) `(computer . ,health)))
          ((equal rand 1) (setf (manofwar-bow ship) `(engine . ,health)))
          ((equal rand 2) (setf (manofwar-bow ship) `(power . ,health)))
          ((equal rand 3) (setf (manofwar-bow ship) `(shield . ,health))))    
))

(defun choose-game ()
  (backstory)
  (format t "~%Would you like to customize your ship before departing? (y / n)" )
  (let ((choice (read)))
    (if (equal choice 'n) 
        (start-game)
      (custom-game))))
  
;ROUND UPDATES!
(defun move-ships (ship1 ship2)
  (when (not (equal (manofwar-location ship1)
                    (manofwar-course ship1))) 
    (move ship1))
  (when (not (equal (manofwar-location ship2)
                    (manofwar-course ship2)))
    (move ship2))) 

;maybe write these as smaller functions. 
(defun show-round(map ship1 ship2 count)
  (setf *round* (1+ *round*))
  (set-power ship1)
  (set-power ship2)
  (format t "Round Count: ~S~%" *round*) 
  (move-ships ship1 ship2)
  (display-map map ship1 ship2)
  (format t "~%=====================================================~%")
  (apply-hacks ship1 ship2) ;MAKE sure this works.  
  (update-ion ship1 ship2)
  (set-power ship1)
  (check-shield ship1) ;this is what's keeping gauss from permanently knocking out the shields. 
  (check-speed ship1)
  (if (manofwar-hacked ship1)
      (progn
       (setf (manofwar-hacked ship1) nil) ;this resets so that their next choice won't be random. 
       (random-command ship1 ship2))
    (get-command ship1 ship2))
;  (setf (manofwar-drain ship1) 0) ;Will something bad happen when this is removed? 
  (set-power ship1)
  (setf (manofwar-max-power ship1) 100) ;this is to ensure that ion-cannon damage doesn't carry over into other rounds. 
  (when (destruction ship2) 
    (format t "~S was destroyed.GAME OVER. The ~S drifts victorious!" (manofwar-name ship2) (manofwar-name ship1))
    (break));PROBLEM this won't actually end the game if second player dies though.  
  (format t "~%=====================================================~%")
  (apply-hacks ship2 ship1) 
  (update-ion ship2 ship1)
  (set-power ship2)
  (check-shield ship2)
  (check-speed ship2)
  (if (manofwar-hacked ship2)
      (progn
       (setf (manofwar-hacked ship2) nil)
       (random-command ship2 ship1))
    (get-command ship2 ship1))
;  (setf (manofwar-drain ship2) 0)
  (set-power ship2)
  (setf (manofwar-max-power ship2) 100)
  (cond ((destruction ship1) (format t "~S was destroyed. GAME OVER. The ~S floats victorious!" (manofwar-name ship1) (manofwar-name ship2)))
        ((destruction ship2) (format t "~S was destroyed." (manofwar-name ship2)))
        (t (show-round map ship1 ship2 (+ 1 count))))) 

(defun resume-game (map ship1 ship2 count)
  (show-round map ship1 ship2 count))
             

;this is where it'll check system health, just like it checks weapon power-cost. 
(defun get-command (ship target)
  (which-status ship)
  (format t "[spd] [crs] [shld] [wpn] [hck]~%")
  (format t "Enter your command: ")
  (let ((cmd (read)))
    (cond ((equal cmd 'spd) (set-speed ship))
          ((equal cmd 'crs) (set-course ship))
          ((equal cmd 'shld) (set-shield ship))
          ((equal cmd 'wpn) (weapons ship target))
          ((equal cmd 'hck) (pick-hack ship target))
          (t (format t "Error, reenter command.~%") (get-command ship target))))) 

(defun random-command (ship target)
  (which-status ship)
  (format t "[spd] [crs] [shld] [wpn] [hck]~%")
  (format t "Enter your command: ")
  (setf choice (read))
  (format t "~%Error, wrong!~%") 
  (let ((pick (random 5)))
;error, because if pick is same as choice, then it won't do anything, and so the player won't even get a random action. I'm not sure if this is
;unbalanced or not. I'm not sure it is, the hacker essentially passes, there's a chance the enemy will pass too, but a higher chance they'll
;just do something other than they intended. This 
   (cond ((and (equal pick 0) (not (equal choice 'spd))) (set-speed ship))
          ((and (equal pick 1) (not (equal choice 'crs))) (set-course ship))
          ((and (equal pick 2) (not (equal choice 'shld))) (set-shield ship))
          ((and (equal pick 3) (not (equal choice 'wpn))) (weapons ship target))
          ((and (equal pick 4) (not (equal choice 'hck))) (pick-hack ship target))))) 

;use sleep function to improve the flow of game, give player time to review a hack, etcetera. 

