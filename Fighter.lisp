(defstruct player
           name
           ;ATTributes. How would I put these in a list (attributes (...)) and access them? 
           strength 
              dexterity 
              cunning
              ;STATS
              health 
              stamina 
              will)

(defparameter *points* 20)

(defparameter *player* (make-player :name nil
                                    :strength 5
                                    :dexterity 5
                                    :cunning 5
                                    :health 10
                                    :stamina 10
                                    :will 10))

(defparameter *player-stance* '((defensive aggressive evasive reactive))) ;reactive means to counter an attack

(defparameter *inventory* nil) 

;this will create the character, giving a name to display and setting attributes and stats(setf *player* )
(defun create-player()
  (princ "Please enter your name: ")
  (let ((name (read)))
    (setf (player-name *player*) name))
  (princ "Now distribute your ")
  (princ *points*)
  (princ " points across your attributes.")
  (loop while (> *points* 0) do
        (spend-points)) )

(defun spend-points()
  (princ "Select which attribute or stat: ")
  (princ "strength [s] dexterity[d] cunning [c]")
  (princ "health [h] stamina [st] will [w]")
  (case (read) 
    (s ;strength
     (princ "how many points? ")
     (let ((x (read)))
       (setf (player-strength *player*) (+ x (player-strength *player*)))
       (setf *points* (- *points* x))))
    (d
     (princ "how many points? ")
          (let ((x (read)))
       (setf (player-dexterity *player*) (+ x (player-dexterity *player*)))
       (setf *points* (- *points* x))))
    (c
     (princ "how many points? ")
     (let ((x (read)))
       (setf (player-cunning *player*) (+ x (player-cunning *player*)))
       (setf *points* (- *points* x))))
    (h
     (princ "how many points? ")
     (let ((x (read)))
       (setf (player-health *player*) (+ x (player-health *player*)))
       (setf *points* (- *points* x))))
    (st
     (princ "how many points? ")
     (let ((x (read)))
       (setf (player-stamina *player*) (+ x (player-stamina *player*)))
       (setf *points* (- *points* x))))
    (w
     (princ "how many points? ")
     (let ((x (read)))
       (setf (player-will *player*) (+ x (player-will *player*)))
       (setf *points* (- *points* x))))
    (otherwise (princ "That is not a valid stat symbol, please enter again: ")
               (spend-points)) ))



