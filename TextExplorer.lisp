;GAME ENGINE!
;Maybe I should put this stuff in a file to read from it, that way this program would work with any custom
;map and paths and objects. 

;;I should maybe create a higher level, so you have something like a house, and then sub-parts of the house
;;same as you'd have the outside, and sub-outsides like a yard; this way you could have paths within certain
;;subareas, but also have paths for wide areas, like if you're in the back yard you would have to follow a path
;;to get to campus, but since you're outside it recognizes you can go anywhere, this way you don't have to
;;include every place when describing the locations for a subarea.
(defparameter *map* '((living-room (you are in the living-room. Deb on the couch.))
                      (bedroom (you are in the bedroom. Cob on the bed.))
                      (back-yard (you are in the back-yard. Finn on the porch.))
                      (front-yard (you are in the front-yard. Finn is barking at the dogs across the street. volleys of sound.))
                      ))

;;Maybe this should include directions for each thing, which will fit his design and help the player 
;;or me when designing a map. 
;;first has to be where it leads to am I right? can't be something like backdoor?
(defparameter *paths* '((living-room (bedroom hallway  door) (back-yard west door) (front-yard east door))
                        (back-yard (living-room east door) (front-yard south gate) (front-yard east gate))
                        (front-yard (living-room front door) (back-yard west gate) (back-yard south driveway))
                        (bedroom (living-room door door))
                        ))

;I kind of want to merge these two 'cause keeping track of all objects might not be easy once it exceeds
;a certain point. 
(defparameter *objects* '(game-console book ball sprinkler))

(defparameter *object-locations* '((game-console living-room) 
                                   (book bedroom) 
                                   (ball back-yard) 
                                   (sprinkler front-yard)) )

;;PLAYER PAREMETERS
(defparameter *location* 'living-room)

(defparameter *commands* '(look walk pickup inventory))

;;DESCRIPTION CODE
(defun describe-location (location map)
  (second (assoc location map)) )

(defun describe-path (path)
  `(there is a ,(third path) going ,(first path) from here.))

(defun describe-paths (location paths)
  (apply #'append (mapcar #'describe-path (rest (assoc location paths)))) )

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc))) ) )

(defun getRoom ()
  (describe-location *location* *map*))

;PLAYER FUNCTIONS! 
(defun look ()
  (append (describe-location *location* *map*)
          (describe-paths *location* *paths*)
          (describe-objects *location* *objects* *object-locations*)) )

(defun walk (direction)
  (let ((next (find direction
                    (rest (assoc *location* *paths*))
                    :key #'second)))
    (if next (progn (setf *location* (first next))
               (look))
      '(you cannot go that way..))) )

;Will this let the player pick the item up and carry it with them. I need to create an inventory system. 
;something that is picked up can be dropped, used, or inventoried. 
;;ins't working for some reason. 
(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
               `(you are now carring the ,object))
         (t '(you cannot get that.)) ) )

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)) )


;COMMUNICATION
(defun say-hello()
  (print "Please type your name:")
  (let ((name (read)))
    (format t "nice to meet you, ~S." name ) ) )

(defun game-repl()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")" ))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))


(defun game-eval(sexp)
  (if (member (car sexp) *commands*)
      (eval sexp)
    '(you cannot find it within yourself. Perhaps something more mundane is musterable with your weak and wretched will.)) )

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

(defun game-print(lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))

                           


