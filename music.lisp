;music.lisp
(defparameter *note-values* '((c . 1) (c# . 2) (d . 3) (d# . 4) (e . 5) (f . 6) (f# . 7) (g . 8)
                              (g# . 9) (a . 10) (a# . 11) (b . 12))) 

(defparameter *MajKeys* '((C D E F G A B)
                          (C# D# F F# G# A# C)
                          (D E F# G A B C#)
                          (D# F G G# A# C D)
                          (E F# G# A B C# D#)
                          (F G A A# B C D E)
                          (F# G# A# B C# D# F)
                          (G A B C D E F#)
                          (G# A# C C# D# F G)
                          (A B C# D E F# G#)
                          (A# C D D# F G A)
                          (B C# D# E F# G# A#))) 


(defun convert-to-number (note)
 (cdr (assoc note *note-values*))) 

(defun convert-to-note (value)
  (car (rassoc value *note-values*)))

(defun increment (melody n)
  (if (> n 12)
      (format t "Error, enter a nummber of steps < 11")
    (mapcar #'convert-to-note
            (mapcar #'(lambda (value)
                        (if (> (+ value n) 12)
                            (- value (- value n))
                          (+ value n)))
                    (mapcar #'convert-to-number melody)))))

(defun get-notes (melody)
  (get-notesb melody nil))

(defun get-notesb (melody lst)
  (if (equal melody nil)
      lst
    (if (equal (member (car melody) lst) nil)
        (get-notesb (cdr melody) (cons (car melody) lst))
      (get-notesb (cdr melody) lst))))  

;doesn't work
(defun get-key (melody)
  ;checks major keys
  (do ((keys *MajKeys* (cdr keys)))
      ((equal keys nil) nil)
    (let ((count 0))
      (dolist (i (car keys))
        (print i)
        (if (not (equal nil (member i melody)))
            (setf count (1+ count))))
      (if (equal count 7)
          (return (car (car keys)))))))
      
(defun make-bindings (melody-key transposition-key)
  (mapcar #'cons (assoc melody-key *MajKeys*) (assoc transposition-key *MajKeys*)))

(defun make-minor-bindings (key)
  (mapcar #'cons (assoc key *MajKeys*) (mapcar #'convert-to-note (make-natural-minor 
                                               (mapcar #'convert-to-number (assoc key *MajKeys*)))))) 

(defun make-natural-minor (value-key)
  (let ((third (1- (nth 2 value-key)))
        (sixth (1- (nth 5 value-key)))
        (seventh (1- (nth 6 value-key))))
    (cond ((zerop third) (setf third 12))
          ((zerop sixth) (setf sixth 12))
          ((zerop seventh) (setf seventh 12)))
    (setf (nth 2 value-key) third)
    (setf (nth 5 value-key) sixth)
    (setf (nth 6 value-key) seventh))
  value-key) 


;harmonic


(defun transpose (melody)
  
  )
