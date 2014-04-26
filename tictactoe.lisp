;tic tac toe
;0 means empty, 1 is an O, 10 is an x. 
(defun make-board()
  (list 'board 0 0 0 0 0 0 0 0 0) )

(defun convert-to-letter (v)
  (cond ((equal v 1) "O")
        ((equal v 10) "X")
        (t " ")) )

(defun print-row (x y z)
  (format t "~&    ~A | ~A | ~A"
          (convert-to-letter x)
          (convert-to-letter y)
          (convert-to-letter z)) )

(defun print-board (board)
  (format t "~%")
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~&   -----------")
  (print-row
   (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~&   -----------")
  (print-row
   (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%") )

;SET VALUES FOR PLAYERS
(setf *comp* 10)
(setf *user* 1)

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

;Winning moves!
(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9)
        (1 4 7) (2 5 8) (3 6 9)
        (1 5 9) (3 5 7)) )

;sum triplets, this will tell you how many pieces are on a line. 30 or 3 will mean a win. 
;if it isn't either of these, 12 means there's two Os and one X, 11 is 1 and 1, 21 is two O and one X
(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)) )

;to fully analyze a board you have to look at all sums. This will take in the board, and mapcar over each
;list in triplets, calling sum-triplet on it to see if its a victory, and possibly how far from victory it is. 
(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
              (sum-triplet board triplet))
          *triplets*))

(defun winnerp (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *comp*) sums)
        (member (* 3 *user*) sums)) ))

(defun play-on-game ()
  (if (y-or-n-p "Would you like to place first?")
      (user-move (make-board))
    (comp-move (make-board))) )

(defun user-move (board) ;let* allows multiple declarations, and for those variables to be used in later declarations. 
  (let* ((pos (read-legal-move board))
         (new-board (make-move
                     *user*
                     pos
                     board))) ;weird way to bracket it? 
    (print-board new-board)
    (cond ((winnerp new-board)
           (format t "~&You win!"))
          ((board-fullp new-board)
           (format t "~&Tie game."))
          (t (comp-move new-board))) ) )

;a legal move is an int between 1 and 9 such that the corresponding board is empty. 
(defun read-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read))) ;gets move
    (cond ((not (and (integerp pos) ;1st condition, not(pos is int and is between 1 and 9) so if either are true then tell the user its invalid, and ask again by recalling the function. Remember, cond is triggered if true
                     (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-legal-move board))
          ((not (zerop (nth pos board))) ;checks to make sure move entred isn't occupied by seeing if that position is a zero, if it is then this condition isn't triggered, if it isn't then it calls the function again. 
           (format t "~&That space is occupied.")
           (read-legal-move board))
          (t pos))) )

(defun board-fullp (board)
  (not (member 0 board)) )

(defun comp-move (board)
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move *comp* pos board)))
         (format t "~&My move: ~S" pos)
         (format t "~&My strategy: ~A~%" strategy)
         (print-board new-board)
         (cond ((winnerp new-board)
                (format t "~&I win!"))
               ((board-fullp new-board)
                (format t "~&Tie game."))
               (t (user-move new-board)))))

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-user board)
      (random-move-strategy board)))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
        "random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
        pos
         (pick-random-empty-position board))))

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board (* 2 *comp*))))
    (and pos (list pos "make three in a row"))))

(defun block-user (board)
  (let ((pos (win-or-block board (* 2 *user*))))
    (and pos (list pos "block opponent"))) )

(defun win-or-block (board target-sum)
  (let ((triplet (find-if #'(lambda (trip)
                              (equal (sum-triplet board trip)
                                     target-sum))
                          *triplets*)))
    (when triplet
      (find-empty-position board triplet))) )

(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
               (zerop (nth pos board)))
        squares))

