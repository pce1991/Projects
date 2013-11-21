;DNA

(defun complement-base (base)
  (cond ((equal base 'a) 't)
        ((equal base 't) 'a)
        ((equal base 'g) 'c)
        ((equal base 'c) 'g)
        (t (format t "error, ~S is not a base." base))))


(defun complement-strand (strand)
  (let ((complement nil))
    (dolist (b strand (reverse complement)) ;this is saying go through each element in strand, when you reach the end
    ;return complement. 
      (push (complement-base b) complement))))

(defun make-double (strand)
  (let ((double nil))
    (dolist (b strand (reverse double))
      (push (list b (complement-base b)) double))))

;keep in mind that strand could be doubles (a t) (g c). this doesn't work for that at all. so gacky! GACK
(defun count-bases (strand)
  (let ((count (list (list 'a 0)
                     (list 't 0)
                     (list 'g 0)
                     (list 'c 0))))
    (dolist (pair strand count)
      (if (consp pair)
          (dolist (p pair)
            (cond ((equal p 'a) (incf (second (first count))) (incf (second (second count))))
                  ((equal p 't) (incf (second (second count))) (incf (second (first count))))
                  ((equal p 'g) (incf (second (third count))) (incf (second (fourth count))))
                  ((equal p 'c) (incf (second (fourth count))) (incf (second (third count)))))))
      (cond ((equal pair 'a) (incf (second (first count))))
            ((equal pair 't) (incf (second (second count))))
            ((equal pair 'g) (incf (second (third count))))
            ((equal pair 'c) (incf (second (fourth count))))))))


;it will only be a prefix if first of first is equal to first of second, and so on. nested dolist?
;use return 
(defun prefixp (pre post)
  (do ((base1 pre (rest base1))
       (base2 post (rest base2)))
      ((not (equal (first base1) (first base2))) nil)
    (if (equal (rest base1) nil)
        (return t))))

;so this will have to have several matches in a row, there can be a wrong match, but once there's a right one
;there can't be any wrong ones. 
(defun appearsp (segment strand)
  (do ((base1 segment)
       (base2 strand (rest base2)))
      ((equal (first base1) nil) t)
    (if (equal (first base1) (first base2))
        (setf base1 (rest base1)))
    (if (equal base2 nil)
        (return nil))))

;checks to see if a strand is composed entirely of repetitions of the segment. 
;Uh oh, this is wrong, returns t for '(a) '(a g c)
(defun coverp (segment strand)
  (do ((base1 segment (rest base1))
       (base2 strand (rest base2)))
      ((equal (first base2) nil) t)
    (if (equal (first base1) nil)
        (setf base1 segment))
    (if (not (equal (first base1) (first base2)))
        (return nil))))

(defun get-prefix (n strand)
  (do ((count 0 (+ 1 count))
       (str strand (rest str))
       (seg nil (cons (first str) seg)))
       ((equal count n) (reverse seg))))

;look at prefixes of increasing length until one is found that can cover the strand. 
(defun kernel (strand)
  (do ((count 1 (+ 1 count)))
      ((coverp (get-prefix count strand) strand) (get-prefix count strand))))

;draw DNA. GACKY PRINT JOB
(defun draw-dna (strand)
  (format t "---------------------------~%")
  (dolist (b strand)
    (format t " ! "))
  (format t "~%")
  (dolist (b strand)
    (format t " ~S " b))
  (format t "~%")
  (dolist (b strand)
    (format t " . "))
  (format t "~%")
  (dolist (b strand)
    (format t " . "))
  (format t "~%")
  (dolist (b (complement-strand strand))
    (format t " ~S " b))
  (format t "~%")
  (dolist (b strand)
    (format t " ! "))
  (format t "~%")
  (format t "---------------------------"))
