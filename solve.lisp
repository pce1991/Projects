(defun op (lst)
  (first lst))

(defun lhs(lst)
  (second lst))

(defun rhs(lst)
  (third lst))

(defun solve (e v)
  (if (equal (lhs e) v)
      e
    (if (equal (rhs e) v)
        (list (op e) (rhs e) (lhs e))
      (if (and (not (equal (rhs e) v))
               (not (consp (rhs e))))
          nil))))
  ;now here is where I write a pattern matcher. 
  ;so this is what I do, return call on solve with a substitution in e
  ;but how do I do the substituion match. 

;;do I need a readlist for this or is that just java? 
(setq algpats (list '( (= ?x (+ ?y ?z)) (= (- ?x ?y) ?z))))