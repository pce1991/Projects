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
          nil))) )

(setq algpats 2)

