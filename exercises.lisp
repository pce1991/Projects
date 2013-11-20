;(list x) will create a list. 

(defun compare (x y)
  (cond ((equal x y) 'numbers 'are 'the 'same) ;prints the last quoted item
        ((< x y) "first is smaller") ;prints the string in quotes
        ((> x y) '(first is bigger)) 
        (t "these can't be compared") ) ) ;returns this list. 

;rewrite with pattern matching
(defun emphasize (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
        ((equal (first x) 'bad) (cons 'awful (rest x)))
        (t (cons 'very x))) )

;and will go through the list and return nil and quit the first instance it finds.
;otherwise it will return the last item. 
(and 1 2 3 4 5) ;return 5. 

;or will go through and return the first one that isn't nil. 
(or nil t t) ;return t.
(or 'george 'fred) ; return george.

;Thse are condiotinals and not functions because they do not have to evaluate every clause. 

;the p indicates its a predicate. 
(defun posnump (x)
  (and (numberp x) (plusp x)) ) ;its important to check numberp first because non-numbers will cause an error
;in plusp. 

;(defun bool-and (x y) (and x y t))

(defun bool-and (x y)
  (if x
      (if y
          t)) )

(defun bool-or (x y) 
  (if (or x y)
      t
    nil))

(defun demorgand-bool (x y)
  (if (not x)
      nil
    (if (not y)
        nil
      t)))

(defun demorgor-bool (x y)
  (if (not x) ;if this is t
      (if (not y) ;and this is t
          nil ;return nil
        t);if x is t and y is not t then return t
    t)) ;if not x is nil then return t. 

(defun demorgan-and (x y)
  (not (or (not x) (not y))) )

(defun demorgan-or (x y)
  (not (and (not x) (not y))) )

;setf assigns a value to a variable. It will overwrite the current value stored. 

;the syntax of let is "let x be 1, let y be 2; return z"
;let* forms variables one at a time, so you can reference the 1st declared variable when declaring the 2nd. 

;nth will give you a certain item. nthcdr will give you the rest of a list starting at a certain index. 

;equal can compare the contents of lisp. eq will compare pointers. eql is similar but can also compare nums
;equalp is like equal except it will ignore capitlization. 

;(type-of x) will tell us the type. Watch out, if you don't use a quote, and it isn't an assigned variable
;then it will give you an error. 

(defun add1 (x)
  (+ 1 x))

(defun increment-list (lst)
  (mapcar #'add1 lst))

;same thing done with lambda. so much prettier. 
(defun list-incf (lst)
  (mapcar (lambda (n) (+ 1 n)) lst))

;find-if can be used to return an element that satisfies a predicate. 
;similar applicative operators are remove-if and remove-if-not
;these can take keyword arguments like :from-end t to find the last item matching that criteria in list. 
;remove-if and if-not can take :count arguments specificying how many elements to remove. 

;musical transposition code. 

;reduce takes a list and reduces it to a single element. #' to multiply, or #'append to combine seperate lists

;every takes a predicate and returns t if each element in lst satisfies it. 

(defun all-odd (lst)
  (every oddp lst))

;also means they're all even. 
(defun none-odd (lst)
  (every evenp lst))

(defun not-all-odd (lst)
  (not (all-odd lst)) )

(defun not-none-odd (lst)
  (not (none-odd lst)) )

(defun sum-list (lst)
  (if (equal lst nil)
      0
    (+ (car lst) (sum-list (cdr lst)))) )


;Functions that make functions
(defun greater-than-predicate (n)
  #'(lambda (x) (> x n)) )
;this is ow a function that can be assigned to a variable like (setf pred greater-than-predicate 2)
;then it can be called using funcall and with another variable which it will compare against the declaration.

;LABELS. notice how count-up-recursive reference n of defun. This lets us hide the helper function rather
;than having it as a separate part. 
(defun count-up (n)
  (labels ((count-up-recursive (cnt)
             (if (> cnt n) nil
               (cons cnt 
                     (count-up-recursive
                      (+ cnt 1))))))
    (count-up-recursive 1)) )

;maybe start storing dialogue segments as variables rather than as printing out strings. This would allow 
;easier identification and less time spent searching for who said what where. Instead it would just be a var
;with their name attached to it. Maybe even there's an association list where it says when and where it was
;said. 

;this will snip out the middle element of a list of length 3. Or i guess the second for a list of any length
(defun snip (x)
  (setf (cdr x) (cdr (cdr x))))

;delete will only remove a single element unlike remove. 

;symbol-value function returns the contents of a symbols value cell. 

(defun it-member (item lst)
  (dolist (e lst)
    (when (equal e item)
      (return t))
    (return nil)))

;Do list can take three arguments (dolist (i j k)) ? what does the kay do? is this the value returned 
;if the statement isn't evaluated for some reason? So I think its what's returned when the count cant go higher

;so it keeps pushing element onto result, and then returns result once element = x. 
(defun it-intersection (x y)
  (let ((result-set nil))
    (dolist (element x result-set)
      (when (member element y)
        (push element result-set)))) )

(defun it-length (lst)
  (let ((length 0))
    (dolist (element lst length)
      (setf length (+ 1 length))))) 

;it-nth

(defun launch (n)
  (do ((count n (- count 1)))
      ((zerop count) (format t "Blast off!"))
    (format t "~S..." count)))

(defun fact (n)
  (do ((i n (- i 1))
       (result 1 (* result i)))
      ((zerop i) result)))

;some messing around with true and null
(true nil) ;return t
(true t);also returns t
(null t) ;returns nil
(null nil) ;returns t
;the if always tests to see if the case is true, so (if nil (+ 2 2)) will always return nil, you can't do this in java. 

;While infinite loops are usually babbabbab they can be useful. 
;Here the function loops forever until a number is entered. 

(defun read-number ()
  (do ((answer nil)) ;sets this variable, 
      (nil) ;test action, will never be true so body will always be initiatied. 
    ;body. This will return, thus breaking the loop if answer is a number, if it isn't then it will just
    ;repeat the loop again, but since answer has no update, the body will just be initiated again. 
    (format t "~&Please enter a number: ")
    (setf answer (read))
    (if (numberp answer)
        (return answer))
    (format t "~&Sorry, ~S is not a number. Try again."
            answer)))
