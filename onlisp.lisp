;On LISP exercises

(defun my-remove-if (fn lst)
  (if (null lst) ;this is really interesting, you're literally saying if lst is null, but the function
      ;is actually, if nulling lst is t, then do this. null nil is t. 
      nil
    (if (funcall fn (car lst)) ;funcall is like apply but takes individual arguments instead of lists. 
        ;fn isn't #' because the function is stored as a variable. 
        (my-remove-if fn (cdr lst)) 
      (cons (car lst) (my-remove-if fn (cdr lst))))))

(defun behave (animal)
  (funcall (get animal 'behavior)))

;??? what's this do? Giving me an error, wag-tail is undefined or something about 0 arguments. 
;HOw do I call this in behave? The advantage is we can just create new animals without having to change behave
(setf (get 'dog 'behavior)
      #'(lambda ()
          (wag-tail)
          (bark)))


;demonstration of scope. Whenever scope-test is called it will use the value of y which is tied up with its
;definition. Declaring a y right before scope-test will not change the value, this y will still be used. 
(let ((y 5))
   (defun scope-test (x)
     (list x y)))

;n is a free variable. It's a closure because it must save a copy of that binding when the function is defined.
;what about lst? 
(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n))
          lst))

;this keeps us from having to define a global variable counter and thus protects it from tampering. 
;this is an example of a closure with local state. So is counter free and copied at the creation of new-id? 
(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))

;this returns a function with local state. Takes a number, returns a closer which when called adds a number to 
;its argument. THe internal state is fixed. 
(defun make-adder (n)
  #'(lambda (x) (+ x n)))

;you create these new variables which are functions and can be used with funcall. 
(setq add2 (make-adder 2)
      add10 (make-adder 10))

(defun make-adderb (n)
  #'(lambda (x &optional change)
      (if change (setq n x)
        (+ x n))))

(setq addx (make-adderb 1))
;when this adder is called with change set to t, then the internal copy of n will be reset to the new value. 
(funcall addx 100 t)
(funcall addx 3)

;creates primitive databases. Takes an assoc-list and returns three closures: query, add, delete
;how do I use delete? 
(defun make-dbms (db)
  (list
   #'(lambda (key)
       (cdr (assoc key db)))
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   #'(lambda (key)
       (setf db (delete key db :key #'car))
       key)))

;this creates a database where this list is its contents. funcall can be used to change the contents. 
(setq cities (make-dbms '((boston . us) (paris . france))))

;creates a nicer syntax for looking up cities. 
(defun lookup (key db)
  (funcall (car db) key))

;here we'll use labels to let us use define a function and call it recursively. Couldn't do with let. 
;this can handle a list of lists, but can't handles lists embedded inside any of those. lame. its pretty hot though. 
(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
             (if (consp lst)
                 (+ (if (eq (car lst) obj) 1 0) ;adds a 1 or a 0
                    (instances-in (cdr lst)))
             0)))
  (mapcar #'instances-in lsts)))

;a tail recursive findif method. Its tail recursive because if its found it just returns the item without further calls. 
(defun our-find-if (fn lst)
  (if (funcall fn (car lst))
      (car lst)
    (our-find-if fn (cdr lst))))

;acc is the value thats returned. 
(defun our-length (lst)
  (labels ((rec (lst acc)
             (if (null lst)
                 acc
               (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))

;returns sum of all ints from 1 to n. I don't know what declare or fixnum are doing. the fixnum seems to be making sure that
;the values given back to tri are both numbers. I don't see the point in this though.  
(defun triangle (n)
  (labels ((tri (c n)
             (declare (type fixnum n c))
             (if (zerop n)
                 c
               (tri (the fixnum (+ n c))
                    (the fixnum (- n 1))))))
    (tri 0 n)))

(defun good-reversre (lst)
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
               (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

;lisp functions can return multiple values, like truncate. 

;values returns multiple values. 
(defun powers (x)
  (values x (sqrt x) (expt x 2)))

(multiple-value-bind (base root square) (powers 4) 
  (list base root square))

;pg 46 this is a functional version of the imperative program below
(defun fun (x)
  (list 'a (expt (car x) 2)))

;imp BAD
(defun imp (x)
  (let (y sqrt)
    (setq y (car x))
    (setq sqr (expt y 2))
    (list 'a sqr)))

;This is a find-if where it returns the successful element and the value returned by the test function. 
(defun find2 (fn lst)
  (if (null lst)
      nil
    (let ((val (funcall fn (car lst))))
      (if val
          (values (car lst) val)
        (find2 fn (cdr lst))))))

;mapcan builds a list out of the results of successive functional calls on a lisp. 
(mapcan #'(lambda (x) (and (numberp x) (list x)))
          '(a 1 b c 3 4 d 5))

;this will returns a list where the non numbers are nil, and numbers are lists. weird. 
(mapcar #'(lambda (x) (and (numberp x) (list x)))
          '(a 1 b c 3 4 d 5))

;UTILITIES: 
(defun last1 (lst)
  (car (last lst)))

;this is hawt. you don't need to check length = 1, you just check to see if there are at least two elements, if not then its 1.
;this is because consp () is nil. 
(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

;destructive
(defun conc1 (lst obj)
  (nconc lst (list obj)))

;this ensures that something is a list by making it a list? why not just use list? Oh, because this checks to see if its a list or not. 
(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;returns t if x is a longer list than y
(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
      (> (length x) (length y)))))

;filters out everything from a list that doesn't satisy the function. Can this be used in mapping reduce? 
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

;will take a list and group it into a number of sublists equal to n. 
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc) ;what is acc? where does it come from? 
               (let ((rest (nthcdr n source)))
                 (if (consp rest)
                     (rec rest (cons (subseq source 0 n) acc))
                   (nrevesre (cons source acc))))))
    (if source (rec source nil) nil)))

;flattens a list of sublists into a single list. 
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;this will work like remove-if but on a tree. great name.
(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                             (cons (car tree) acc)))))))
    (rec tree nil)))

(defun find2 (fn lst)
  (if (null lst)
      nil
    (let ((val (funcall fn (car lst))))
      (if val
          (values (car lst) val)
        (find2 fn (cdr lst))))))

;before tells you if an item is before another item in the list. Like member it'll return cdr. 
;will return t if the second element doesn't exist in set at all. 
(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

;requires both to be in the list
(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

;this will return the cdr from where the duplicate first occurs
(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

;this will split a list in two halves, one failing the function, the other passing it. 
(defun split-if (fn lst)
          (let ((acc nil))
            (do ((src lst (cdr src)))
                ((or (null src) (funcall fn (car src)))
                     (values (nrevesre acc) src))
              (push (car src) acc))))

;looks at one element at a time, then returns element with highest score derived from function.
;could be used with length, most numbers, something like that. 
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
    (let* ((wins (car lst))
           (max (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (> score max)
            (setq wins obj
                  max score))))
      (values win max))))

;this is the same as most, except it returns a lst of elements for which function yields highest score. 
(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
    (let ((result (list (car lst)))
          (max (funcall fn (car lst))))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (cond ((> score max) ;first condition
                 (setq max score
                       result (list obj)))
                ((= score max) ;second condition
                 (push obj result)))))
      (values (nreverse result) max))))
         
;function must be a predicate of two arguments. returns lst, which according to predicate, beats the others. 
;can be thought of as the equivalent of to car of sort, but more efficient.     
(defun best (fn lst)
  (if (null lst)
      nil
    (let ((wins (car lst)))
      (dolist (obj (cdr lst))
        (if (funcall fn obj wins)
            (setq wins obj)))
      wins)))
      
;returns a list of the items which have the highest value for the function, also returns the counts. 
(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
    (let ((result (list (car lst)))
          (max (funcall fn (car lst))))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (cond ((> score max)
                 (setq max score result (list obj)))
                ((= score max)
                 (push obj result)))))
      (values (nreverse result) max))))

;review &rest and &key
 
;this starts the list of numbers at 0? 
(defun map0-n (fn n)
  (mapa-b fn 0 n))

;I assume this starts the list of numbers at 1
(defun map1-n (fn n)
  (mapa-b fn 1 n))

;applyies a function to a range of numbers. 
(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

;he says something about  CLTL2 rendering mapping antiquated. says mapa-b can be rewritten
;(collect (#Mfn (scan-range :from a :upto b :by c))) 

;just a redefintion of mapa-b  using map-> 
(defun mapa->b (fn a b &optional (step 1))
  (map-> fn 
         a
         #'(lambda (x) (> x b))
         #'(lambda (x) (+ x step))))

;this works for sequences of objects of any kind. begins with second argument, end of sequence defined by
;the function given as fourth argument. navigates arbitarary data structures as well as numbers. 
(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

;this is a non-descrututive version of mapcan
(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lst))) ;apply uses mapcar, which uses fn

;this lets us mapcar over several lists instead of having to append them. for things like sqrt, expt, etc...
(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nrevese result)))

;recursive mapcar, meant to work on trees, glory glory. can take more than one argument like mapcar. 
(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
    (apply #'mapcar
           #'(lambda (&rest args)
               (apply #'rmapcar fn args))
           args)))

;lets the user type input and then turns it into a list. 
(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                        (apply #'read-line args)
                        ")" ))))

;takes a string to print as an argument, and then additional values to be placed in string.
(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

;this simulates lisp top-level. 
(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break loop.~%")
  (loop
   (let ((in (apply #'prompt args)))
     (if (funcall quit in)
         (return)
       (format *query-io* "~%~A~%" (funcall fn in))))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

;this deconstructs a symbol into a list of individual symbols.
(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element c))
                 (symbol-name sym))))

;this won't work for nested lists.  
(defun combine (lst) 
  (let ((str ""))
    (dolist (i lst)
      (setf str (concatenate 'string (symbol-name i) str)))
    (symb (reverse str))))
    
;CHAPTER 5 returning functions. 
;you can use complement to get opposite of a predicate, so now there is little need for remove-if-onts and such.
;could just be written (remove-if (complement #'predicate) lst)


(defvar *!equivs* (make-hash-table))

;this returns the destructive equivalent of a function. more efficient to define this as macro, maybe he'll explain how later. 
(defun ! (fn)
  (or (gethash fn *!equivs*) fn))

;this sets destructive equivalents. 
(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))

;this caches the return value of all previous call, and when the fucntion is called again, it looks to see if value is already known.
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
            (setf (gethash args cache)
                  (apply fn args)))))))

;this lets you create composition of functions. all fns must take one argument, except the last one which can take any number.
(defun compose (&rest fns)
  (if fns
      (let ((fn (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
    #'identity))

;this means a mapcar lambda could be rewritten (fif #'x #'y)
(defun fif (if then &optional else)
  #'lambda (x)
  (if (funcall if x)
      (funcall then x)
    (if else (funcall else x))))

;if we're doing something with a (find-if #'lambda(x) (and x y z a)) could be (find-if (fint #'x #'y #'z) a). INTERSECTION
(defun fint (fn &rest fns)
  (if (null fns)
      fn
    (let ((chain (apply #'fint fns)))
      #'(lambda (x)
          (and (funcall fn x) (funcall chain x))))))

;looks like this is the same as fint except an or instead of and. UNION
(defun fun (fn &rest fns)
  (if (null fns)
      fn
    (let ((chain (apply #'fun fns)))
      #'(lambda (x)
          (or (funcall fn x) (funcall chain x))))))

;list recurser. functionp is causing a problem? 
(defun lrec (rec &optional base)
  (labels ((self (lst) ;this labels will build a local recursive function. 
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                   base)
               (funcall rec (car lst)
                        #'(lambda ()
                           (self (cdr lst)))))))
    #'self)) 

;how do I use these functions once they're declared?  
;length 
(lrec #'(lambda (x f) (1+ (funcall f))) 0)

;every
(lrec '(lambda (x f) (and (oddp x) (funcall f))) t)

;copy-list
(lrec '(lambda (x f) (cons x (funcall f))))

;remove-duplicates
(lrec '(lambda (x f) (adjoin x (funcall f))))

;find-if
(lrec '(lambda (x f) (if (fn x) x (funcall f))))

;some
(lrec '(lambda (x f) (or (fn x) (funcall f))))


;Tree Traverser. Two values are passed in recursive case, left and right subtree. If the base
;argument is a function, it'll be called on the current leaf. In flat list recursion base
;case will always be nil. 
(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                   base)
               (funcall rec (self (car tree))
                        (if (cdr tree)
                            (self (cdr tree)))))))
    #'self))

;copy-tree
(ttrav #'cons)

;count-leaves
(ttrav #'(lambda (l r) (+ 1 (or r 1))) 1)

;flatten
(ttrav #'nconc #'mklist)

;this is like tree traverse, except it'll take a functino of three arguments, current object and two recursers. 
(defun trec (rec &optional (base #'identity))
  (labels
      ((self (tree)
         (if (atom tree)
             (if (functionp base)
                 (funcall base tree)
               base)
           (funcall rec tree
                    #'(lambda ()
                        (self (car tree)))
                    #'(lambda ()
                        (if (cdr tree)
                            (self (cdr tree)))) ))))
       #'self))

;faltten with trec
(trec #'(lambda (o l r) (nconc (funcall l) (funcall r))) #'mklist) 

;rfind, in this case for oddp. 
(trec #'(lambda (o l r) (or (funcall l) (funcall r)))
      #'(lambda (tree) (and (oddp tree) tree))) 

;CHAPTER 6: NETWORKS

;first he shoes a bad way to write 20 questions, and then how to do it with closures.

(defvar *nodes* nil)

(defun defnode (&rest args)
  (push args *nodes*)
  args)

(defun compile-net (root)
  (let ((node (assoc root *nodes*))) ;so you have to use defnode to make the nodes before using this right? 
    (if (null node)
        nil
      (let ((conts (second node))
            (yes (third node))
            (no (fourth node)))
        (if yes
            (let ((yes-fn (compile-net yes))
                  (no-fn (compile-net no)))
              #'(lambda ()
                  (format t "~A~%>> " conts)
                  (funcall (if (eq (read) 'yes)
                               yes-fn
                             no-fn))))
          #'(lambda () conts)))))) 

;CHAPTER 7: MACROS

;so why do we need to use a macro instead of this defun? 
(defun !nil! (x)
  (setq x nil))

;this doesn't work on symbols, so what's the point of making it a macro since it's less versatile? 
(defmacro nil! (var)
  (list 'setq var nil))

(defmacro nil! (var)
  `(setq ,var nil))

;setf and setq can take multiple pairs. Maybe write my own that works like (setf ((a 2) (b 1) (c (1 2 3))))

(defmacro nif (expr pos zero neg)
  `(case (truncate (sigum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

; ,@ splices a list, so (1 2 3) is 1 2 3. the ,@ must occur within something, not at the beginning. use with atoms can only occur at end. 

(defmacro our-when (test &body body)
  `(if ,test
       (progn
         ,@body)))

(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))

(defmacro mem-equal (obj lst)
  `(member ,obj ,lst :test #'equal))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr))) ;   ',?

;written strangely to avoid using gensyms, whatever that means. 
(defmacro our-dolist ((var list &optional result) &body body)
  `(progn
     (mapc #'(lambda (,var) ,@body)
           ,list)
     (let ((,var nil))
       ,result)))

;like when, except it also binds a variable to a value that's returned. 
(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro our-do (bindforms (test &rest result) &body body)
  (let ((label (gensym)))
    `(prog ,(make-initforms bindforms)
       ,label
       (if ,test
           (return (progn ,@result)))
       ,@body
       (psetq ,@(make-stepforms bindforms))
       (go ,label))))

(defun make-initforms (bindforms)
  (mapcar #'(lambda (b)
              (if (consp b)
                  (list (car b) (cadr b))
                (list b nil)))
          bindforms))

(defun make-stepforms (bindforms)
  (mapcan #'(lambda (b)
              (if (and (consp b) (third b))
                  (list (car b) (third b))
                nil))
          bindforms))

;and written with macros. Its inefficient, but clear, which is what's wanted for expander code. 
(defun our-and (&rest args)
  (case (length args)
    (0 t)
    (1 (car args))
    (t `(if ,(car args)
            (our-and ,@(cdr args))))))

(defmacro for ((var start stop) &body body)
  `(do ((b #'(lambda (,var) ,@body))
        (count ,start (1+ count))
        (limit ,stop))
       ((> count limit))
     (funcall b count)))

;so this one is reversible, problem is going from 10 to 1 will end at 2, which is less obvious than 1 to 10 ending at 9. 
;his for doesn't leave off one either. 
;MAKE SURE THIS HAS NO CAPTURABLE VARIABLES
(defmacro my-for ((var start stop) &body body)
  `(do ((b #'(lambda (,var) ,@body))
        (count ,start (if (> ,start ,stop)
                          (1- count)
                        (1+ count))) 
        (limit ,stop))
       ((if (> ,start limit)                ;GACK
            (equal count (1- limit))
          (equal count (1+ limit)))) 
     (funcall b count))) 

;WARNING, these loops can't be broken out of, since they're within an implicit block statement. Well the for is broken, but not the block. 
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))


;CHAPTER 11
(defmacro when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
    `(let (,(car binds))
       (If ,(caar binds)
           (when-bind* ,(cdr binds) ,@body)))))

;allows you to make multiple gensym declarations within a single phrase
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

;lets you declare bindings based on certain conditions. could redefine my forloop with this. 
(defmacro condlet (clauses &body body)
  (let ((bondfn (gensym))
        (vars (mapcar '(lambda (v) (cons v (gensym)))
                      (remove-duplicates
                       (mapcar 'car
                               (mappend 'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar 'car vars)
                ,@body))
       (cond ,@(mapcar '(lambda (cl)
                          (condlet-clauses vars cl bodfn))
                       clauses))))) 

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar 'cdr vars)
                (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar 'cdr vars))))))

(defun condlet-binds (vars cl)
  (mapcar '(lambda (bindform)
             (if (consp bindform)
                 (cons (cdr (assoc (car bindform) vars))
                       (cdr bindform))))
          (cdr cl)))

;this is a generic database function which will open a global var *db* and lock it, edit, then release it. 
(defmacro with-db (db &body body)
  (let ((temp (gensym)))
    `(let ((,temp *db*))
       (unwind-protect
           (progn
             (setq *db* ,db)
             (lock *db*)
             ,@body)
         (progn 
           (release *db*)
           (setq *db* ,temp))))))
;this could alternatively be written using a helper function with-db-fn as shown on pg 149. 


;code from chapter 8, some of which I rewrote using with-gensyms.
;The code is meant to be a 2d drawing program where lines have a point (x,y) and a vector (dx, dy)
(defmacro with-rewdraw ((var objs) &body body)
  (with-gensyms ((gob x0 y0 x1 y1))
                `(let ((,gob ,objs))
                   (multiple-value-bind (,x0 ,y0 ,x1 ,y1) (bounds ,gob)
                     (dolist (,var ,gob) ,@body)
                     (multiple-value-bind (xa ya xb yb) (bounds ,gob)
                       (redraw (mind ,x0 xa) (min ,y0 ya)
                               (max ,x1 xb) (max ,y1 yb)))))))

(defun move-objs (objs dx dy)
  (with-redraw (o objs)
               (incf (obj-x o) dx)
               (incf (obj-y o) dy)))

(defun scale-objs (objs factor)
  (with-redraw (o objs)
               (setf (obj-dx o) (* (obj-dx o) factor)
                     (obj-dy o) (* (obj-dy o) factor)))) 

            
(defun negp (x)
  (if (< x 0)
      t
    nil))

;0 will return t
(defun posp (x)
  (if (>= x 0)
      t
    nil)) 
