;XANADU
;USE TRANSCLUSIONS. Xanalogical structure. 

;write my own format function. So I guess it'll take something like 
;"hello ~S" and would create a println with "hello " then substitute the
;variable given for ~S. would also be nice to format spacin for line-nums. 
;(define-macro (printf string . values)
  
;this will need an auxiliary that'll go through the string and find formatting

;brings in some novak utility code like while, dolist, dotimes. 
;(load "~/Projects/initdr-novak.scm")

;WHY are all macros so iffy on this? sometimes push works, sometimes it says its an unbound variable.
;dotimes was working earlier, but not anymore... 

(define (1+ n) (+ n 1))
(define (1- n) (- n 1))			  

(define (void? n)
  (equal? n #!void))

(define (nthcdr n lst)
  (list-tail lst n)) 

(define (nth n lst)
  (list-ref lst n))

(define (last lst)
  (if (not (null? lst))
      (car (nthcdr (1- (length lst)) lst))))

(define-macro (push var val)
  `(set! ,var (cons ,val ,var)))

(define-macro (pop var)
  `(set! ,var (cdr  ,var)))

(define-macro (inc n)
  `(set! ,n (+ ,n 1)))

(define-macro (dec n)
  `(set! ,n (- ,n 1)))

;change this to remove since it isn't destructive. 
(define (delete item list)
  (cond
   ((equal? item (car list)) (cdr list))
   (else (cons (car list) (delete item (cdr list))))))

(define (delete-assoc item lst)
  "deletes an association list."
  (delete (assoc item lst) lst))

(define (replace target item source)
  "replaces an element in a list."
  (replace-aux target item source '()))

(define (replace-aux target item source replacement)
  (if (null? source)
      (reverse replacement)
      (if (equal? (car source) target)
	  (replace-aux target item (cdr source) (cons item replacement))
	  (replace-aux target item (cdr source) (cons (car source) replacement)))))

;fix this up so it doesn't have to take #\c as input, but could take in a string "c" as well. 
(define (replace-char target ch string)
  (list->string (replace target ch (string->list string))))

(define (before? a b lst)
  "returns #t if a occurs before b in the lst"
  (if (and (member a lst) (member b lst))
      (> (length (member a lst))
	 (length (member b lst)))
      "either a or b is not in the list.")) ;should use a better method of error breaking. 

;weird that this is required to set lists. 
(define (list-set! list k val)
    (if (zero? k)
        (set-car! list val)
        (list-set! (cdr list) (- k 1) val)))

(define (newline? ch)
  (if (or (equal? ch #\newline) (equal? ch #\return) (equal? ch "\r"))
      #t
      #f))

(define (space? ch)
  (if (or (equal? #\space ch) (equal? " " ch))
      #t
      #f))

(define (empty? str)
  "returns t if the string is empty."
  (if (equal? str "")
      #t
      #f))

;if the first of sub matches first of lst, go further, if it doesn't get results
;then skip over that element in lst. 
;(exact-subset? '(c b) '(a b c)) -> #f"
(define (exact-subset? sub lst)
  (exact-subset-aux sub sub lst))

(define (exact-subset-aux sub sub2 lst)
  (if (and (null? lst) (not (null? sub2)))
      #f
      (if (null? sub2)
	  #t
	  (if (equal? (car sub2) (car lst)) ;I need branching paths here. if cdr on both
	      ;doesn't work, then I need to back up and return. 
	      (if (exact-subset-aux sub (cdr sub2) (cdr lst))
		  (exact-subset-aux sub (cdr sub2) (cdr lst))
		  (exact-subset-aux sub sub (cdr lst)))
	      (exact-subset-aux sub sub (cdr lst))))))

(define (substring? word string)
  "meant to be used to find single words, but could be used to find a phrase."
  (exact-subset? (string->list word) (string->list string)))

;unfinished. currently more like string-member which I can't see a use for. 
(define (search-string word string)
  "this will return a word and char-count  of where the word was found."
  (let ((word-count 0)
	(char-count 0))
    (member word (list-string string))))

(define (list-string string)
  (list-string-aux string 0 '()))

(define (list-string-aux string place lst)
  (if (equal? place (word-count string))
      (reverse lst)
      (list-string-aux string (1+ place) (cons (nth-word place string) lst))))

;splice-word is not the best name. think of something clearer. 
(define (splice-word word break-point)
   (list->string (nthcdr (find-char break-point word) (string->list word))))

;this will be determined by tense as well. 
(define *contractions* '(("'m" . "am") ("'ve" . "have") ("'s" . "is") ("'ll" . "will")
			 ("'re" . "are")))

(define (expand-contraction word)
  "Takes in a single word and expands its form."
  (string-append  
   (substring word 0 (find-char #\' word)) " "
   (cdr (assoc (splice-word word #\') *contractions*))))

(define (nth-char n string)
  (nth n (string->list string)))

;trouble where it'll count "  " as a word since it's preceded by a space. 
(define (nth-word n string)
  (nth-word-aux n (string->list string) 0 '()))

;can cause some problems if it get a space as a word, it'll return an empty string.
;write a version that doesn't trim punctuation.  
(define (nth-word-aux n ch-lst place word)
  (if (null? ch-lst)
      #f
      (begin
	(set! word (cons (car ch-lst) word))
	(if (space? (car ch-lst))
	    (begin
	      (set! place (+ 1 place))
	      (if (eq? n (1- place))
		  (trim-punctuation (list->string (reverse word)))
		  (nth-word-aux n (cdr ch-lst) place '())))
	    (if (and (null? (cdr ch-lst)) (eq? n place))
		(trim-punctuation (list->string (reverse word)))
		(nth-word-aux n (cdr ch-lst) place word))))))

(define (nth-word-w/punc n string)
  "nth-word that preserves punctuation. might be more useful in some instances."
  (nth-word-w/punc-aux n (string->list string) 0 '()))

(define (nth-word-w/punc-aux n ch-lst place word)
  (if (null? ch-lst)
      #f
      (begin
	(set! word (cons (car ch-lst) word))
	(if (space? (car ch-lst))
	    (begin
	      (set! place (+ 1 place))
	      (if (eq? n (1- place))
		  (trim-spaces (list->string (reverse word)))
		  (nth-word-w/punc-aux n (cdr ch-lst) place '())))
	    (if (and (null? (cdr ch-lst)) (eq? n place))
		(trim-spaces (list->string (reverse word)))
		(nth-word-w/punc-aux n (cdr ch-lst) place word))))))

(define (list-string-w/punc string)
  "list-string that preserve punctuation, so its more accurate in preserving the string
state. This is what'll be used in pattern matching since it'll keep x? in a string."
  (list-string-w/punc-aux string 0 '()))

(define (list-string-w/punc-aux string place lst)
  (if (equal? place (word-count string))
      (reverse lst)
      (list-string-w/punc-aux string (1+ place) (cons (nth-word-w/punc place string) lst))))


;define equal-or which tests one thing against a bunch of stuff. 

;add parens, brackets, etcetera. 
(define *punctuation* '("." "," "!" "?" #\. #\, #\! #\?))

(define (punctuation? ch)
  (if (member ch *punctuation*)
      #t
      #f))

(define (trim-string string)
  "trims the last character off a string."
  (list->string (reverse (cdr (reverse (string->list string))))))

(define (trim-punctuation string)
  (let ((ch (last-char string)))
    (if (and (not (space? ch))
	     (not (punctuation? ch)))
	string
	(trim-punctuation (trim-string string)))))

(define (trim-spaces string)
  (if (or (null? (string->list string)) (null? (cdr (string->list string))))
      string
      (trim-spaces-aux (string->list string))))

(define (trim-spaces-aux lst)
  (if (pair? lst) ;make sure that there will be a car. 
      (if (space? (car lst))
	  (trim-spaces-aux (cdr lst))
	  (if (space? (last lst))
	      (trim-spaces-aux (string->list (trim-string (list->string lst))))
	      (list->string lst)))))



;I think this is being overzealous and getting rid of things like #\" and such. it's this
(define (trim-returns string)
  (let ((lst (string->list string)))
    (if (newline? (last lst))
	(trim-string string)
	string)))

(define (trim-numbers string)
  (trim-numbers-aux (string->list string)))

(define (trim-numbers-aux lst)
  (if (number? (char->number (last lst)))
      (trim-numbers-aux (trim-list lst))
      (list->string lst)))

(define (char->string char)
  (if (not (void? char))
      (list->string (list char))))

;not sure how this could even get called with a void value... 
(define (char->number char)
  (if (not (void? char))
      (string->number (list->string (list char)))))

(define (trim-list lst)
  (reverse (cdr (reverse lst))))

(define (cleanup-string string)
  (trim-spaces (trim-numbers (trim-returns string))))

(define (count-sentences string)
  (+ (char-count #\. string) (char-count #\? string)
     (char-count #\! string)))

;make sure to not cut off " or parentheses. find at what position sentence end, 
;convert to list, then nthcdr at that position? 
(define (get-sentence string)
  "returns a substring that starts at the beginning of the string until sentence's end."
  (let ((end (find-punctuation string)))
    (if (not (void? end))
	(begin 
	  (if (not (void? (find-other-punctuation (substring string end (string-length string)))))
	      (set! end (1+ end)))
	  (trim-spaces (substring string 0 (1+ end))))))) ;1+ since find-char will return at what place that char is, and we want it included. 

;change this so that it it can take interjections as punctuation like Look! there they are.
;also it doesn't account for a sentence ending "run." after the period.  
;make it so if a string has no periods then it just returns the string. bad idea? 
(define (nth-sentence n string)
  (if (and (zero? (count-sentences string)) (eq? 0 n)) 
      string
      (nth-sentence-aux n string 0)))

(define (nth-sentence-aux n string position)
  (if (eq? n position)
      (get-sentence string)
      (nth-sentence-aux n 
			(list->string (nthcdr (1+ (find-punctuation string)) 
					      (string->list string)))
				(1+ position))))

;write something that'll get phrases, so it'll separate things by comma, dashes, parens, colons, etcetera.
;it'd be more useful if this could be expanded to find asides. 

;write something that'll take a whole string. then break it up into a list of its individual sentences. 
(define (list-sentences string)
  (list-sentences-aux string '() 0 (count-sentences string)))

(define (list-sentences-aux string lst count max)
  (if (eq? count max)
      (reverse lst)
      (list-sentences-aux string (cons (nth-sentence count string) lst)
			(1+ count) max)))
      
;this is pretty dumb, it returns a string, not a char. 
(define (last-char string)
  (let ((str (string->list string)))
    (if (not (null? str))
	(last str))))

(define (first-char string)
  (let ((str (string->list string)))
    (if (not (null? str))
	(car str))))

;maybe convert ch to a char if it isn't. 
(define (find-char ch word)
  "will return at what place in a word the first instance of the char appears."
  (let ((word (string->list word)))
    (list-index ch word)))


;this is ending punctuation. uh oh! this doesn't work right, it'll returns which it finds first.
;I need it to return the lowest. 

(define (find-punctuation string)
  (get-least (filter number? 
	  (list (find-char #\. string) (find-char #\! string) (find-char #\? string)))))

;change this! use aif here. 
(define (find-other-punctuation word)
  (get-least (filter number? (list (find-char #\, word) (find-char #\" word)
				   (find-char #\) word) (find-char #\( word)))))

(define (list-index element lst)
  (list-index-aux element lst 0))

(define (list-index-aux element lst position)
  "returns position of element in a list. #f if abscent."
  (if (null? lst)
      #f
      (if (equal? element (car lst))
	  position
	  (list-index-aux element (cdr lst) (1+ position)))))

(define (last-word string)
  "returns the last word minus punctuation."
  (nth-word (1- (word-count string)) string))

(define *sentence-types* '(("." . declarative) ("?" . interrogative) ("!" . exclamatory)
			   ("." "!" imperative)))

(define (sentence-type sentence)
  (cdr (assoc (last-char sentence) *sentence-types*)))

;this is kind of error prone though 'cause it would count indentation as 4 words... 
(define (word-count string)
  (if (and (not (empty? string)) (not (void? string)))
      (let ((count 0))
	(for-each (lambda (ch) (if (space? ch) (set! count (+ 1 count))))
		  (string->list string))
	(1+ count))
      0))
 
;counts the number of elements in a list.  
(define (element-count element lst)
  (element-count-aux element lst 0))

(define (element-count-aux element lst count)
  (if (null? lst)
      count
      (if (equal? element (car lst))
	  (element-count-aux element (cdr lst) (1+ count))
	  (element-count-aux element (cdr lst) count))))
	
;fix this so it'll count more than just the first sequence, but will return the longest? 
(define (elements-in-row-count element lst)
  "will return how many items it found in a row. starting with the first sequence."
  (elements-in-row-count-aux element lst 0 #f))

(define (elements-in-row-count-aux element lst count begun?)
  (if (and begun? (not (equal? element (car lst))))
      count
      (if (equal? element (car lst))
	  (elements-in-row-count-aux element (cdr lst) (1+ count) #t)
  	  (elements-in-row-count-aux element (cdr lst) count #f))))
	  
(define (char-count ch string)
  "count instances of a char in a string."
  (element-count ch (string->list string)))

(define (string->char string)
  (car (string->list string)))

(define (string-begins? string beginning)
  (equal? beginning (nth-word 0 string)))

(define (before-in-alphabet? str1 str2)
  (before? (first-char str1) (first-char str2) *alphabet*))

;allow this to take in infinite arguments for a much more useful function. 
(define (lower-letter str1 str2)
  (if (before-in-alphabet? str1 str2)
      str1
      str2))

(define (get-least lst)
  (if (pair? lst)
      (get-lowest-aux lst (car lst)))) ;won't work with negatives since lowest starts at 0. 

(define (get-lowest-aux lst lowest)
  (if (null? lst)
      lowest
      (if (< (car lst) lowest)
	  (get-lowest-aux (cdr lst) (car lst))
	  (get-lowest-aux (cdr lst) lowest))))

(define (get-greatest lst)
  (get-greatest-aux lst (car lst))) ;won't work with negatives since lowest starts at 0. 

(define (get-greatest-aux lst greatest)
  (if (null? lst)
      greatest
      (if (> (car lst) greatest)
	  (get-greatest-aux (cdr lst) (car lst))
	  (get-greatest-aux (cdr lst) greatest))))

;hmm, got an error here and now everything is broke. 
(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

;loop through each word in a sentence, and perform a procedure on each word. 
(define (sentence-loop string proc)
  "allows you to perform a proceadure on each word in a string.
procedure must produce a side effect of some kind, can't just return a value."
  (let loop ((i 0))
    (apply proc (list (nth-word i string))) 
    (if (< i (1- (word-count string)))
	(loop (1+ i)))))

;update this so it isn't case sensitive. 
(define (sentence-contains word string)
  (let ((true? #f))
    (sentence-loop (lambda (w) (if (equal? word w) (set! true? #t))) string)
    true?))

;count syllables for determing meter of poetry. 

(define (sublist lst start end)
  "gives you a sublist starting at nth element, consing on until it start = end."
  (sublist-aux lst start end '()))

(define (sublist-aux lst start end lst2)
  (if (eqv? start end)
      (reverse lst2)
;      (reverse (cons (nth end lst) lst2))
      (sublist-aux lst (1+ start) end (cons (nth start lst) lst2))))

;=========================================
;Patern matching
;=========================================
;use this pattern matching stuff in finding book/chapter names. 
(define (var? x)
  "returns if a symbol matches the form for pattern matching variables."
  (if (symbol? x)
      (equal? (first-char (symbol->string x)) #\?) ;last char? maybe use first for convention. 
      #f))

(define (match pat inp)
  "returns a list of bindings if the pattern matches the input."
  (match-aux pat inp '((t t))))

;why's this put so much stuff in? 
(define (match-aux pat inp bindings)
  (and bindings 
       (if (pair? pat) ;interior node? 
	   (and (pair? inp)
		(match-aux (cdr pat) (cdr inp) 
			   (match-aux (car pat) (car inp) bindings)))
	   (if (var? pat) ;is leaf a variable? 
	       (let ((binding (assoc pat bindings)))
		 (if binding 
		     (and (equal? inp (cadr binding))
			  bindings)
		     (cons (list pat inp) bindings)))
	       (and (eqv? pat inp) bindings)) )))

(define (sublis alist tree)
  "performs substitutions in a list. uses (x y) instead of (x . y)" 
  (if (pair? tree)
      (cons (sublis alist (car tree))
            (sublis alist (cdr tree)))
      (if (assoc tree alist) 
          (cadr (assoc tree alist))
          tree)))

;not working!
(define (transform pattern-pair input)
  (let ((bindings '()))
    (if (set! bindings (match (car pattern-pair) input))
;	(begin 
;	  (set! bindings (match (car pattern-pair) input))
	  (sublis bindings
		  (cadr pattern-pair))) ))

;========================================
;MAP REDUCE
;========================================
;(define (mapreduce mapfun reducefun lst)
 ; (let (db keylist)

;===============================================================================
;===============================================================================
;PATHS
;===============================================================================

(define *paths* "~/Projects/paths.txt")

(define *path-map* '())

;path-names should be hyphenated, but if they aren't, I can fix them up. 
(define (write-path path-name path)
  (with-output-to-file (list path: *paths*
			     append: #t)
    (lambda () ;why does it have to be lambda? 
      (println  path-name ":     "  path))))

;this will contain all the hashmaps in an assoc list (bible . bible-map)
;or it could be a table itself... 
(define *open-paths* '())
;I'll lose this everytime, and if notes and stuff are saved to the hash then those'll be gone
;too. I guess I should write them to a file, each entry with its table and key, followed by
;the contents. 

;an open path is defined as a pair of name and map that is in the list open-paths. 
(define (path-opened? pathname)
  (if (assoc pathname *open-paths*)
      #t
      #f))

;loading a path means mapping its contents and putting that into open-paths. 
;maybe change name to map-path. maybe more specifically map-poem
(define (load-path name filename)
  "puts a file into a hasmap, and pushes it onto *open-paths*"
  (let ((table (make-table)))
    (fold-lines-in-file 
     filename
     (lambda (line line-number)
       (table-set! table line-number line)
       (1+ line-number)) 1)
    ;check if the path is already there, if it is overwrite the old entry. 
    (if (path-opened? name)
	(set! *open-paths* (cons (cons name table) (delete-assoc name *open-paths*))) ;use replace here? 
	(set! *open-paths* (cons (cons name table) *open-paths*)))))
    ;(push *open-paths* table)))
     
(define (path-ref n)
  (table-ref *path-map* 1))

(define (user-load-path number)
  (let ((entry (path-ref number)))
    (load-path (get-file-name entry) (get-file-path entry))))

(define (get-file-name entry)
  (condense-name (substring entry 0 (find-char #\: entry))))

;come back to this and decapitalize the string. 
(define (condense-name name)
  (string->symbol (list->string (replace #\space #\- (string->list name)))))

(define (get-file-path entry)
  (substring entry (find-char #\~ entry) (string-length entry)))

(define (remove-path path-name)
  (set! *open-paths* (delete-pair path-name *open-paths*)))

;make sure to call this whenever paths file is updated.
(define (map-paths)
  (load-path 'file-paths *paths*) ;this doesn't overwrite the previous path though. 
  (set! *path-map* (access-file 'file-paths)))

;===============================================================================
;===============================================================================
;TEXT retrieval and access.
;===============================================================================
;make this optional, so that for some files you won't need to specify a book. 
(define (text-ref text index1 index2 index3) ;text is a table, will mess up bible stuff!
  (table-ref text (list index1 index2 index3) #f)) 

;bring over the bible functions like get-verse and such. make more general. 

;===============================================================================
;===============================================================================
;BIBLE processing stuff
;===============================================================================
;why on earth does it always say this is an unbound variable!?!?!
(define *bible* "~/Projects/xanadu/the-holy-bible/kjv-text-only.txt")

(define *bible-books* '("ECCLESIASTES" "SONG OF SOLOMON" "HOSEA" "JOEL" "AMOS" "OBADIAH" "JONAH" "MICAH" "NAHUM" "HABAKKUK" "ZEPHANIAH" "HAGGAI" "ZECHARIAH" "MALACHI" "EZRA"  "SONG OF SOLOMON" "THE PROVERBS"  "THE LAMENTATIONS OF"  "THE GOSPEL ACCORDING TO" "THE ACTS OF THE APOSTLES" "EPISTLE OF"  "EPISTLE GENERAL OF" "THE REVELATION OF" "GENESIS" "EXODUS" "NUMBERS" "LEVITICUS" "DEUTERONOMY" "THE BOOK OF" "SAMUEL" "THE KINGS" "CHRONICLES"))

(define (book? line)
  (check-book (string->list line) *bible-books*))

;subset might be too lenient here. it'll return true even if it isn't an exact
;match. so right an exact match function. this is still not working cause it's counting 85!
(define (check-book line books)
  (if (null? books)
      #f
      (if (exact-subset? (string->list (car books)) line) ;load-bible quits here a lot, and I don't know why. too slow? 
	  #t
	  (check-book line (cdr books)))))

;maybe make this more discerning. It doesn't just need chapter, but it can't have BOOK and
;CHAPTER in same part. 
(define (chapter? line)
  (exact-subset? (string->list "CHAPTER") (string->list line)))

(define (psalm? line)
  (exact-subset? (string->list "PSALM") (string->list line)))

;I tried writing this using an index list instead of book, chap, vers, but it mapped
;everything to the final value of index for some weird reason. 
;way too slow. but I'm not sure why, 'cause fold-lines is recursive?
;I notice it lags quite a bit at a few parts. 
;it went really fast when it only indexes by chapter... why exponentionally? 
(define (load-bible)
  "Maps the bible with a list containing the index as the key to each verse."
  (let ((table (make-table))
	(book 0)
	(chapter 0)
	(verse 0))
    (fold-lines-in-file 
;     "~/Projects/xanadu/the-holy-bible/genesis.txt"
     "~/Projects/xanadu/the-holy-bible/kjv-text-only.txt"
     (lambda (line number)
      ; (println book " " chapter " " verse)
       (cond ((book? line) (set! book (1+ book))
	      (set! chapter 0);resets chapter when book changes. 
	      (set! verse 0)) ;resets verse when book changes. 
	     ((or (chapter? line) (psalm? line)) 
	      (set! chapter (1+ chapter))
	      (set! verse 0))) ;resets verse when chapter changes.
       (if (and (not (book? line)) (not (chapter? line)))
	   (set! verse (1+ verse)))
 ;      (if (not (newline? line))
	   (table-set! table (list book chapter verse) line)); (trim-returns line)))) ;why!?!
     1) 
    (set! *open-paths* (cons (cons 'bible table) *open-paths*))))
;need to include the same fail-safe here of overwriting itself in open-paths if its already there

;==================================================
;RETRIEVING books, chapters, verses
;==================================================
;is I'm using triple indexing for everything, even prose and poems then I should make these
;more general. chapters in book could apply to a lot more than just the bible. 
;change names to bible-chapter, bible-book, bible-verse. more general functions can
;be renamed get-first-index get-second-index get-third-index, get-third-index-sequence. 

(define (bible-ref index)
  (table-ref *bible* index #f))

(define (get-book book)
  (get-chapter-sequence book 1 (chapters-in-book book)))

(define (get-chapter book chapter)
  (get-verse-sequence book chapter 1 (verses-in-chapter book chapter)))

(define (get-chapter-sequence book at end)
  (reverse (get-chapter-sequence-aux '() book at end)))

(define (get-chapter-sequence-aux lst book at end)
  (if (equal? at end)
      (cons (get-chapter book at) lst)
      (get-chapter-sequence-aux (cons (get-chapter book at) lst) book (1+ at) end)))

(define (get-verse book chapter verse)
  (bible-ref (list book chapter verse)))

;return multiple values instead of a list? 
(define (get-verse-sequence book chapter at end)
  (reverse (get-verse-sequence-aux '() book chapter at end)))

(define (get-verse-sequence-aux lst book chapter at end)
  (if (equal? at end)
      (cons (get-verse book chapter at) lst)	
      (get-verse-sequence-aux  (cons (get-verse book chapter at) lst)
			       book chapter (1+ at) end)))

;is text a map or a symbol? change title to index1-count
(define (books-in-text text)
  (books-in-text-aux text 0))

(define (books-in-text-aux text count)
  (if (text-ref text (index count 0 0))
      (books-in-text-aux text (1+ count))
      count))

(define (verses-in-chapter book chapter)
  (verses-in-chapter-aux book chapter 1))

(define (verses-in-chapter-aux book chapter verse)
  (if (bible-ref (list book chapter verse))
      (verses-in-chapter-aux book chapter (1+ verse))
      (- verse 2))) ;I don't understand why it's two over. it counts the return which makes sense,
                    ;but why does it count the #f as one? won't work generally unless every text
                    ;has these extra lines, so try to eliminate these in the mapping. WATCH THIS 3-14

(define (chapters-in-book book) 
  (chapters-in-book-aux book 1))

(define (chapters-in-book-aux book chapter)
  (if (bible-ref (list book chapter 0))
      (chapters-in-book-aux book (1+ chapter))
      (1- chapter))) ;this makes sense that it doesn't count #f 

;===============================================================================
;===============================================================================
;POETRY formatting and processing
;===============================================================================
;if text is too small for following indexing conditions, maybe set first or third as 0,
;so a text could be made up of just verses. 

;unconventional: for things like Milton maybe map it book/canto, line, sentence. 
;for things like Dante this might be canto, stanza, line. 

(define (string->symbol-list string)
  (if (not (void? (list-string-w/punc string)))
      (map string->symbol (list-string-w/punc string))))

;section-match seems to slow it down dramatically... 
(define (section-match? pattern line)
  "works like match but it takes in strings instead of symbols."
  (match (string->symbol-list pattern) (string->symbol-list line)))

;ideally I'd have a way to know the justification of the text, and if a line is further to the
;right than the others, but for now I'll just measure it by 4 spaces, many poems are already
;spaced over twice, and then another two if indented. 
(define (indented? string)
  (let ((string (string->list string)))
    (if (> (length string) 4)
	(exact-subset? (string->list "    ") (sublist string 0 4))
	#f)))

(define (indent string)
  (let ((lst (string->list string)))
    (list->string (append (string->list "    ") lst))))
    
;technically this only checks justified from the left, won't tell you if its right justified,
;but that's not really necessary for english. 
(define (justified? string)
  "this is another way to mark the change of a section or segment depending on the text."
  (let ((string (string->list string)))
    (if (> (elements-in-row-count #\space (string->list string)))
	#t
	#f)))

;write a function that justifies text. 

;use all-caps? as another method of checking a section. another possible candidate would be number? 
;for plays though that would also denote segment markers since character names are in all-caps. 

;the sentence isn't a fundamental unit of poetry the way it is for prose, maybe why
;I'm less comfortable with it. It'd be nice to also have access them. 
;let them specify how many indexes they want? faerie queene should be 4 indexes, maybe dante too. 
;some things aren't returned between paragraphs, but feature an indentation. Account for this somehow
;sometimes it'll dwell on one line for a long time it seems. 
;also have the user add a segment-marker-procedure such as newline? or indented?
;it seems that segment is incrementing at the wrong times. seems it sometimes increments without passing segment-procedure. odd. 
;I'm not happy with the way sections are always mapped, they should be alone at (x 0 0), but for PR
;it puts it in with the whole paragraph. OH! it's cause the first paragraph following isn't indented.
;argh. make it a special case, or just fix the four places it appears. will I encounter it again? 
;alternatively I could say that a section marker is its own segment! 
(define (map-poetry name filename section-marker segment-procedure map-segment?)
  (let ((table (make-table))
	(section 0) ;book/canto
	(segment 0)
	(line-number 0)) ;stanza/paragraph. these follow an empty line. 
    (fold-lines-in-file
     filename
     (lambda (line line-num)
      ;(println section-marker " " (section-match? section-marker (trim-returns (trim-spaces line))))
      ;(println section " " segment " " line-number (segment-procedure line))
       (if (section-match? section-marker (trim-returns (trim-spaces line))) 
	   (begin
	     (set! section (1+ section))
	     (set! segment 0)
	     (set! line-number 0)))
       (if (segment-procedure line) ;something is going wrong with this for indentation. 
	   (begin
	     (set! segment (1+ segment)) 
	     (set! line-number 0)
	     (if (and map-segment? (not (empty? (cleanup-string line)))) 
		 (begin 
		   (table-set! table (list section segment line-number) 
			       (cleanup-string line))
		   (set! line-number (1+ line-number))))) 
	   (if (not (empty? (cleanup-string line)))
	       (begin 
		 (table-set! table (list section segment line-number) (cleanup-string line))
		 (set! line-number (1+ line-number))
		 (if (and (section-match? section-marker (trim-returns (trim-spaces line))) map-segment?) ;hmm, this is a pretty hacky solution. might only be sustainable if segment-procedure and map-segment always correspond to (newline? #f) and (indented? #t)
;this is so section marker is mapped to its own segment, skips too far though on newline? 
		     (begin (set! segment (1+ segment))
			    (set! line-number 0))))))
       (println section " " segment " " line-number (segment-procedure line) " " line)
       (1+ line-num)) 1)
    (if (path-opened? name)
	(set! *open-paths* (cons (cons name table) (delete-assoc name *open-paths*))) ;use replace here? 
	(set! *open-paths* (cons (cons name table) *open-paths*)))))
;on incrementing segment: ;does segment reset line-number? if it does you should still be able to access things by lline number. this method is less useful for say Dante, because each segment is always 6 lines, so its more desireable to get things by line number. 

;===============================================================================================
;===============================================================================================
;PLAY formatting
;===============================================================================================

;(section-match? "ACT ?x Scene ?y" line) ;acts may sometimes be followed by a description of
;of where they are taking place, and maybe some introductory stage direction. 
;stage directions are justified. these may in the middle of a line, so how to index that? 

;===============================================================================================
;===============================================================================================
;PROSE formatting and processing
;===============================================================================================

(define *prose-section-markers* '("CHAPTER" "BOOK" "EPILOGUE" "PROLOGUE" "PRELUDE"))

(define (section? string identifier)
  (string-begins? string identifier)) ;this might be a problem: I can imagine sections not beginning with identifier, especially if its endented or something like that. 
;  (exact-subset? (string->list identifier) (string->list string)))

;give this a section identifier or something so it knows what to look for. 
(define (prose-section? string identifier)
  (and (section? string identifier) ;and each other section marker isn't there. 
       (not (member #t (map (lambda (marker) (exact-subset? (string->list marker) (string->list string))) ;how to break down list
	    (delete identifier *prose-section-markers*))))))
;this still has problems of filtering out things that might contain CHAPTER, but is not a marker. maybe it has to begin
;with chapter. also, what about other sections like prelude, epilogue, EXTRACTS in the case of moby-dick. 

;put each line into a paragraph, then concatenate them all. Make sure to remove return values!
;I need to find a way to extract a new line out of a line. possibly characters are \n and \r
;Here's a problem: neseted chapters, or rather chapters that contain other chapters like in
;moby-dick. chapter 31 thinks that there are 18 other chapters cause of all the BOOK 1. stuff.
;chapter marker fails in delete when it's lowercase. why? 
;so pattern matching fails if chapter-markers vary. 
(define (map-prose name filename chapter-marker) ;include a titled? boolean? 
  (let ((table (make-table))
	(chapter 0)
	(paragraphs 0)
	(paragraph '()))
    (fold-lines-in-file 
     filename
     (lambda (line line-number)
;       (println line)
       (if (newline? line)
	   (begin  
	     ;(println chapter " "paragraphs "    " (reverse paragraph)) ; (reverse paragraph))
	     (let ((sentence 0)) ;its 0 indexing right now, which I'm not sure about.  
	       (for-each (lambda (sent) 
			   ;(println chapter " " paragraphs " " sentence)
			   (table-set! table (list chapter paragraphs sentence) sent)
			   (set! sentence (1+ sentence)))
			 (list-sentences (apply string-append (reverse paragraph))))) 
;makes all the lines into one long string, then breaks it up into a list of those sentences. 
	     (if (not (null? paragraph))
		 (set! paragraphs (1+ paragraphs)))
	     (set! paragraph '()))
	   (set! paragraph (cons (string-append (trim-returns line) " ") paragraph)))
       (if (section-match? chapter-marker (trim-returns (trim-spaces (nth-sentence 0 line))))
	   (begin 
	     (set! chapter (1+ chapter))
	     (set! paragraphs 0))) ;also reset the paragraph marker here. 
       (1+ line-number)) 1)
    (if (path-opened? name)
	(set! *open-paths* (cons (cons name table) (delete-assoc name *open-paths*))) ;use replace here? 
	(set! *open-paths* (cons (cons name table) *open-paths*)))))

;map things by chapter, paragraph, sentence. this parallels bible mapping. 

;chapters are formatted differently in some texts. Ulysses uses -- I --
;most use CHAPTER x which is easy. So maybe I need use converting to roman numerals
;so that I can use the proper chapter key. 

;since the paragraph is the unit rather than the line, I need a way to count
;them. For justified text I'll count the number of characters in each line, and
;whenever a break occurs in a line with less than that length, then it's a paragraph.
;the other way texts are formatted is with line breaks between paragraphs. 

;once you have a paragraph you can break it up by sentences. for dialogue
;I'll need to look right before the final quotation mark. 

;TEXT RETRIEVAL, this applies for all texts now. Maybe adapt them so it can take in a table, or a file-name and file-access it. 

(define (get-third text first second third)
  (text-ref text first second third))

(define (get-third-seq text first second start end)
  (reverse (get-third-seq-aux '() text first second start end)))

;include an error message if sequence goes over the end. 
(define (get-third-seq-aux lst text first second at end)
  (if (equal? at end)
      (cons (get-third text first second at) lst)
      (get-third-seq-aux (cons (get-third text first second at) lst) text
			 first second (1+ at) end)))

;might be some problems here if second doesn't exist it seems. 
(define (get-second text first second)
  (get-third-seq text first second 0 (1- (thirds-in-second text first second))))

(define (get-second-seq text first start end)
  (reverse (get-second-seq-aux '() text first start end)))

(define (get-second-seq-aux lst text first at end)
  (if (equal? at end)
      (cons (get-second text first at) lst)
      (get-second-seq-aux (cons (get-second text first at) lst) text first (1+ at) end)))

;getting some voids here! investigate why!
(define (get-first text first)
  (get-first-aux text first 0))

(define (get-first-aux text first paragraphs)
  (get-second-seq text first 0 (seconds-in-first text first)))

(define (first-count text)
  "returns how many unique first indexes there are."
  (first-count-aux text 1)) ;I can't start it at 0. maybe I should fix this. 

;make sure this is right. might be off by 1 since count starts there... 
(define (first-count-aux text count)
  (if (text-ref text count 0 0)
      (first-count-aux text (1+ count))
      count))

(define (seconds-in-first text first) 
  (seconds-in-first-aux text first 0))

(define (seconds-in-first-aux text first count)
  (if (text-ref text first count 0)
      (seconds-in-first-aux text first (1+ count))
      (1- count))) ;this is so it doesn't count CHAPTER as a pargraph. 

(define (thirds-in-second text first second)
  "returns how many entries there are under the second index." 
  (thirds-in-second-aux text first second 0))

(define (thirds-in-second-aux text first second count)
  (if (and (text-ref text first second count) (not (void? (text-ref text first second count))))
      (thirds-in-second-aux text first second (1+ count))
      count))

;write something that loops through the whole file and performs procedures on each entry. 
;very un lispy to use so many loops. since I can get firsts maybe just loop through that, then
;for each list in first, apply proc to it, actually use for-each instead. 
(define (text-loop text proc)
  (let ((firsts (first-count text)))
    (let loop-firsts ((i 0))
      (let loop-seconds ((j 0))
	(let ((seconds (seconds-in-first text i)))
	  (let loop-thirds ((k 0))
	    (let ((thirds (thirds-in-second text i j)))
	      (apply proc (list (get-third text i j k))) ;am I sometimes getting #f because i,j,or k are out of bounds? 
	      (if (< k thirds)
		  (loop-thirds (1+ k)))))
	  (if (< j seconds)
	      (loop-seconds (1+ j)))))
      (if (< i firsts)
	  (loop-firsts (1+ i))))))

(define (append-paragraph-list lst)
  "appends all the sentences in a pargraph with an extra space between them, and trims the last
one off the end."
  (trim-string (apply string-append 
		      (map (lambda (str) (string-append str " ")) lst))))

;need to concatenate " " at the end of each sentence. 
(define (print-first text first)
  (for-each (lambda (x) (println (indent (append-paragraph-list x))))
	    (get-first text first)))

;===============================================================================
;===============================================================================
;WRITING to files
;===============================================================================
;(define (write-to-file filename)
 ; (with-output-to-file filename
   
;COPY a file, read from one file, and for each line write it into a new file. 
;(define (copy-text text copy-to)
 ; '())

;experiment with writing table->list to a file, then reading that list as a whole
;and converting it to a table. will need a read function that takes in a whole file, 
;not just a single line. 

;===============================================================================
;===============================================================================
;READING from files
;===============================================================================

;can I put utf here? 
(define (fold-lines-in-file filename proc init . mode)
  (with-input-from-file
   filename
   (lambda () (apply fold-lines proc init (current-input-port) mode))))

;why can't accum ever be a list??? 
(define (fold-lines proc init . port+mode)
  (let while ((accum init))
    (let ((line (apply read-line port+mode)))
      (if (eof-object? line) accum
          (while (proc line accum))))))

;========================================
;MY READING FUNCTIONS
;========================================

;trying to mimic the fold-lines-in-file. change it to init-book, init-chap, init-verse
(define (read-bible-indexed proc init . mode)
  (with-input-from-file *bible*
    (lambda () (apply index-bible proc init (current-input-port) mode))))

;seems like the while statement can't take multiple arguments. 
(define (index-bible proc init . port+mode)
  (let while ((book init)
	      (chapter init)
	      (verse init))
    (let ((line (apply read-line port+mode)))
      (if (not (eof-object? line))
          (while (proc line book chapter verse))))))

;so proc can be multiple things, like print and increment the count. 

(define (read-lines-from-file filename)
  (with-input-from-file filename
    (lambda () (read-lines (current-input-port)))))

(define (read-lines mode)
  (let ((line (read-line mode)))
    (if (not (eof-object? line)) ;gack, change this to when. 	
	(begin
	  (println line)
	  (read-lines mode)))))

(define (read-bible-with-index)
  (with-input-from-file *bible*
    (lambda () (index-lines '(0 0 0) (current-input-port)))))

;how to I access this index when I map it? 
(define (index-lines index mode)
  (let ((line (read-line mode)))
    (if (not (eof-object? line)) ;gack, change this to when. 	
	(begin
	  (cond ((book? line) (list-set! index 0 (1+ (car index)))
		 (list-set! index 1 0);resets chapter when book changes. 
		 (list-set! index 2 0)) ;resets verse when book changes. 
		((or (chapter? line) (psalm? line)) 
		 (list-set! index 1 (1+ (cadr index)))
		 (list-set! index 2 0))) ;resets verse when chapter changes.
	  (if (and (not (book? line)) (not (chapter? line)))
	      (list-set! index 2 (1+ (caddr index))))
	  (index-lines index mode))))) 

;===============================================================================
;===============================================================================
;DICTIONARY
;===============================================================================
;Since I've got access to all of C I should really take advantage of the more 
;intricate features of word-net. It'd be perfect for xanadu!

;given that Word Net definitions are sometimes inaccurate, and almost always sparse, it
;might be useful to process a dictionary from gutenberg and incorporate definitions from 
;that into wordnet. 

(define *alphabet* (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))

;this can be used as another way to check section markers since they are very often in all-caps. 
(define (all-caps? string)
  (if (member #f (map (lambda (x) (if (member x (sublist *alphabet* 0 26)) #t #f)) 
		  (string->list string)))
      #f
      #t))

(define (access-file name)
  (if (assoc name *open-paths*)
      (cdr (assoc name *open-paths*))
      #f))

(define *word-types* '(("n" . NOUN:) ("v" . VERB:) ("a" . ADJECTIVE:) ("r" . ADVERB: )))

(define (get-word entry)
  (nth-word 0 entry))

(define (word-type entry)
  (cdr (assoc (nth-word 1 entry) *word-types*)))
  
(define (find-word word)
  (let ((dict (access-file 'dictionary)))
    (find-word-aux word dict 1 (table-length dict))))


;pretty slow and horrible. use a divide and conquer approach. divide based on letter? 
(define (find-word-aux word dict start length)
  (if (eq? start length)
      #f
      (if (equal? word (get-word (table-ref dict start)))
	  (table-ref dict start)
	  (find-word-aux word dict (1+ start) length))))
    
;===============================================================================
;===============================================================================
;PARSING/NLP
;===============================================================================

;parse a sentence, then loop up each word. I guess I don't actually need to parse
;the sentence, but it might be more efficient that way if I only want to look up
;certain words like a verb or proper noun. 



;===============================================================================
;===============================================================================
;USER
;===============================================================================
;write a work-with-file function which lets you do things like get-third-seq and
;such without having to specify a file. you can switch at anytime between which
;text you're currently working with from a list of loaded files. if there's a 
;connection between two files (either loaded or not, depending on the user's 
;preference), the file will say it's pointing to '(1 1 1) in the Bible. if you
;swich to that index, then the file you're working in is the Bible. 

;===============================================================================
;===============================================================================
;INITIALIZE
;show paths, load the dicionary and any other foundational texts. 
;===============================================================================

(fold-lines-in-file 
 "~/Projects/paths.txt" 
 (lambda (line line-num) 
   (println line-num "     " line) (1+ line-num)) 1)

(define *dictionary* 
  (load-path 'dictionary "~/Projects/DICTIONARIES/WordNet-3.0/dict/word-list.txt"))

;Map the paths here
(map-paths)
