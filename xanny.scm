;XANNY

;write my own format function. So I guess it'll take something like 
;"hello ~S" and would create a println with "hello " then substitute the
;variable given for ~S. would also be nice to format spacin for line-nums. 
;(define-macro (printf string . values)
  
;this will need an auxiliary that'll go through the string and find formatting

;brings in some novak utility code like while, dolist, dotimes. 
(load "~/Projects/initdr-novak.scm")

;WHY are all macros so iffy on this? sometimes push works, sometimes it says its an unbound variable.
;dotimes was working earlier, but not anymore... 

(define (nthcdr n lst)
  (list-tail lst n)) 

(define (nth n lst)
  (list-ref lst n))

(define (last lst)
  (car (nthcdr (1- (length lst)) lst)))

(define-macro (push var #!optional val)
  `(set! ,var (cons ,val ,var)))

(define-macro (pop var)
  `(set! ,var (cdr  ,var)))

(define-macro (inc n)
  `(set! n (+ n 1)))

(define-macro (dec n)
  `(set! n (- n 1)))

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
  (if (or (equal? ch #\newline) (equal? ch #\return))
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

(define (expand-contractions word)
  "Takes in a single word and expands its form."
  (string-append  
   (substring word 0 (find-char #\' word)) " "
   (cdr (assoc (splice-word word #\') *contractions*))))

(define (nth-char n string)
  (nth n (string->list string)))

(define (nth-word n string)
  (nth-word-aux n (string->list string) 0 '()))

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
  (trim-spaces-aux (string->list string)))

(define (trim-spaces-aux lst)
  (if (space? (car lst))
      (trim-spaces-aux (cdr lst))
      (if (space? (last lst))
	  (trim-spaces-aux (string->list (trim-string (list->string lst))))
	  (list->string lst))))

;TODO: finish this and generalize get-sentence to get nth-sentence. 
(define (count-sentences string)
  (+ (char-count #\. string) (char-count #\? string)
     (char-count #\! string)))

;make sure to not cut off " or parentheses. find at what position sentence end, 
;convert to list, then nthcdr at that position? 
(define (get-sentence string)
  "returns a substring that starts at the beginning of the string until sentence's end."
  (let ((end (find-punctuation string)))
    (trim-spaces (substring string 0 (1+ end))))) ;1+ since find-char will return at what place that char is, and we want it included. 

(define (nth-sentence n string)
  (nth-sentence-aux n string 0))

(define (nth-sentence-aux n string position)
  (println string)
  (if (eq? n position)
      (get-sentence string)
      (nth-sentence-aux n 
			(list->string (nthcdr (1+ (find-punctuation string)) (string->list string)))
				(1+ position))))

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

(define (find-punctuation word)
  (cond ((find-char #\. word) (find-char #\. word))
	((find-char #\? word) (find-char #\? word))
	((find-char #\! word) (find-char #\! word))))

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
  (if (not (empty? string))
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
		  
(define (char-count ch string)
  "count instances of a char in a string."
  (element-count ch (string->list string)))



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
(define (text-ref text-name index1 index2 index3)
  (table-ref (access-file text-name) (list index1 index2 index3) #f))

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

(define (chapter? line)
  (exact-subset? (string->list "CHAPTER") (string->list line)))

(define (psalm? line)
  (exact-subset? (string->list "PSALM") (string->list line)))

;I tried writing this using an index list instead of book, chap, vers, but it mapped
;everything to the final value of index for some weird reason. 
;way too slow. but I'm not sure why, 'cause fold-lines is recursive?
;I notice it lags quite a bit at a few parts.  
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
;       (println book " " chapter " " verse)
       (cond ((book? line) (set! book (1+ book))
	      (set! chapter 0);resets chapter when book changes. 
	      (set! verse 0)) ;resets verse when book changes. 
	     ((or (chapter? line) (psalm? line)) 
	      (set! chapter (1+ chapter))
	      (set! verse 0))) ;resets verse when chapter changes.
       (if (and (not (book? line)) (not (chapter? line)))
	   (set! verse (1+ verse)))
       (table-set! table (list book chapter verse) line)) 1) 
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
      (1- chapter))) ;this makes sense that it doesn't count #f but does 

;===============================================================================
;===============================================================================
;POETRY formatting and processing
;===============================================================================
;if text is too small for following indexing conditions, maybe set first or third as 0,
;so a text could be made up of just verses. 

;unconventional: for things like Milton maybe map it book/canto, line, sentence. 

;for things like Dante this might be canto, stanza, line. 
;(define (numbers->index lst)
 ; (map number->string lst)

;===============================================================================
;===============================================================================
;PROSE formatting and processing
;===============================================================================

;put each line into a paragraph, then concatenate them all. Make sure to remove return values!
(define (map-prose name filename)
  (let ((table (make-table))
	(chapters 0)
	(paragraphs 0)
	(paragraph '()))
    (fold-lines-in-file 
     filename
     (lambda (line line-number)
       (if (equal? 1 (string-size line))
	   (push lst line)))
       (table-set! table line-number line)
       (if (empty? line)
	   (println line-number line))
       (1+ line-number)) 1)
    (if (path-opened? name)
	(set! *open-paths* (cons (cons name table) (delete-assoc name *open-paths*))) ;use replace here? 
	(set! *open-paths* (cons (cons name table) *open-paths*))))

    



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

;===============================================================================
;===============================================================================
;WRITING to files
;===============================================================================
;(define (write-to-file filename)
 ; (with-output-to-file filename
   

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

(define *alphabet* (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))

(define (access-file name)
  (if (assoc name *open-paths*)
      (cdr (assoc name *open-paths*))
      #f))

(define *word-types* '(("n" . NOUN:) ("v" . VERB:) ("a" . ADJECTIVE:) ("r" . ADVERB: )))

(define (get-word entry)
  (nth-word 0 entry))

(define (word-type entry)
  (cdr (assoc (nth-word 1 entry) *word-types*)))
  
(define (before-in-alphabet? str1 str2)
  (before? (first-char str1) (first-char str2) *alphabet*))

;allow this to take in infinite arguments for a much more useful function. 
(define (lower-letter str1 str2)
  (if (before-in-alphabet? str1 str2)
      str1
      str2))

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
;INITIALIZE
;show paths, load the dicionary and any other foundational texts. 
;===============================================================================

(fold-lines-in-file 
 "~/Projects/paths.txt" 
 (lambda (line line-num) 
   (println line-num "     " line) (1+ line-num)) 1)

(define *dictionary* "~/Projects/DICTIONARIES/WordNet-3.0/dict/word-list.txt")
(load-path 'dictionary *dictionary*) 

;Map the paths here
(map-paths)

;this won't be initialized properyly unless bible is mapped before hand, and that's too inefficient. 
(define *bible* (access-file 'bible))
