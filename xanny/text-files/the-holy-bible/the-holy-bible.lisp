;;THE HOLY BIBLE
(ql:quickload "lispbuilder-sdl")

;the general methods I use will work for anything that is line based, but won't for prose works. 
;Write a separate thing for that, maybe something that treats each paragraph as a unit, as the number of lines
;in those works is arbitrary since lines don't have a set length. 

;adapt some rules here for general use. Example, when mapping the text, its important to keep marks of what books
;it in, what canto/actor's-line, what verse/line, and to map those so that canto1 will be at inferno-1-0. 
;will need to do this with shakespeare plays by book, dialogue, description?, line. 

;maybe use xml instead of just a regular txt file duh. Nevermind, most xml files are way grosser than 
;<book><start-chapter1><v1>.....<v1>......<end-chapter1>.....<end-book1> maybe I should add these to text documents? 
    
;TAGGING SYSTEM: do I tag each individual verse? Yes, but also tag sections?
;maybe make it so that if adjacent verses share a certain number of tags they're
;grouped together. This might group together too often, so maybe they have to
;share a majority of tags, or perhaps all tags to truly be groupped together.
;make tags :tag

;============================================================================================================
;MACROS and PRECURSOR functions
;============================================================================================================
(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element c)))
                 (symbol-name sym)))

(defmacro iterate-over-text (text body)
  `(with-open-file (stream ,text)
     (loop for line = (read-line txt nil :eof)
           until (eq line :eof) do
           ,@body)))
            
(defun number? (x)
  (equal 'fixnum (type-of x)))

(defun mashup-symbol (&rest objects)
  (intern (format nil "~{~a~}" objects)))

(defun mashup-list (lst)
  (let ((str nil))
    (dolist (i lst)
      (if (not (null i))
          (concatenate 'string str (symbol-name i))))
    (intern str)))
 
;LOOPS

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro for ((var start stop) &body body)
  `(do ((b #'(lambda (,var) ,@body))
        (count ,start (if (> ,start ,stop)
                          (1- count)
                        (1+ count))) 
        (limit ,stop))
       ((if (> ,start limit)                ;GACK
            (equal count (1- limit))
          (equal count (1+ limit)))) 
     (funcall b count))) 

(defun spacep (char)
  (or (equal char #\Space)
      (equal char " ")))


;incorporate mapreduce so that I can get a list of unique words that appear and create a concordance, thats the first step. 
;will need to know what book chapter and verse the word appears in. this means I need a way to navigate by those criteria. 

;Should I convert the text into symbols so that I can deal with them more easily? or should I get experience w with text?
;if I do symbolize them then I need to preserve punctuation. 

;allow the user to sort the text by author, by date, etcetera. 

;create a way to write a note associated with a verse, or set of verses, or even cross referenced between different
;chapters. 

;need a way to have a certain element in a hash table, contain a pointer to another element, possibly in a 
;different hash-table. 

;============================================================================================================
;GLOBAL vars
;============================================================================================================

;I know you usually need to open and close a file, but in this case, wouldn't it be easier to just open the file
;and set it to a variable and not worry about closing it until the user terminates their session loop? 

;create a parameter that has the current translation and displays that. it'll be neat to cross reference translations. 
;Take the contents and put it all in a hashmap or an array. 

;hash with buckets to store multiple translations at a single index, and write another fucntion to determine translation. 
(defparameter *bible-map* (make-hash-table)) 


(defparameter *kjv* "~/Projects/xanadu/the-holy-bible/KJV-text-only.txt")
(defparameter *old-testament "~Projects/xanadu/the-holy-bible/KJV-old-testament.txt")
(defparameter *new-testament "~Projects/xanadu/the-holy-bible/KJV-new-testament.txt")

;how can I format this over to the right instead of the left. reverse duh. This isn't working well
;with map-bible though :(, and it really slows it down just printing genesis, gasp!
(defparameter *tanakh* "Projects/xanadu/the-holy-bible/TANAKH/GENESIS.txt")
 
(defparameter *number-of-books* 66) 

(defparameter *punctuation-list* '(#\# #\[ #\] #\Return))


;this can be rewritten with everything in the string prophets. code will be much shorter. 
(defun check-book (line)
  (setf prophets "HOSEA JOEL AMOS OBADIAH JONAH MICAH NAHUM HABAKKUK ZEPHANIAH HAGGAI ZECHARIAH MALACHI EZRA")
  ;ezra isn't a prophet, but is a single name in the KJV text. 
  (cond ((and (search "BOOK OF" line) (not (search "COMMONLY CALLED" line)) 
              (not (search "OTHERWISE CALLED" line))) t)
        ((search "ECCLESIASTES" line) t)
        ((search "SONG OF SOLOMON" line) t)
        ((search "THE PROVERBS" line) t)
        ((search "THE LAMENTATIONS OF" line) t)
        ((search "THE GOSPEL ACCORDING TO" line) t)
        ((search "THE ACTS OF THE APOSTLES" line) t)
        ((search "EPISTLE OF" line) t)
        ((search "EPISTLE GENERAL OF" line) t)
        ((search "THE REVELATION OF" line) t)
        ((search line prophets) t) ;this relies on the title of a book being in all caps, and only the prophets names
        (t nil)))

;gack. change this so it just takes 3 numbers, no a list, that'll be much cleaner in the other code. 
(defun convert-index (lst)
  (let ((lst1 nil))
    (dolist (i lst)
      (push (write-to-string i) lst1))
    (let ((str ""))
      (dolist (i (reverse lst1)) 
        (setf str (concatenate 'string str i "-")))
      (read-from-string (subseq str 0 (1- (length str))))))) 

;rewrite this so the key for something is its book, chapter, verse, but as a number, so 1-1-3, no genesis-1-3
;in sbcl for some reason this gives me an end of line error and won't map past 1-1-31 
;A better way to do this might be to put everything in a bible map, and then into book maps, chapter maps. 
(defun map-bible (path)
  (let* ((book 0)
         (chapter 0)
         (verse 0))
    (with-open-file (stream path :external-format :utf-8)
      (loop for line = (read-line stream nil :eof) ;include a utf thing here?
	 until (eq line :eof) do
	   (let ((ln (delete-if #'(lambda (x) (member x *punctuation-list*)) line)))
	     (when ;(and 
		 (not (equal ln "")) 
;(or (search "Psalm" ln) (search "chapter" ln) (not (search "xxxx" ln)))) ;is this uncessesary?
	       (let ((name (list book chapter verse)))
		 (cond (
			(or (equal 'chapter (read-from-string ln)) (equal 'psalm (read-from-string ln)))
			(incf chapter) 
			(setf verse 0)
			(setf name (list book chapter verse)) ;add chapter name here. should be book-chapter-0
			   (setf (gethash (convert-index name) *bible-map*) ln))
		       ((check-book ln) ;this isn't picking up the minor prophets. 
			(incf book)
			(setf chapter 0)
			(setf verse 0)
			(setf name (list book chapter verse)) ;add book name here. book-0-0
			(setf (gethash (convert-index name) *bible-map*) ln))
		       ((numberp (read-from-string ln)) 
			(incf verse) 
			(setf name (list book chapter verse))
			(setf (gethash (convert-index name) *bible-map*) ln))))))))))


					;(map-bible) 

(defun build-contents ()
  (dotimes (i 66)
    (print (get-verse (1+ i) 0 0)))) 
       
;this will go rename each book in the hash-map so that Genesis is just Genesis, not the first book of moses. 
(defun fix-book-name (book)
) 

(defun get-book-name (book)
  (gethash (convert-index (list book 0 0)) *bible-map*))

(defun print-book-name (book)
  (princ (get-book-name book))
  (fresh-line))

(defun get-chapter-num (book chapter)
  (gethash (convert-index (list book chapter 0)) *bible-map*))

(defun print-chapter-num (book chapter)
  (princ (get-chapter-num book chapter))
  (fresh-line))

(defun print-book (book)
  (let ((chap 1))
    (print-book-name book)
    (while (gethash (convert-index (list book chap 0)) *bible-map*)
      (print-chapter book chap)
      (incf chap))))

(defun print-chapter (book chapter)
  (let ((v 1))
    (print-book-name book)
    (print-chapter-num book chapter)
    (while (gethash (convert-index (list book chapter v)) *bible-map*)
      (print-verse book chapter v)
      (incf v))))  

;this is gross, and I think all long sequences like this are, because it doesn't keep the cursor at the top
;of the next. Maybe there's a way I can have it do an automatic C-l on the first line? 
(defun print-chapter-sequence (book start end)
  (print-book-name book)
  (for (chapter start end)
    (let ((v 1))
      (format t "~%")
      (print-chapter-num book chapter)
      (while (gethash (convert-index (list book chapter v)) *bible-map*)
	(print-verse book chapter v)
	(incf v)))))  
           

(defun get-verse (book chapter verse)
  (gethash (convert-index (list book chapter verse)) *bible-map*))

(defun print-verse (book chapter verse)
  (princ (get-verse book chapter verse))
  (fresh-line)) 

;will this print them out, or put them all in a list for another function to print? 
;if you exceed the limit it'll just keep printing out nils, so incorporate a check so that it'll stop at the last. 
;maybe write another version where it'll spill over into the next chapter, so you could say: give me the first
;666 verses of the bible, starting with genesis. In this version book and chapter will be more like a starting marker
;than a boundary. 
(defun print-verses (book chapter start stop) ;write a separate one to gather a list of them? 
  (for (i start stop)
       (princ (get-verse book chapter i))
       (fresh-line)))  



(defun compare-verses (book1 chapter1 verse1 
                             book2 chapter2 verse2)
  (print-book-name book1)
  (print-chapter-num book1 chapter1)
  (print-verse book1 chapter1 verse1) 
  (format t "~%") 
  (print-book-name book2)
  (print-chapter-num book2 chapter2)
  (print-verse book2 chapter2 verse2))

(defun compare-sequences (book1 chapter1 start1 end1
                                book2 chapter2 start2 end2)
  (print-book-name book1)
  (print-chapter-num book1 chapter1)
  (print-verses book1 chapter1 start1 end1)
  (format t "~%")
  (print-book-name book2)
  (print-chapter-num book2 chapter2)
  (print-verses book2 chapter2 start2 end2)) 

(defun compare-chapters (book1 chapter1
                               book2 chapter2)
  (print-chapter book1 chapter1)
  (print-chapter book2 chapter2))

            
(defun get-lines ()
  (with-open-file (stream *unwavering-path*)
    (count-lines1 stream)))

(defun count-lines (txt)
    (let ((cnt 0))
      (loop for line = (read-line txt nil :eof)
            until (eq line :eof) do
            (incf cnt))
      cnt))

;(defparameter *lines-total* (count-lines)) 

(defun in-the-beginning ()
  (with-open-file (stream *unwavering-path*)
    (format t "~a~%" (read-line stream))))


(defun table-of-contents ()
  (with-open-file (stream *contents*)
    (loop for line = (read-line stream nil :eof)
          until (eq line :eof) do
          (format t "~a~%" line))))

(defun whole ()
  (with-open-file (stream *unwavering-path*)
    (loop for line = (read-line stream nil :eof)
          until (eq line :eof) do
          (format t "~a~%" line)))) 


(defun get-verse-nums ()
  (let ((lst nil))
    (with-open-file (stream *unwavering-path*)
      (loop for line = (read-line stream nil :eof)
            until (eq line :eof) do
            (let ((ln (delete-if #'(lambda (x) (member x *punctuation-list*)) line)))
              (when (not (equal line ""))
                (if  (equal 'fixnum (type-of (read-from-string ln)))
                    (push (read-from-string ln) lst)
                  (push ln lst))))))
    (reverse lst))) 

;this will combine "chapter 1" with the verses until the next chapter is encountered. 
(defun group-verses ()
  (let ((contents nil)
        (book nil)
        (chapter nil))
    (dolist (i (get-verse-nums))
      (when (and (stringp i) (not (equal 'chapter (read-from-string i))))
        (when (not (null book))
          (push (reverse book) contents)
          (setf book nil))
        (push i book))
      (when (and (stringp i) (equal 'chapter (read-from-string i)))
        (when (not (null chapter))
          (push (reverse chapter) book)
          (setf chapter nil))
        (push i chapter))
      (when (not (stringp i))
        (push i chapter)))
    (reverse contents)))

    
;maybe store this info in the table so its quick to lookup. 
(defun num-books ()
  (let ((count 1))
    (while (not (null (get-verse count 0 0)))
      (incf count))
    (1- count)))

(defun num-chaps-book (book)
  (let ((count 1))
    (while (not (null (get-verse book count 0)))
      (incf count))
    (1- count)))

(defun num-chaps ()
  (let ((count 1))
    (dotimes (book (num-books))
      (setf count (+ count (num-chaps-book (1+ book)))))
    (1- count)))

(defun num-verses-chap (book chap)
  (let ((count 1))
    (while (not (null (get-verse book chap count)))
      (incf count))
    (1- count)))

(defun num-verses-book (book)
  (let ((count 1))
    (dotimes (chap (num-chaps-book book))
      (setf count (+ count (num-verses-chap book (1+ chap)))))
    (1- count)))

;off by 1 because 3 John has 14 or 15, KJV uses 14 apparently. 
(defun num-verses ()
  (let ((count 1))
    (dotimes (book (num-books))
      (setf count (+ count (num-verses-book (1+ book)))))
    (1- count)))

;ALSO HORRIBLE IT SEEMS
(defun find-first (phrase) 
  (let ((results nil))
    (block loop 
    (dotimes (book (num-books))
      (dotimes (chap (num-chaps-book (1+ book)))
	(dotimes (vers (num-verses-chap (1+ book) (1+ chap)))
	  (when (search (string-downcase phrase) 
			(string-downcase (get-verse (1+ book) (1+ chap) (1+ vers))))
	    (return-from loop (get-verse (1+ book) (1+ chap) (1+ vers))))))))))
 

;how do I let book, chap and vers be referenced by body? 
(defmacro bible-loop1 (&body body)
  `(dotimes (book (num-books))
    (dotimes (chap (num-chaps-book (1+ ,book)))
      (dotimes (vers (num-verses-chap (1+ ,book) (1+ ,chap)))
	,@body))))

;a different version hacked out. 
(defmacro bible-loop (book chap vers &body body)
  `(for (bk ,book (num-books))
     (for (chp ,chap (num-chaps-book ,bk))
       (for (vrs ,vers (num-verses-chap ,bk ,chp))
	 (setf ,book ,bk) (setf ,chap ,chp) (setf ,vers ,vrs)
	 ,@body))))

;(defun loop-bible (&body body)
;  (bible-loop 1 1 1 body))


(defun print-all ()
  (bible-loop (print-verse (1+ book) (1+ chap) (1+ vers))))

;create a macro to loop through the bible. book, chap, and vers would need to be bound and referencable. 
(defun search-phrase (phrase)
  "returns a list containing the book and chapter # followed by a string 
of the verse."
  (let ((results nil))
    (dotimes (book (num-books))
      (dotimes (chap (num-chaps-book (1+ book)))
	(dotimes (vers (num-verses-chap (1+ book) (1+ chap)))
	  (when (search (string-downcase phrase) 
			(string-downcase (get-verse (1+ book) (1+ chap) (1+ vers))))
	    (push (list (1+ book) (1+ chap) (1+ vers)
			(get-verse (1+ book) (1+ chap) (1+ vers))) results)))))
    (reverse results)))

;create a search phrase that orders it by relevance rather than first similar result found. 


;create something that'll print book and chapter for search results
;actually have it organize them by book and chapter, then create another to print
;a third function should let the user read one at a time, and then select the next result
;where it'll tell you if 
    
(defun print-search (lst)
  (print-book-name (car lst))
  (print-chapter-num (car lst) (second lst))
  (print-verse (car lst) (second lst) (third lst))) ;this contains the verse number already, at least in KJV version. 
	      
(defun print-whole-search (results)
  (let ((book nil)
	(chapter nil))
    (dolist (verse results)
      (when-cond ((not (equal book (car verse)))
		  (setf book (car verse))
		  (format t "~% ~%")
		  (print-book-name (car verse)))
		 ((not (equal chapter (second verse)))
		  (setf chapter (second verse))
		  (format t "~%")
		  (print-chapter-num book (second verse))))
      (print-verse (car verse) (second verse) (third verse)))))

;GACK, just reuses the code above. 
(defun print-one-by-one (results) 
  (let ((book nil)
	(chapter nil)
	(remaining (length results)))
    (dolist (verse results)
      (when-cond ((not (equal book (car verse)))
		  (setf book (car verse))
		  (format t "~% ~%")
		  (print-book-name (car verse)))
		 ((not (equal chapter (second verse)))
		  (setf chapter (second verse))
		  (format t "~%")
		  (print-chapter-num book (second verse))))
      (print-verse (car verse) (second verse) (third verse))
      (decf remaining)
      ;find a way to wipe this, or maybe just display it at beginning, only displaying the count
      ;at every iteration. That'd make it more readable. 
      (format t "~%press return for next result (~S remaining), q to quit: " remaining) 
      ;allow user to input a number to skip to. 
      (let ((choice (read-line)))
	(if (equal choice "q")
	    (return nil))))))

      
;CREATE A MAPREDUCE SYSTEM AND COLLECT ALL WORDS AND FIND OCCURENCES. 
;THEN I can crossreference these with the dictionary files. 
 
;============================================================================================================
;UTILITIES
;============================================================================================================

;inefficient cause of the reverse... 
(defun range-list (n)
  (let ((lst nil))
    (dotimes (i n)
      (push i lst))
    (reverse lst)))

;can't really figure out how to make this efficient. 
(defun range-vector (n)
  )

(defun num->char (x)
  (coerce (write-to-string x) 'character))

(defun numerical-char? (x)
  (let ((nums (mapcar #'num->char (range 10))))
    (if (member x nums)
	t
	nil)))

;THESE THREE WORK ON LISTS, specifically lists of chars. 

;write a function to clean up text, removing #, and brackets around words (why are they there in the bible?)
;does this work for more than a character? 
(defun remove-markups (line)
  (delete-if #'(lambda (x) (member x *punctuation-list*)) line))

;I like the format the way it is, but this might be nice to use. 
(defun remove-verse-num (line)
  (cdr (cdr line)))
         
(defun remove-spaces (line)
  (delete-if #'(lambda (x) (equal x #\Space)) line))


;GRAPHICS/UI

;so this might be GACK, but I could just clear the text whenever the user moves the screen
;DO I have to wipe the whole screen, and reprint it? that's horribly inefficient. 
;get text to wrap around. 
(defun make-window (book chapter line-start line-stop)
  (sdl:with-init ()
    (sdl:window 800 640 :title-caption "bible")
    (setf (sdl:frame-rate) 60)
    (setf font (sdl:initialise-default-font))
    (setf line-pos-y 10)
    (setf line-pos-x 10)

    ;wrap around after 100. 
    (for (pos line-start line-stop)
      (let ((verse (get-verse book chapter pos)))
	(sdl:draw-string-solid-* verse line-pos-x
				 line-pos-y :color sdl:*white* :font font)
	(incf line-pos-y 10)))
    (setf line-pos-y 10)
    (sdl:with-events ()
		     (:quit-event () t)
		     (:key-down-event ()
				      (when (sdl:key-pressed-p :sdl-key-escape)
					(sdl:push-quit-event)))
				      ;WRONG. these will need to clear the screen and reset
				      ;the marker I think. use when-cond. 
				    
		     (:idle ()
			    ;change this to a while that says as long as pos-y < y-limit. 			   
			    (sdl:update-display)))))


(map-bible *kjv*)


