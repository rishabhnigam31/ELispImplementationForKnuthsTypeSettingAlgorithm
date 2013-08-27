;;check if a character is num or not
(defun isnum(c) 
  (if (and (< c 58) (> c 47 ))t nil))


;;; This function takes input a string and puts alphabets into list chars and number in the list points.
;;;Convert the a pattern like 'a1bc3d4' into a list of chars (a b c d) and a list of points ( 1 0 3 4 ).
 
(defun extractchar (s) 
  (setq len (length s)) 
  (setq extractchar-i 0) 
  (setq points ()) 
  (setq chars ())
;etractchar-i is a iterator goes from 0 to length of string
  ( while (< extractchar-i len)      
    (cond     
     ((and (= extractchar-i 0) (isnum (elt s 0)))    (push (- (elt s 0) 48) points))
     ((and (= extractchar-i 0) (not (isnum (elt s 0))))   
      (push  0 points) (push (substring s extractchar-i (+ extractchar-i 1)) chars)
      (if(and (< extractchar-i (- len 1)) (isnum (elt s (mod (+ extractchar-i 1) len))))
	  (push (- (elt s (+ extractchar-i 1)) 48) points)           
	(push 0 points)))
;check for a alphabet or a . if found push on list chars
     ((or (= (elt s extractchar-i) 46) (and (> (elt s extractchar-i) 96) (< (elt s extractchar-i) 123) ))       
      (push (substring s extractchar-i (+ extractchar-i 1)) chars)
;check for a number if found push on list points
      (if(and (< extractchar-i (- len 1)) (isnum (elt s (mod (+ extractchar-i 1) len))))
	  (push (- (elt s (+ extractchar-i 1)) 48) points)           
	(push 0 points))))    
    (setq extractchar-i (+ extractchar-i 1)))
  (setq points (reverse points)) 
  (setq chars (reverse chars)))


;;;This function converter a list L in string and save in str
(defun listtostring(L)
  (setq str "")      
  (dolist ( x L)
    (setq str (concat str x)))str)


;;; This is the Binary Search function
;;; It search a specific pattern from sorted array of patterns
;;; If pattern is found it return index of pattern in array
;;; If not found -1 is returned
;;; binary_i and binary_j are the intial and final indexes of array 
(defun binarysearch (pat binary_i binary_j _arr)           
  (setq mid (/ (+ binary_i binary_j) 2))
  (if(< binary_j binary_i)
;binary_a store the result of binarysearch function.      
      (setq binary_a -1)                                   
    (cond     
     ((string< pat (listtostring (extractchar (elt _arr mid ))))   (setq binary_a (binarysearch pat binary_i (- mid 1) _arr)))               
					;extractchar function extract alphabet from a string and listtosring function convert a list into string    
     ((string< (listtostring (extractchar (elt _arr mid ))) pat)   (setq binary_a (binarysearch pat (+ mid 1) binary_j _arr)))             
     ((string= (listtostring (extractchar (elt _arr mid ))) pat)  (setq binary_a mid))))binary_a)


(defun update (updatei updatestr pointsstring)
  (setq uplocali 0)
  (extractchar updatestr)
  (setq updatelen (length points))
  (while(< uplocali updatelen)
    (if (> (elt  points uplocali) (elt pointsstring (+ updatei uplocali)))	
	(aset pointsstring ( + updatei uplocali) (elt points uplocali )))
    (setq uplocali (+ uplocali 1)))pointsstring)



(defun runoverstring(runover-str)
  (setq pointsstring [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  
  (setq runover_i 0)

  (while (< runover_i (length pointsstring))
    (aset pointsstring runover_i 0)
    (setq runover_i (+ 1 runover_i)))

  (setq runoverstring-strlen (length runover-str))
  (setq runoverstring-len 2)
  (setq runoverstring-initial 0)
  (while (< runoverstring-len (+ 1 runoverstring-strlen) )
    (setq runoverstring-initial 0)
    (while (< (+ runoverstring-initial runoverstring-len) (+ runoverstring-strlen 1))
      (cond 
       ((not (= -1 (setq b (binarysearch (substring runover-str runoverstring-initial (+ runoverstring-initial runoverstring-len)) 0 4446 arr ) )))
	(setq pointsstring ( update runoverstring-initial (elt arr b ) pointsstring)))                               
       ((not (= -1 (setq b (binarysearch (substring runover-str runoverstring-initial (+ runoverstring-initial runoverstring-len)) 0 491 arr1 ) )))
	(setq pointsstring ( update runoverstring-initial (elt arr1 b ) pointsstring) )))
      (setq runoverstring-initial ( + runoverstring-initial 1)))
    (setq runoverstring-len ( + runoverstring-len 1)))

  (aset pointsstring 0 0)
  (aset pointsstring 1 0)
  (aset pointsstring 2 0)
  (aset pointsstring runoverstring-strlen 0)
  (aset pointsstring (- runoverstring-strlen 1) 0)
  (aset pointsstring (- runoverstring-strlen 2) 0) pointsstring)


;;; This function takes care of two types of exceptions
;;; If word is less than or equal to 4 letter it doesnot hypenete
;;; There are fourteen exception of frank liang patterns these are given in exception-arr 
;;; This Function also takes carre about those 14 word
(defun exceptions (exceptions-string)
  (setq exceptions-output nil)
  (cond 
   ((< (length exceptions-string) 5)
    (setq exceptions-output exceptions-string)exceptions-output)
   ((> (length exceptions-string ) 4)
    (setq exceptions-i 0)
    (while (< exceptions-i 14)
      (extractchar (elt exception-arr exceptions-i))
      (setq test-string (listtostring chars))
      (if (string= exceptions-string test-string)
	  (progn
	    (setq exceptions-output (elt exception-arr exceptions-i))
	    (setq exceptions-i 14))
	(setq exceptions-i (+ exceptions-i 1)))))) exceptions-output)


;;; This function takes the word to be hyphenate and put "." in the begining and end of word 
;;; Then it check the subscript of each letter of word in the pointstring 
;;; If subscript of letter is odd it hyphenate the word at that point 
(defun hyphenate (hystr)
  (setq outstr "")
  (exceptions hystr)
; If word is exception 
  (if exceptions-output
      (progn
	(setq outstr exceptions-output) outstr)
    (progn
; If word is not exceptio
  (setq myhystr "")
  (setq myhystr (concat myhystr "."))
  (setq myhystr (concat myhystr hystr))
  (setq myhystr (concat myhystr "."))
  (setq hypoint (runoverstring myhystr))
  (setq hyiter 2)
  (while (< hyiter  (length myhystr) )
    (setq outstr (concat outstr ( substring myhystr (- hyiter 1) hyiter)))	
    (if (= (mod (elt hypoint hyiter ) 2) 1)
	(setq outstr (concat outstr "-")))
    (setq hyiter (+ hyiter 1))))) outstr)



