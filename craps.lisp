;;;
;;; EECS 5740 - Artificial Intelligence (Spring 2013)
;;; Lisp Assignment #2: CRAPS
;;; Christopher J. Volny
;;;
;;; Description:
;;; ================================
;;; Plays a game of craps by American Casino Rules:
;;;  - First Roll Wins: 7, 11
;;;  - First Roll Loses: 2, 3, 12
;;;  - Subsequent Roll Wins: roll value equals point
;;;  - Subsequent Roll Loses: 7
;;;
;;; Loading, Compiling, and Running:
;;; ================================
;;; > (compile-file "craps.lisp")
;;; > (load "craps")
;;; > (play-game)
;;;

(setf *random-state* (make-random-state t))

(defun throw-die ()
"random die throw [1,6]"
	(+ 1 (random 6)))

(defun throw-dice ()
"random 2 die throw using `throw-die`"
	(list (throw-die) (throw-die)))

(defun snake-eyes-p (throw)
"predicate for snakeeyes named roll"
	(equal throw '(1 1)))

(defun boxcars-p (throw)
"predicate for boxcars named roll"
	(equal throw '(6 6)))

(defun get-roll-value (throw)
"determine dice summed value"
	(apply '+ throw))

(defun instant-win-p (throw)
"predicate for instant win states for first roll"
	(> (length (intersection (list (get-roll-value throw)) '(7 11))) 0))

(defun instant-loss-p (throw)
"predicate for instant loss states for first roll"
	(> (length (intersection (list (get-roll-value throw)) '(2 3 12))) 0))

(defun say-throw (throw)
"Speak the roll results including named rolls"
	(cond ((snake-eyes-p throw) (string "snake-eyes"))
		((boxcars-p throw) (string "boxcars"))
		(t (get-roll-value throw))))

(defun craps ()
"Start craps game with first roll and print result. Win on 7 or 11; lose
on 2, 3, or 12; and display point for all others.
Include sets for *point* to work with `play-game`.
Utilizes `replace-all` function to remove extraneous quotes from output."
	(let* ((throw (throw-dice))
		(said (say-throw throw))
		(result (cond ((instant-win-p throw) (string "YOU WIN"))
				((instant-loss-p throw) (string "YOU LOSE"))
				(t (setq *point* (get-roll-value throw)) 
					(format nil "YOUR POINT IS ~D" *point*)))))
		(replace-all (format nil "THROW ~D AND ~D -- ~S -- ~S" 
			(car throw)
			(car (cdr throw))
			said
			result
			) "\"" "")))

(defun try-for-point (n)
"Attempt subsequent rolls for the point value n; win on n, lose on 7,
repeat for all others.
Include sets for *point* to work with `play-game`.
Utilizes `replace-all` function to remove extraneous quotes from output."
	(let* ((throw (throw-dice))
		(value (get-roll-value throw))
		(result (cond ((= value n) (setq *point* nil) (string "YOU WIN"))
			((= value 7) (setq *point* nil) (string "YOU LOSE"))
			(t (string "TRY AGAIN")))))
		(replace-all (format nil "THROW ~D AND ~D -- ~S -- ~S"
			(car throw)
			(car (cdr throw))
			value
			result
			) "\"" "")))

;; *point* and `play-game` are bonus features for this assignment.
;; I realized very quickly that continually keying/pasting calls to
;; `try-for-point` got annoying and a simple loop until would suffice.

(defparameter *point* nil)

(defun play-game ()
"Bonus: Play a game of craps using `craps` and `try-for-point`."
	(print (craps))
	(loop until (eq *point* nil) do
		(print (try-for-point *point*))))




;; Function `replace-all` from http://cl-cookbook.sourceforge.net/strings.html
;;  This function was necessary to ensure no additional quotes were needed 
;;  when including strings within a format string (and is cleaner than 
;;  using concatenate).

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurrences of the part 
is replaced with replacement."
	(with-output-to-string (out)
		(loop with part-length = (length part)
			for old-pos = 0 then (+ pos part-length)
			for pos = (search part string
								:start2 old-pos
								:test test)
			do (write-string string out
								:start old-pos
								:end (or pos (length string)))
			when pos do (write-string replacement out)
			while pos)))

