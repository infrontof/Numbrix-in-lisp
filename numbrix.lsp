
(defparameter *size* 0)
;(defparameter myboard nil)
(defparameter *blank-board* nil)
(defparameter *moverecord* nil)
(defparameter *printrecord* nil)
(defparameter *position-types-key* '(:inner :inner :n :n :ne :ne :e :e :se :se :s :s :sw :sw :w :w :nw :nw))
(defparameter *position-types-neighbors*
  '(:inner 4 :n 3 :ne 2 :e 3 :se 2 :s 3 :sw 2 :w 3 :nw 2))
(defvar *debug-mode* nil)
(defparameter *board* nil)
(defparameter *position-types*
'(:inner inner :n n :ne ne :e e :se se :s s :sw sw :w w :nw nw))
(defparameter *boardname-definitions* "boards.lsp")
(defparameter *board-path* "boards")
(defparameter *program-flow* '(:exit exit :continue continue :handle-move handle-move :invalid-move invalid-move))

(eval-when (:compile-toplevel :load-toplevel :execute)
   (defparameter *neighbor-function-types*
     '(:n get-neighbor-n :e get-neighbor-e :s get-neighbor-s :w get-neighbor-w
      :ne get-neighbor-ne :se get-neighbor-se :sw get-neighbor-sw :nw get-neighbor-nw)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *easy-solution-function-types*
    '(:vert easy-solution-vert :horz easy-solution-horz
      :ne easy-solution-ne :se easy-solution-se
      :sw easy-solution-sw :nw easy-solution-nw
      :max easy-solution-max :min easy-solution-min)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *find-adjacency-function-types*
    '(:n find-adjacency-n :e find-adjacency-e
      :s find-adjacency-s :w find-adjacency-w)))

(defparameter *user-help-message* '(" NUMBRIX! "))


 (defun numbrix()
   (dotimes (i 70)
       (format t "~A" "#"))
	(format t "~%~%~30tWelcome to Numbrix!~%~%")
	(dotimes(i 70)
	   (format t "~A" "#"))
	(format t "~%~%About the game:~%")
	(print "1. The objective of this game is to place numbers into the 
	open cells so they make a path in numerical order, 1 through the last number. ")
	(print "2.You can add numbers horizontally or vertically in any 
	direction. Diagonal paths are not allowed. ")	
	(format t "~%")
	(dotimes (i 70)
       (format t "~A" "#"))
	(print "Please select AI mode or Human mode: (a/h)")
	(setf mode (read))
	(cond ((equal mode 'a) (AI-mode))
	      ((equal mode 'h) (human-mode))
		  (t (print "Illegan choice!")(numbrix)))
	(print "Do you want to play numbrix again? (yes/no)");other choice from begin or just this game
	(setf moregame (read))
	(cond ((eql moregame 'yes)(numbrix))
	      (t (progn (print "Thank you for playing, bye!")
		            (return-from numbrix t))))
	   )
(defun human-mode()
     (setq file1 '((1 - 3)(- - -)(7 - 9)))
	 (setq file3 '((1 - 3)(- - -)(7 - 9)))
    (setq file2 '((45 44 39 38 23 22 19 18)(46 - - - - - - 17)(47 - - - - - - 16)(48 - - - - - - 15)(63 - - - - - - 14)(64 - - - - - - 3)(59 - - - - - - 4)(58 57 56 55 8 7 6 5)))
    (setq file4 '((45 44 39 38 23 22 19 18)(46 - - - - - - 17)(47 - - - - - - 16)(48 - - - - - - 15)(63 - - - - - - 14)(64 - - - - - - 3)(59 - - - - - - 4)(58 57 56 55 8 7 6 5)))

   (dotimes (i 70)
      (format t "~A" "#"))
   (format t "~%~%~30tWelcome to human mode!~%~%")
   (format t "~%Rules!~%")
   (print "1. Each position is referenced by two numbers: its column and its row.
   The lower left corner is position (1,1),")
   (print "2. A moves will be specified as three numbers separated by blanks with no other punctuation: column row value.")
   (print "3. The number you input to specify the colimn or row should be integer between 1 and the size of the board.")
   (print "4. The number you input to specify the value should be integer between 1 and the square of the board size.")
   (print "5. Please do not input any value which already exist in the original board.")
   (format t "~%")
   (dotimes (i 70)
      (format t "~A" "#"))
   (print "Please indicate the board size you want to play, 3 or 8?")
 
   (setf *size* (read))
    (cond ((equal *size* 3)(setq myboard '((1 - 3)(- - -)(7 - 9)))(setq *blank-board* file3))
          ((equal *size* 8)(setq myboard file2)(setq *blank-board* file4))
          (t (print "Illegan choice!")(human-mode)))   
       (dotimes (j *size*)
           (dotimes (i *size*)
		       (if (not(equal (sqr (1+ i) (1+ j) myboard) (sqr (1+ i) (1+ j) *blank-board*)))(set-sqr (1+ i) (1+ j) myboard '-))))
	
   (p-board myboard *size*)
   (loop 
      (setq *legal-sign* nil)
      (loop while (not *legal-sign*) do
	       (setq *legal-sign* t)
          (print "Please make a move by input X Y value")
          (setq xx (read) y-axis (read) val (read))
          (setq x-axis (1+ (- *size* xx)))
          (legal-move x-axis y-axis val myboard)
          ;(setq legal-move (and (integerp x-axis)(integerp y-axis)(integerp val)(<= val (* *size* *size*))(equal '- (sqr x-axis y-axis *blank-board*))(not (norepeatcheck val myboard))))
          
          (if *legal-sign* (progn(set-sqr x-axis y-axis myboard val)(setq *moverecord* (cons (list xx y-axis val) *moverecord*)))(print "*"))         
		  )
       
       (p-board myboard *size*)
	   (if (not(gamenotfinish myboard))(progn(print "You have finished the board,do you want to check if you succeed? (yes/no)")(setf submit (read))
	        (cond ((eql submit 'yes)(return t))
	        (t (progn (print "Keep trying!"))))))
	   )
	   
       ;(when (not(gamenotfinish myboard))(return t))
	   
       (loop while (not (null *moverecord*)) do
	      (push (pop *moverecord*) *printrecord*))  
	   
	   
       (if(checksuccess myboard)(progn(print "Your move is :")(print *printrecord*)(return-from human-mode))(print "try human mode again? (yes/no)"))
	   	(setf onceagain (read))
	    (cond ((eql onceagain 'yes)(human-mode))
	      (t (progn (print "Quit human mode")
		            (return-from human-mode t))))
	  
       )
       
(defun gamenotfinish(myboard)
       
       (dotimes (j *size*)
           (dotimes (i *size*)
              (if (equal '- (sqr (1+ i) (1+ j) myboard)) (return-from gamenotfinish t))))
	
       (return-from gamenotfinish nil))

              
(defun checksuccess(myboard) 

     (setq a 1)
       (dotimes (j *size*)
           (dotimes (i *size*)
              (if (equal a (sqr (1+ i) (1+ j) myboard))(setq a-i (1+ i) a-j (1+ j)))))
     (loop
        (setq up-sign nil)
	    (setq down-sign nil)
	    (setq left-sign nil)
	    (setq right-sign nil)
        (setq a (1+ a))
        (if (> (1- a-i) 0)(setq up-sign (equal a (sqr (1- a-i) a-j myboard))))
        (if (<= (1+ a-i) *size*)(setq down-sign (equal a (sqr (1+ a-i) a-j myboard))))
        (if (> (1- a-j) 0)(setq left-sign (equal a (sqr a-i (1- a-j) myboard))))
        (if (<= (1+ a-j) *size*)(setq right-sign (equal a (sqr a-i (1+ a-j) myboard))))
        (if up-sign (setq a-i (1- a-i) a-j a-j))
        (if down-sign (setq a-i (1+ a-i) a-j a-j))
        (if left-sign (setq a-i a-i a-j (1- a-j)))
        (if right-sign (setq a-i a-i a-j (1+ a-j)))
        (when (not (or up-sign down-sign left-sign right-sign))(return)))
    (if (equal (1- a) (* *size* *size*))(progn (print "Succeed! Congratulations!")(return-from checksuccess t)))
    (print "Sorry, you failed!")
	
    (return-from checksuccess nil))
        
(defun norepeatcheck(val myboard)
      (dotimes (j *size*)
           (dotimes (i *size*)
              (if (equal val (sqr (1+ i) (1+ j) *blank-board*)) (return-from norepeatcheck t)(setq jj 1))))
       (return-from norepeatcheck nil))

(defun legal-move(x-axis y-axis val myboard)
      (if (not(and (integerp x-axis)(integerp y-axis)(integerp val))) (progn (print "Illegal move! Please input integerp")(setq *legal-sign* nil)(return-from legal-move t))(setq jj 1))
	    (if (or (<= x-axis 0)(> x-axis *size* ))(progn (print "Illegal move! Row is out of boundary,please input the row between 1 and ")(print *size*)(setq *legal-sign* nil)(return-from legal-move t))(setq jj 1))
	      (if (or (<= y-axis 0)(> y-axis *size* ))(progn (print "Illegal move! Col is out of boundary,please input the col between 1 and ")(print *size*)(setq *legal-sign* nil)(return-from legal-move t))(setq jj 1))
            (if (not (equal '- (sqr x-axis y-axis *blank-board*)))(progn (print "Illegal move! You can not change the value in this place. Try again")(setq *legal-sign* nil)(return-from legal-move t))(setq jj 1))
	          (if (or (<= val 0)(> val (* *size* *size*)))(progn (print "Illegal move! value is out of boundary,please input the row between 1 and ")(print (* *size* *size*))(setq *legal-sign* nil)(return-from legal-move t))(setq jj 1))
                  (if (norepeatcheck val myboard)(progn (print "Illegal move! The value already exist in original board!")(setq *legal-sign* nil)(return-from legal-move t))(setq jj 1))
	   (return-from legal-move t))
	 ;(if (and (integerp x-axis)(integerp y-axis)(integerp val)(<= val (* *size* *size*))(equal '- (sqr x-axis y-axis *blank-board*))(not (norepeatcheck val))))

(defun sqr(row col brd)
 
      (nth (1- col)
	      (nth(1- row) brd)))
(defun set-sqr(row col brd val)
     (setf (nth(1- col)
	           (nth(1- row) brd)) val))
			   
(defun p-board(brd size)
  ;;based on the size change
  (if (< *size* 4)(cond ((null brd)(format t "~%")(dotimes (i size)
                                              (format t "+-"))(princ "+"))
             (t  (format t "~%")(dotimes (i size)
                                    (format t "+-"))(princ "+")(format t "~%")(p-row(car brd))(p-board(cdr brd) size))))
  (if (and(>= *size* 4)(< *size* 10))(cond ((null brd)(format t "~%")(dotimes (i size)
                                              (format t "+--"))(princ "+"))
             (t  (format t "~%")(dotimes (i size)
                                    (format t "+--"))(princ "+")(format t "~%")(pp-row(car brd))(p-board(cdr brd) size))))								
  (if (>= *size* 10)(cond ((null brd)(format t "~%")(dotimes (i size)
                                              (format t "+---"))(princ "+"))
             (t  (format t "~%")(dotimes (i size)
                                    (format t "+---"))(princ "+")(format t "~%")(p-row(car brd))(p-board(cdr brd) size))))								
									
									)				 


(defun p-row(row)
    (cond ((null row)(princ "|"))
	    (t (progn(princ "|")(princ (car row))(p-row(cdr row))))))
		
(defun pp-row(row)
    (cond ((null row)(princ "|"))
	    (t (if (equal '- (car row))(princ "| ")(if(< (car row) 10)(princ "| ")(princ "|")))(princ (car row))(pp-row(cdr row)))))   	  
        

(defun AI-mode ()
  (format t "~%Board list:~%")
  (board-list)
  (print-the-board *board*)
  (multiple-value-bind (board-result moves real-runtime cpu-runtime)
      (ai-dfs-start)
    (let ((move-format "~3d. Value: ~3d  X:~3d  Y:~3d~%"))
      (format t "~%Result :~%~%")
      (print-the-board board-result)
      (format t "    Real time(seconds) is : ~a~%" real-runtime)
      (format t "    CPU time(seconds) is :   ~a~%~%~%" cpu-runtime)
;;;;;;
      (when (prompt-yes-no "See each step of how AI play the board?")
        (let ((board-state (copy-board *size* *board*))
              (counter 0)
              (move-format (concatenate 'string "~%" move-format "~%")))
          (mapcar #'(lambda (move)
                      (let ((value (first move))
                            (x (1+ (second move)))
                            (y (- *size* (third move))))
                        (apply-solutions board-state (list move))                       
                        (print-the-board board-state)
                        (format t move-format (incf counter) value x y)
                        (prompt-read-enter)))
                  moves)))
      (when (prompt-yes-no "Try another board?")
        (AI-mode)))))

(defun board-list ()
  (let ((choice-index nil)
        (choice-raw "")
        (choice-value "")
        (board-not-loaded t)
        (user-command nil))
    (load-boardname-list)
    (loop do
         (progn
           (format t "~%")
           (print-board-list)
           (format t "~%")
           (setf choice-raw (prompt-read "Please input the index of the board"))
           (setf choice-index (parse-integer choice-raw :junk-allowed t))
           (setf user-command (user-command-parse choice-raw))
           (if user-command
               (progn
                 (setf user-command (user-command-handle user-command))
                 (when (eq user-command (getf *program-flow* :exit))
                   (setf board-not-loaded nil)))
               (progn
                  (if (board-list-choice-invalid? choice-index)
                     (print-user-error-msg "Invail choice,try again")
                     (progn
                       (setf choice-value (elt *boardname-list* (1- choice-index)))
                       (load-board (make-pathname :directory `(,*board-path*) :name choice-value))
                       (setf board-not-loaded nil))))))
       while 
       board-not-loaded)
       user-command))

(defun print-the-board (board)
    (dotimes (row *size*)
      (format-board-row 
       (board-to-string-pretty-component *size* 'divider))
      (format-board-row 
       (board-to-string-pretty-component *size* 'spacer))
      (format-board-row 
       (board-to-string-pretty-component *size* 'number (elt board row)) "~2d ~a~%" (convert-internal-coord-y row))
      (format-board-row 
       (board-to-string-pretty-component *size* 'spacer)))
    (format-board-row (board-to-string-pretty-component *size* 'divider))
    (format t "~a~%~%" (format-board-x-coords)))

(defun ai-dfs-start ()
  (let ((real-start (get-internal-real-time))
        (cpu-start (get-internal-run-time)))
    (multiple-value-bind (board-result solutions-result)
        (ai-dfs *board*)
    (let* ((real-end (get-internal-real-time))
           (cpu-end (get-internal-run-time))
           (real-runtime (float (/ (- real-end real-start) internal-time-units-per-second)))
           (cpu-runtime (float (/ (- cpu-end cpu-start) internal-time-units-per-second)))
           (moves (extract-moves-from-solution-tracker solutions-result)))
      (values
       board-result
       moves
       real-runtime
       cpu-runtime)))))

(defun prompt-yes-no (msg)
   (let ((response (string-downcase (prompt-read (concatenate 'string msg " (yes/no) ")))))
     (find response '("y" "ye" "yes") :test 'string=)))

(defun apply-solutions (board solutions)
  (mapcar #'(lambda (solution) 
              (set-pos board 
                       (second solution) 
                       (third solution) 
                       (first solution)))
          solutions))


(defun load-boardname-list ()
  (setf *boardname-list*
         (with-open-file (stream *boardname-definitions*)
         (read stream))))

(defun print-board-list ()
   (dotimes (i (length *boardname-list*))
    (format t "~2d. ~a~%" (1+ i) (elt *boardname-list* i))))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun user-command-parse (move)
  (let ((move-clean (string-downcase (string-trim '(#\Space #\Tab #\Newline) move))))
  (cond
      ((string= move-clean "quit")
       (getf *user-commands* :quit))
      ((string= move-clean "exit")
       (getf *user-commands* :quit))
      ((string= move-clean "help")
       (getf *user-commands* :help)))))

(defun user-command-handle (command)
  (ecase command
    (quit 
      (getf *program-flow* :exit))
    (help
      (progn
       (print-user-help-messages)
       (getf *program-flow* :continue)))))

(defun copy-board (size board &optional (init-val 0) (data-type 'integer) (deep-copy t))
  (let ((board-copy (make-board size init-val data-type)))
    (when deep-copy
      (dotimes (y *size*)
        (dotimes (x *size*)
          (let ((value (get-pos board x y)))
          (set-pos board-copy x y value)))))
     board-copy))

(defun print-user-help-message (msg)
  (format t "~%~a~%~%" msg) 
  (prompt-read-enter))

(defun print-user-help-messages ()
  (mapcar #'(lambda (msg) (print-user-help-message msg)) *user-help-message*))

(defun prompt-read-enter ()
  (prompt-read "Next step press enter"))

(defun board-list-choice-invalid? (choice)
  (let ((min-val 1)
        (max-val (length *boardname-list*)))
     (or (not (integerp choice)) (range-invalid? min-val max-val choice))))

(defun print-user-error-msg (msg)
  (format t "~%~a~%~%" msg)
  (prompt-read-enter))

(defun load-board (boardname)
  (numbrix-board 
   (with-open-file (stream boardname)
     (read stream))))

(defun format-board-row (row &optional (format-string "~a~a~%") (margin "   "))
  (format t format-string margin row))

(defun board-to-string-pretty-component (size type &optional row)
  (let ((divider-segment "+------")
        (divider-endpoint "+")
        (spacer-segment "|      ")
        (spacer-endpoint "|")
        (number-segment "|~4d  ")
        (component ""))
    (dotimes (i size)
      (ecase type
        (divider
           (setf component (board-to-string-pretty-component-build 
                    size i component divider-segment divider-endpoint)))
      (spacer
           (setf component (board-to-string-pretty-component-build 
                      size i component spacer-segment spacer-endpoint)))
       (number
        (let ((number (elt row i)))
            (setf component (board-to-string-pretty-component-build 
                   size i component number-segment spacer-endpoint :number number :aux-segment spacer-segment))))))
     component))

(defun ai-dfs (board)
  (let ((solution-tracker nil)
        (invalid-solution nil)
        (endpoints nil)
        (endpoint-start nil)
        (endpoint-end nil)
        (move-tracker nil)
        (board-state (copy-board *size* board))
        (attempt-tracker nil)
        (endpoint-tracker nil)
        (solutions-trivial nil))
    (loop do
         (debug-message "AI-DFS: ENDPOINTS: ~a ~a" endpoint-start endpoint-end)
         (when *debug-mode* (print-the-board board-state))
         (if invalid-solution
             (progn
               (debug-message "AI-DFS: ANOTHER:~%~a~%~a~%~a~%" move-tracker board-state attempt-tracker)
               (multiple-value-bind (move-tracker-result board-state-result attempt-tracker-result endpoint-tracker-result)
                   (try-dfs-another-solution
                    board-state endpoint-start endpoint-end move-tracker board-state attempt-tracker endpoint-tracker)
                 (setf move-tracker move-tracker-result)
                 (setf board-state board-state-result)
                 (debug-message "BOARD-STATE: ~a" board-state)
                 (setf attempt-tracker attempt-tracker-result)
                 (setf endpoint-tracker endpoint-tracker-result)))
             (multiple-value-bind (solutions-trivial-result board-state-trivial)
                 (try-easy board-state)
               (setf move-tracker nil)
               (setf solutions-trivial solutions-trivial-result)
               (setf board-state board-state-trivial)
               (debug-message "BOARD-STATE: ~a" board-state)
               (setf endpoints (first (incomplete-sequence-endpoints board-state)))
               (setf endpoint-start (first endpoints))
               (setf endpoint-end (second endpoints))
               (when endpoints
                 (multiple-value-bind (move-tracker-result board-state-result 
                                                           attempt-tracker-result endpoint-tracker-result)
                     (try-dfs-search-delegator board-state endpoint-start endpoint-end)
                   (setf move-tracker move-tracker-result)
                   (setf board-state board-state-result)
                   (debug-message "BOARD-STATE: ~a" board-state)
                   (setf attempt-tracker attempt-tracker-result)
                   (setf endpoint-tracker endpoint-tracker-result)))))
         (when endpoints
           (if move-tracker
               (progn
                 (setf invalid-solution nil)
                 (push (make-try-dfs :move-tracker move-tracker
                                                 :move-trivial-tracker solutions-trivial
                                                 :board-state board-state
                                                 :attempt-tracker attempt-tracker
                                                 :endpoint-start endpoint-start
                                                 :endpoint-end endpoint-end
                                                 :endpoint-tracker endpoint-tracker)
                       solution-tracker)
                 (debug-message "AI-DFS: MOVE FOUND:~%move-tracker:~a~%board-state:~a~%invalid-solution:~a~%solution-tracker:~a~%" move-tracker board-state invalid-solution solution-tracker))               
               (progn
                 (debug-message "AI-DFS: MOVE NOT FOUND")
                 (setf invalid-solution (pop solution-tracker))
                 (when invalid-solution
                   (setf move-tracker (try-dfs-move-tracker invalid-solution))
                   (setf solutions-trivial (try-dfs-move-trivial-tracker invalid-solution))
                   (setf board-state (try-dfs-board-state invalid-solution))
                   (setf attempt-tracker (try-dfs-attempt-tracker invalid-solution))
                   (setf endpoint-start (try-dfs-endpoint-start invalid-solution))
                   (setf endpoint-end (try-dfs-endpoint-end invalid-solution))
                   (setf endpoint-tracker (try-dfs-endpoint-tracker invalid-solution)))
                 (debug-message "invalid-solution: ~a" invalid-solution))))
         (when (and (null endpoints) solutions-trivial)
           (debug-message "Capturing lingering trivial solutions: ~a" solutions-trivial)
           (push (make-try-dfs :move-tracker move-tracker
                                           :move-trivial-tracker solutions-trivial
                                           :board-state board-state
                                           :attempt-tracker attempt-tracker
                                           :endpoint-start endpoint-start
                                           :endpoint-end endpoint-end
                                           :endpoint-tracker endpoint-tracker)
                 solution-tracker))
       while
         (board-invalid? board-state))
    (values
     board-state
     solution-tracker)))

(defun extract-moves-from-solution-tracker (solution-tracker)
  (let ((moves (mapcan #'(lambda (solution)
                           (debug-message "extracting: ~a" solution)
                           (append (try-dfs-move-tracker solution)
                                   (try-dfs-move-trivial-tracker solution)))
                       solution-tracker)))
    (reverse moves)))

(defmacro debug-message (control-string &rest format-arguments)
  (when *debug-mode*
    `(format t (concatenate 'string ,control-string "~%") ,@format-arguments)))

(defun try-dfs-another-solution (board start end move-tracker board-state attempt-tracker endpoint-tracker)
  (multiple-value-bind (move-tracker board-state attempt-tracker 
                                     endpoint-tracker another-solution-found endpoint-recalculated)
      (try-dfs-calculate-another-solution move-tracker board-state attempt-tracker endpoint-tracker start end)
    (debug-message "ANOTHER-SOLUTION: found: ~a~%move-tracker: ~a~%attempt-tracker: ~a~%" 
                   another-solution-found move-tracker attempt-tracker)
    (when another-solution-found
      (multiple-value-bind (move-tracker board-state attempt-tracker endpoint-tracker)
          (if (endpoints-known? start end)
              (try-dfs-search board start end move-tracker board-state attempt-tracker)
              (try-dfs-search-endpoints board start end
                                                    move-tracker board-state 
                                                    attempt-tracker endpoint-tracker endpoint-recalculated))
        (values
         move-tracker
         board-state
         attempt-tracker
         endpoint-tracker)))))

(defun try-dfs-calculate-another-solution (move-tracker board-state attempt-tracker endpoint-tracker start end)
  (let ((another-solution-found nil)
        (altered-move nil)
        (failed-attempts nil)
        (invalid-move nil)
        (valid-moves nil)
        (endpoint-recalculated nil))
    (debug-message "CALCULATE-ANOTHER:~%move-tracker: ~a~%board-state: ~a~%start: ~a~%end: ~a~%endpoint-tracker: ~a"
                   move-tracker board-state start end endpoint-tracker)
    (cond 
      ((move-unknown? end)
         (cond 
           ((= 2 (length move-tracker))
            (progn
              (setf move-tracker (rest move-tracker))
              (setf invalid-move (first move-tracker))
              (debug-message "CALCULATE-ANOTHER: Recalculating End Point")
              (set-pos board-state (second invalid-move) (third invalid-move) 0)
              (setf another-solution-found (endpoint-tracker-invalidate-position endpoint-tracker))
                                        ;(setf endpoint-recalculated (when another-solution-found endpoint-recalculated))
              (setf valid-moves (endpoint-tracker-next-available endpoint-tracker))))
           ((= 1 (length move-tracker))
            (progn
              (setf invalid-move (first move-tracker))
              (debug-message "CALCULATE-ANOTHER: Recalculating End Point")
              (set-pos board-state (second invalid-move) (third invalid-move) 0)
              (setf another-solution-found (endpoint-tracker-invalidate-position endpoint-tracker))
             ;(setf endpoint-recalculated (when another-solution-found endpoint-recalculated))
              (setf valid-moves (endpoint-tracker-next-available endpoint-tracker))))
           (t
            (progn
              (setf move-tracker (rest move-tracker))
              (setf invalid-move (first move-tracker))
              (setf valid-moves (rest move-tracker))
              (setf altered-move (first valid-moves))
              (set-pos board-state (second invalid-move) (third invalid-move) 0)
              (debug-message "CALCULATE-ANOTHER: End Point~%tracker: ~a~%board-state:~a~%valid-moves: ~a~%"
                             move-tracker board-state valid-moves)
              (setf failed-attempts (get-pos attempt-tracker (second altered-move) (third altered-move)))
              (setf another-solution-found (failed-attempt-mark-next failed-attempts))
              (debug-message "CALCULATE-ANOTHER: End Point~%tracker: ~a~%board-state:~a~%found: ~a~%valid-moves: ~a~%failed-attempts: ~a"
                             move-tracker board-state another-solution-found valid-moves failed-attempts)))))
      ((move-unknown? start)
       (cond
         ((= 2 (length move-tracker))
          (progn
            (setf invalid-move (first move-tracker))
            (setf move-tracker (rest move-tracker))
            (setf valid-moves move-tracker)
            (setf altered-move (first valid-moves))
            (setf failed-attempts (get-pos attempt-tracker (second altered-move) (third altered-move)))
            (setf another-solution-found (failed-attempt-mark-next failed-attempts))
            (set-pos board-state (second invalid-move) (third invalid-move) 0)
            (debug-message 
             "CALCULATE-ANOTHER:~%tracker: ~a~%board-state:~a~%found: ~a~%valid-moves: ~a~%failed-attempts: ~a"
             move-tracker board-state another-solution-found valid-moves failed-attempts)))
         ((= 1 (length move-tracker))
          (let ((invalid-move (first move-tracker)))
            (debug-message "CALCULATE-ANOTHER: Recalculating Start Point")
            (set-pos board-state (second invalid-move) (third invalid-move) 0)
            (setf another-solution-found (endpoint-tracker-invalidate-position endpoint-tracker))
            (setf valid-moves (endpoint-tracker-next-available endpoint-tracker))))
         (t
          (progn
            (setf move-tracker (butlast move-tracker))
            (setf invalid-move (first move-tracker))
            (setf valid-moves (rest move-tracker))
            (setf altered-move (first valid-moves))
            (setf failed-attempts (get-pos attempt-tracker (second altered-move) (third altered-move)))
            (set-pos board-state (second invalid-move) (third invalid-move) 0)
            (setf another-solution-found (failed-attempt-mark-next failed-attempts))
            (debug-message 
             "CALCULATE-ANOTHER:~%tracker: ~a~%board-state:~a~%found: ~a~%valid-moves: ~a~%failed-attempts: ~a"
             move-tracker board-state another-solution-found valid-moves failed-attempts)))))
      (t
       (let* ((move-tracker-copy 
               (copy-list (if (= 1 (length move-tracker)) (append move-tracker (list start)) move-tracker))))
         (setf invalid-move (first move-tracker-copy))
         (setf valid-moves (rest move-tracker-copy))
         (setf altered-move (first valid-moves))
         (setf failed-attempts (get-pos attempt-tracker (second altered-move) (third altered-move)))
         (set-pos board-state (second invalid-move) (third invalid-move) 0)
         (setf another-solution-found (failed-attempt-mark-next failed-attempts))
         (debug-message 
          "CALCULATE-ANOTHER:~%tracker: ~a~%tracker-copy: ~a~%board-state:~a~%found: ~a~%valid-moves: ~a~%failed-attempts: ~a"
          move-tracker move-tracker-copy board-state another-solution-found valid-moves failed-attempts))))
    (debug-message "CALCULATE-ANother:~%attempt-tracker: ~a~%" attempt-tracker)
    (values
     (when (/= 1 (length move-tracker))
       valid-moves)
     board-state
     attempt-tracker
     endpoint-tracker
     another-solution-found
     endpoint-recalculated)))

(defun try-dfs-search (board start end &optional move-tracker board-state attempt-tracker)
  (debug-message "SOLUTION-SEARCH: start: ~a end: ~a" start end)
  (let* ((move-tracker (if (null move-tracker) () move-tracker))
         (board-state (if (null board-state) (copy-board *size* board) board-state))
         (attempt-tracker (if (null attempt-tracker) (try-dfs-prepare-attempt-tracker board) attempt-tracker))
         (current-move (if (null move-tracker) start (first move-tracker)))
         (next-move ())
         (value-difference (1- (endpoints-value-difference start end)))
         (value-start (first start))
         (value-goal (first end))
         (expected-values (build-dynamic-value-list value-difference value-start 1))
         (expected-value ())
         (short-solution-found nil))
    (loop do
         (debug-message "expected-values: ~a move-tracker: ~a value-difference: ~a" 
                        expected-values move-tracker value-difference)
         (if (> value-difference 0)
             (progn
               (setf expected-value (elt expected-values (length move-tracker)))
               (setf next-move
                     (try-dfs-next-move board-state attempt-tracker
                                                    expected-value
                                                    end
                                                    value-goal
                                                    (second current-move) 
                                                    (third current-move))))
             (progn
               (debug-message "SOLUTION-SEARCH: SHORT-SOLUTION")
               (setf short-solution-found t)))
         (debug-message "~a: current: ~a next: ~a move-tracker: ~a" 
                        expected-value current-move next-move move-tracker)
         (cond (next-move
                (progn
                  (setf move-tracker (push next-move move-tracker))
                  (setf current-move next-move)
                  (set-pos board-state (second current-move) (third current-move) (first current-move) t)))
               (move-tracker
                (let* ((invalid-move (pop move-tracker)))
                  (set-pos attempt-tracker (second invalid-move) (third invalid-move) (make-failed-attempts))                    
                  (set-pos board-state (second invalid-move) (third invalid-move) 0)
                  (if move-tracker
                      (progn
                        (setf current-move (first move-tracker))
                        (attempt-tracker-mark-failed current-move invalid-move attempt-tracker))
                      (progn
                        (setf current-move start)
                        (attempt-tracker-mark-failed start invalid-move attempt-tracker))))))
         (debug-message "~a: current: ~a next: ~a~%move-tracker: ~a" 
                        expected-value current-move next-move move-tracker)
         (debug-message "move exists: ~a~%" (attempt-tracker-move-exists start attempt-tracker))
         (debug-message "board-state:~%~a~%" board-state)
         (error-if-duplicate-values board-state "TRY-DFS-SEARCH")
       while
         (and (< (length move-tracker) (length expected-values))
              (attempt-tracker-move-exists start attempt-tracker)
              (not short-solution-found)))
    (debug-message "board-state:~%~a~%" board-state)
    (values
     move-tracker
     board-state
     attempt-tracker
     short-solution-found)))

(defun value-empty? (val)
  (= val 0))

(defun value-set? (val)
  (not (value-empty? val)))

(defun get-pos (board x y)
  (aref (aref board y) x))

(defun set-pos (board x y val &optional verify)
  (when (and verify
            (value-set? (get-pos board x y)))
     (error (format nil "Position x: ~a y: ~a is being written to with value ~a but it already has value ~a" x y val (get-pos board x y))))
  (setf (aref (aref board y) x) val))

(defun attempt-tracker-mark-failed (source neighbor attempt-tracker)
  (let ((failed-attempts (get-pos attempt-tracker (second source) (third source))))
    (failed-attempt-mark failed-attempts (direction-neighbor source neighbor)))) 
    
(defun range-invalid? (min max value)
  (or (< value min) (> value max)))

(defun numbrix-board (board-definition)
  (init :board (read-board-definition board-definition)))
  
(defun init (&key size board)
  (cond (size
         (progn
           (setf *size* size)
           (setf *board* (make-board *size*))))
         (board
         (progn
           (setf *size* (length board))
           (setf *board* board)))
          (t
         (error "INIT - unexpected input")))
  (setf *board-default-positions* (make-board-default-positions *board*))
  (setf *board-position-types* (make-board-position-types))
  (setf *num-squares* (* *size* *size*))
  (setf *user-moves* ())
  board)
  
(defun read-board-definition (board-definition)
  (let* ((size (length board-definition))
          (board (make-board size)))
     (when board-definition
       (dotimes (y size)
         (dotimes (x size)
          (set-pos board x y (elt (elt board-definition y) x)))))
    board))
    
(defun make-board (size &optional (init-val 0) (data-type 'integer))
  (let ((board (make-array size :initial-element (make-array size) :element-type 'vector)))
    (map 'vector 
        #'(lambda (x) (setf x (make-array size :initial-element init-val :element-type data-type)))
          board)))

(defun make-board-default-positions (board)
  (let ((board-default-positions (make-board *size* nil 'boolean)))
    (dotimes (y *size*)
      (dotimes (x *size*)
        (let ((value (get-pos board x y)))
            (when (> value 0)
            (set-pos board-default-positions x y t)))))
    board-default-positions))
    
(defun make-board-position-types ()
  (let ((board-position-types (make-board *size* nil 'symbol)))
    (dotimes (y *size*)
      (dotimes (x *size*)
        (let ((value (square-position-type x y)))
           (set-pos board-position-types x y value))))
     board-position-types))

(defun square-position-type (x y &optional return-type-key)
  (let ((bound-lower 0)
        (bound-upper (1- *size*))
        (position-types (if return-type-key *position-types-key* *position-types*)))
    (cond ((eq y bound-lower)
           (cond ((eq x bound-lower)
                  (getf position-types :nw))
                 ((eq x bound-upper)
                  (getf position-types :ne))
                 (t
                   (getf position-types :n))))
          ((eq y bound-upper)
           (cond ((eq x bound-lower)
                  (getf position-types :sw))
                 ((eq x bound-upper)
                  (getf position-types :se))
                (t
                  (getf position-types :s))))
          ((eq x bound-lower)
           (getf position-types :w))
          ((eq x bound-upper)
           (getf position-types :e))
          (t
          (getf position-types :inner)))))
          
(defun board-to-string-pretty-component-build (size iteration component segment endpoint &key number aux-segment)
  (cond
    ((null number) nil)
    ((= number 0) (setf segment aux-segment))
    (number (setf segment (format nil segment number))))
  (setf component (format nil "~a~a" component segment))
  (when (= (1- size) iteration)
    (setf component (format nil "~a~a" component endpoint)))
  component)

(defun convert-internal-coord-y (y)
  (- *size* y))
  
(defun format-board-x-coords ()
  (let ((str "   "))
    (setf str (format nil "~%~a" str))
    (dotimes (i *size*)
      (setf str (format nil "~a~5d  " str (convert-internal-coord-x i))))
    str))

(defun convert-internal-coord-x (x)
  (1+ x))

(defun convert-internal-coord-y (y)
  (- *size* y))
  
(defun try-easy (board)
  (let ((board-state (copy-board *size* board))
        (solution-found nil)
        (solutions ())
        (solved-positions (remove-empty (flatten-board board))))
    (loop do
         (setf solution-found nil)
         (dotimes (x *size*)
           (dotimes (y *size*)
             (let ((solution (easy-solution board-state x y)))
               (when (and solution
                          (not (find solution solved-positions)))
                 (debug-message "try-easy: solution ~a at (~a ~a)" solution x y)
                 (set-pos board-state x y solution t)
                 (setf solutions (push (list solution x y) solutions))
                 (setf solution-found t)))))
         (let ((solutions-multi-cell (easy-solution-multi-cell board-state)))
           (when solutions-multi-cell
             (progn
               (apply-solutions board-state solutions-multi-cell)
               (setf solutions (append solutions-multi-cell solutions))
               (setf solution-found t))))
         (error-if-duplicate-values board-state "try-easy")
       while 
       solution-found)
    (values
     solutions
     board-state)))
     
(defun remove-empty (values &optional with-coords)
  (remove-if-not #'(lambda (x) (> (if with-coords (first x) x) 0)) values))
  
(defun flatten-board (board &optional with-coords)
  (let ((flattened-board ()))
    (dotimes (y *size*)
      (dotimes (x *size*)
        (let* ((value (get-pos board x y))
              (element (if with-coords (list value x y) value)))
         (setf flattened-board (append flattened-board (list element))))))
    flattened-board))


(defun easy-solution (board x y)
  (let ((value (get-pos board x y)))
    (when (value-empty? value)
      (let ((position-type (square-position-type x y)))
        (ecase position-type
          (inner (make-easy-solution-exps :vert :horz :ne :se :sw :nw :min :max))
          (n (make-easy-solution-exps :horz :se :sw :min :max))
          (ne (make-easy-solution-exps :sw :min :max))
          (e (make-easy-solution-exps :vert :sw :nw :min :max))
          (se (make-easy-solution-exps :nw :min :max))
          (s (make-easy-solution-exps :horz :ne :nw :min :max))
          (sw (make-easy-solution-exps :ne :min :max))
          (w (make-easy-solution-exps :vert :ne :se :min :max))
          (nw (make-easy-solution-exps :se :min :max)))))))
          
(defun easy-solution-max (board x y)
  (let* ((max-value *num-squares*)       
         (max-value-adjacent (1- max-value))
         (sequence-direction -1))
    (easy-solution-endpoint board x y max-value-adjacent max-value sequence-direction)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-easy-solution-exp (get-easy-solution-fun)
    (let ((easy-solution-fun (getf *easy-solution-function-types* get-easy-solution-fun)))
      (list easy-solution-fun 'board 'x 'y))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-easy-solution-exps (get-easy-solution-funs)
    (mapcar #'(lambda (x) (get-easy-solution-exp x)) get-easy-solution-funs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro make-easy-solution-exps (&rest funs)
    `(or ,@(get-easy-solution-exps funs))))
    
(defun easy-solution-vert (board x y)
  (let* ((n (get-pos board x (1- y)))
         (s (get-pos board x (1+ y))))
    (easy-solution-linear n s)))

(defun easy-solution-horz (board x y)
  (let* ((e (get-pos board (1+ x) y))
         (w (get-pos board (1- x) y)))
    (easy-solution-linear e w)))

(defun easy-solution-linear (n1 n2)
  (when (and (value-set? n1) (value-set? n2) (neighbors-sequential? n1 n2))
    (complete-sequence n1 n2)))

(defun easy-solution-angular (n1 n2 diag)
  (unless (value-empty? diag)
    (let ((solution (easy-solution-linear n1 n2)))
      (unless (eq solution diag)
        solution))))
        
(defun easy-solution-ne (board x y)
  (let* ((n (get-pos board x (1- y)))
         (e (get-pos board (1+ x) y))
         (ne (get-pos board (1+ x) (1- y))))
    (easy-solution-angular n e ne)))

(defun easy-solution-se (board x y)
  (let* ((s (get-pos board x (1+ y)))
         (e (get-pos board (1+ x) y))
         (se (get-pos board (1+ x) (1+ y))))
    (easy-solution-angular s e se)))

(defun easy-solution-sw (board x y)
  (let* ((s (get-pos board x (1+ y)))
         (w (get-pos board (1- x) y))
         (sw (get-pos board (1- x) (1+ y))))
    (easy-solution-angular s w sw)))

(defun easy-solution-nw (board x y)
  (let* ((n (get-pos board x (1- y)))
         (w (get-pos board (1- x) y))
         (nw (get-pos board (1- x) (1- y))))
    (easy-solution-angular n w nw)))
    
(defun easy-solution-min (board x y)
  (let* ((min-value 1)
         (min-value-adjacent (1+ min-value))
         (sequence-direction 1))
    (easy-solution-endpoint board x y min-value-adjacent min-value sequence-direction)))
    
(defun easy-solution-endpoint (board x y adjacency target direction)
  (unless (find target (flatten-board *board*))
    (let ((adjacency-coords (easy-solution-endpoint-find-adjacency board x y adjacency))
          (neighbors (get-neighbors *board* x y)))
      (when (and adjacency-coords
                 (find adjacency neighbors)) 
        (let* ((next-adjacency (+ adjacency direction))
               (x-coord (first adjacency-coords))
               (y-coord (second adjacency-coords))
               (adjacency-neighbors (remove-if #'value-empty? (get-neighbors board x-coord y-coord)))
               (adjacency-max-neighbors (get-max-neighbors (square-position-type x-coord y-coord t))))
          (when (and (= (length adjacency-neighbors) (1- adjacency-max-neighbors))
                     (find next-adjacency adjacency-neighbors))
            target))))))

(defun easy-solution-endpoint-find-adjacency (board x y adjacency)
  (let ((position-type (square-position-type x y)))
    (ecase position-type
      (inner (make-find-adjacency-exps :n :e :s :w))
      (n (make-find-adjacency-exps :e :s :w))
      (ne (make-find-adjacency-exps :s :w))
      (e (make-find-adjacency-exps :n :s :w))
      (se (make-find-adjacency-exps :n :w))
      (s (make-find-adjacency-exps :n :e :w))
      (sw (make-find-adjacency-exps :n :e))
      (w (make-find-adjacency-exps :n :e :s))
      (nw (make-find-adjacency-exps :e :s)))))
      

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-find-adjacency-exp (get-find-adjacency-fun)
    (let ((find-adjacency-fun (getf *find-adjacency-function-types* get-find-adjacency-fun)))
      (list find-adjacency-fun 'board 'x 'y 'adjacency))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-find-adjacency-exps (get-find-adjacency-funs)
    (mapcar #'(lambda (x) (get-find-adjacency-exp x)) get-find-adjacency-funs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro make-find-adjacency-exps (&rest funs)
    `(or ,@(get-find-adjacency-exps funs))))

(defun find-adjacency-n (board x y adjacency)
  (let ((x-coord x)
        (y-coord (1- y)))
    (find-adjacency board x-coord y-coord adjacency)))

(defun find-adjacency-e (board x y adjacency)
  (let ((x-coord (1+ x))
        (y-coord y))
    (find-adjacency board x-coord y-coord adjacency)))

(defun find-adjacency-s (board x y adjacency)
  (let ((x-coord x)
        (y-coord (1+ y)))
    (find-adjacency board x-coord y-coord adjacency)))

(defun find-adjacency-w (board x y adjacency)
  (let ((x-coord (1- x))
        (y-coord y))
    (find-adjacency board x-coord y-coord adjacency)))
    
(defun find-adjacency (board x y adjacency)
  (let ((value (get-pos board x y)))
    (when (= value adjacency)
      (list x y))))
      
(defun get-neighbors (board x y &optional complete)
  (let ((position-type (get-pos *board-position-types* x y))
        (neighbors ()))
     (when (> *size* 1)
      (if complete
        (ecase position-type
          (inner (make-get-neighbor-exps :nw :w :sw :s :se :e :ne :n))
          (n (make-get-neighbor-exps :w :sw :s :se :e))
          (ne (make-get-neighbor-exps :w :sw :s))
          (e (make-get-neighbor-exps :nw :w :sw :s :n))
          (se (make-get-neighbor-exps :nw :w :n))
          (s (make-get-neighbor-exps :nw :w :e :ne :n))
          (sw (make-get-neighbor-exps :e :ne :n))
          (w (make-get-neighbor-exps :s :se :e :ne :n))
          (nw (make-get-neighbor-exps :s :se :e)))
        (ecase position-type
          (inner (make-get-neighbor-exps :w :s :e :n))
          (n (make-get-neighbor-exps :w :s :e))
          (ne (make-get-neighbor-exps :w :s))
           (e (make-get-neighbor-exps :w :s :n))
          (se (make-get-neighbor-exps :w :n))
          (s (make-get-neighbor-exps :w :e :n))
          (sw (make-get-neighbor-exps :e :n))
          (w (make-get-neighbor-exps :s :e :n))
          (nw (make-get-neighbor-exps :s :e)))))
    neighbors))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-neighbor-exp (get-neighbor-fun)
    (let ((neighbor-fun (getf *neighbor-function-types* get-neighbor-fun)))
      (list 'push (list neighbor-fun 'board 'x 'y) 'neighbors))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-neighbor-exps (get-neighbor-funs)
    (mapcar #'(lambda (x) (get-neighbor-exp x)) get-neighbor-funs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro make-get-neighbor-exps (&rest funs)
    `(progn ,@(get-neighbor-exps funs))))

(defun get-neighbor-n (board x y)
  (get-pos board x (1- y)))

(defun get-neighbor-ne (board x y)
  (get-pos board (1+ x) (1- y)))

(defun get-neighbor-e (board x y)
  (get-pos board (1+ x) y))

(defun get-neighbor-se (board x y)
  (get-pos board (1+ x) (1+ y)))

(defun get-neighbor-s (board x y)
  (get-pos board x (1+ y)))

(defun get-neighbor-sw (board x y)
  (get-pos board (1- x) (1+ y)))

(defun get-neighbor-w (board x y)
  (get-pos board (1- x) y))

(defun get-neighbor-nw (board x y)
  (get-pos board (1- x) (1- y)))
  
(defun easy-solution-multi-cell (*board*)
  (let ((endpoints (incomplete-sequence-endpoints *board*))
        (solutions ()))
    (mapcar #'(lambda (x)
                (let ((start (first x))
                      (end (second x)))
                  (when (endpoints-known? start end)
                    (let ((distance (endpoints-distance start end))
                          (value-difference (endpoints-value-difference start end))
                          (common-axis (endpoints-common-axis start end)))
                      (when (and common-axis
                                 (= distance (1- value-difference)))
                        (setf solutions 
                              (append solutions 
                                      (easy-solution-multi-cell-sequence start end common-axis distance))))))))
            endpoints)
    solutions))
    
(defun incomplete-sequence-endpoints (board &optional (sorted t))
  (let* ((values (stable-sort (remove-empty (flatten-board board t) t) #'sort-ascending-value-coord-tuple))
         (values-length (length values))
         (endpoints ())
         (min 1)
         (max *num-squares*)
         (endpoints-min nil)
         (endpoints-max nil))
    (let ((first-endpoint (first values))
          (last-endpoint (first (reverse values))))
      (when (/= (first first-endpoint) min)
        (setf endpoints-min (list (list (list min nil nil) (car values)))))
      (when (/= (first last-endpoint) max)
        (setf endpoints-max (list (list (car (reverse values)) (list max nil nil))))))
    (dotimes (i values-length)
      (when (< i (1- values-length))
        (let ((start (elt values i))
              (end (elt values (1+ i))))
          (when (/= (- (first end) (first start)) 1)
            (setf endpoints (append endpoints (list (list start end))))))))
    (setf endpoints (append endpoints-min endpoints endpoints-max))
    (if sorted
      (sort-endpoints endpoints)
      endpoints)))
      
(defun sort-ascending-value-coord-tuple (tuple1 tuple2)
  (let ((val1 (first tuple1))
        (val2 (first tuple2)))
    (< val1 val2)))

(defun sort-endpoints (endpoints)
  (stable-sort endpoints #'endpoint-cost-comparison))

(defun incomplete-sequence-endpoints (board &optional (sorted t))
  (let* ((values (stable-sort (remove-empty (flatten-board board t) t) #'sort-ascending-value-coord-tuple))
         (values-length (length values))
         (endpoints ())
         (min 1)
         (max *num-squares*)
         (endpoints-min nil)
         (endpoints-max nil))
    (let ((first-endpoint (first values))
          (last-endpoint (first (reverse values))))
      (when (/= (first first-endpoint) min)
        (setf endpoints-min (list (list (list min nil nil) (car values)))))
      (when (/= (first last-endpoint) max)
        (setf endpoints-max (list (list (car (reverse values)) (list max nil nil))))))
    (dotimes (i values-length)
      (when (< i (1- values-length))
        (let ((start (elt values i))
              (end (elt values (1+ i))))
          (when (/= (- (first end) (first start)) 1)
            (setf endpoints (append endpoints (list (list start end))))))))
    (setf endpoints (append endpoints-min endpoints endpoints-max))
    (if sorted
      (sort-endpoints endpoints)
      endpoints)))

(defun endpoint-cost-comparison (e1 e2)
  (let* ((value-multiplier 2)
         (e1-start (first e1))
         (e1-end (second e1))
         (e2-start (first e2))
         (e2-end (second e2))
         (e1-known? (endpoints-known? e1-start e1-end))
         (e2-known? (endpoints-known? e2-start e2-end))
         (e1-value-difference (- (endpoints-value-difference e1-start e1-end) 1))
         (e2-value-difference (- (endpoints-value-difference e2-start e2-end) 1))
         (e1-coord-difference (if e1-known? 
                                  (endpoints-distance e1-start e1-end) 
                                  (endpoint-unknown-cost-estimate e1-value-difference)))
         (e2-coord-difference (if e2-known? 
                                  (endpoints-distance e2-start e2-end) 
                                  (endpoint-unknown-cost-estimate e2-value-difference)))
         (e1-cost-estimate (if e1-known? (- (* e1-value-difference value-multiplier) e1-coord-difference) e1-coord-difference))
         (e2-cost-estimate (if e2-known? (- (* e2-value-difference value-multiplier) e2-coord-difference) e2-coord-difference))
         (e1-favored (< e1-cost-estimate e2-cost-estimate)))
    (debug-message "e1-start: ~a e1-end: ~a" e1-start e1-end)
    (debug-message "e2-start: ~a e2-end: ~a" e2-start e2-end)
    (debug-message "e1-val-diff ~a e2-val-diff: ~a" e1-value-difference e2-value-difference)
    (debug-message "e1-coord-diff ~a e2-coord-diff: ~a" e1-coord-difference e2-coord-difference)
    (debug-message "e1-cost-estimate ~a e2-cost-estimate: ~a" e1-cost-estimate e2-cost-estimate)
    (debug-message "e1-favored: ~a~%" e1-favored)    
    e1-favored)) 
    
(defun endpoints-known? (start end)
  (every #'move-known? (list start end)))

(defun move-unknown? (move)
  (and (null (second move)) (null (third move))))

(defun move-known? (move)
  (not (move-unknown? move)))
  
  (defun endpoints-value-difference (start end)
  (let* ((value-start (first start))
         (value-end (first end)))
    (- value-end value-start)))
    
(defun endpoints-distance (start end)
  (let* ((x-start (second start))
         (y-start (third start))
         (x-end (second end))
         (y-end (third end)))
    (- (+ (abs (- x-start x-end)) (abs (- y-start y-end))) 1)))
    
(defun endpoint-unknown-cost-estimate (val-diff)
  (let ((base (1+ val-diff)))
    (* (expt base base) 4)))
    
(defun endpoints-common-axis (start end)
  (let* ((x-start (second start))
         (y-start (third start))
         (x-end (second end))
         (y-end (third end)))
    (or (when (= x-start x-end) (list :x x-start))
        (when (= y-start y-end) (list :y y-start)))))
        
(defun error-if-duplicate-values (board-state location)
  (when *debug-mode*
    (when (duplicate-values? board-state)
      (print-the-board board-state)
      (error (format nil "ERROR: ~a Duplicate values exist!" location)))))

(defun try-dfs-search-delegator (board start end &optional move-tracker board-state attempt-tracker endpoint-tracker)
  (debug-message "SEARCH-DELEGATOR START: ~a END: ~a" start end)
  (if (endpoints-known? start end)
      (try-dfs-search board start end move-tracker board-state attempt-tracker)
      (try-dfs-search-endpoints board start end move-tracker board-state attempt-tracker endpoint-tracker)))

(defun try-dfs-prepare-attempt-tracker (board)
  (let ((attempt-tracker 
         (copy-board 
          *size* board nil
          (type-of (make-failed-attempts)) nil)))
    (dotimes (i *size*)
      (dotimes (j *size*)
        (set-pos attempt-tracker i j (make-failed-attempts))))
    attempt-tracker)) 
    
(defstruct failed-attempts
  (n)
  (e)
  (s)
  (w))
  
(defun build-dynamic-value-list (distance start direction &optional coords)
  (cond
    ((= distance 0) coords)
    ((null coords) 
     (build-dynamic-value-list
      (1- distance) start direction (list (+ start direction))))
    (t (build-dynamic-value-list
        (1- distance) start direction (append coords 
                                              (list (+ (car (reverse coords)) direction)))))))
                                              
                                    
(defun try-dfs-next-move (board attempt-tracker value end value-goal x y)
  (let* ((north (list x (1- y))) (x-north (first north)) (y-north (second north))
         (east (list (1+ x) y)) (x-east (first east)) (y-east (second east))
         (south (list x (1+ y))) (x-south (first south)) (y-south (second south))
         (west (list (1- x) y)) (x-west (first west)) (y-west (second west))
         (failed-attempts (get-pos attempt-tracker x y)))
    (debug-message "NEXT-MOVE: ~a from (~a ~a) in:~%~a" value x y attempt-tracker)
    (or (try-dfs-move-legal? board failed-attempts value :n end value-goal x-north y-north)
        (try-dfs-move-legal? board failed-attempts value :e end value-goal x-east y-east)
        (try-dfs-move-legal? board failed-attempts value :s end value-goal x-south y-south)
        (try-dfs-move-legal? board failed-attempts value :w end value-goal x-west y-west))))
        
(defun try-dfs-move-legal? (board failed-attempts value direction end value-goal x y)
  (debug-message "MOVE-LEGAL?~%failed-attempts: ~a move: (~a ~a ~a) direction: ~a~%end: ~a~%board-state: ~a"
                 failed-attempts value x y direction end board)
  (if (and (move-known? end)
           (not (try-dfs-move-misguided? value value-goal (endpoints-distance (list value x y) end)))
           (ai-move-legal? board x y)
           (value-empty? (get-pos board x y))
           (not (failed-attempt-made? failed-attempts direction)))
      (list value x y)
      (progn
        (failed-attempt-mark failed-attempts direction)
        nil)))
        
(defun try-dfs-move-misguided? (value-move value-goal distance-to-goal)
  (>= distance-to-goal (- value-goal value-move)))
  
(defun ai-move-legal? (board x y &optional (get-pos-fun #'get-pos))
  (let ((min 0)
        (max (1- *size*)))
    (unless (or (move-coord-invalid? x y min max)
                (value-set? (funcall get-pos-fun board x y)))
      (list x y))))

(defun move-coord-invalid? (x y min max)
  (some #'(lambda (coord) (range-invalid? min max coord)) (list x y)))

(defun failed-attempt-mark (failed-attempts direction)
  (ecase direction
    (:n (setf (failed-attempts-n failed-attempts) t))
    (:e (setf (failed-attempts-e failed-attempts) t))
    (:s (setf (failed-attempts-s failed-attempts) t))
    (:w (setf (failed-attempts-w failed-attempts) t))))

(defun failed-attempt-made? (failed-attempts direction)
  (ecase direction
    (:n (failed-attempts-n failed-attempts))
    (:e (failed-attempts-e failed-attempts))
    (:s (failed-attempts-s failed-attempts))
    (:w (failed-attempts-w failed-attempts))))
    
(defun attempt-tracker-move-exists (source attempt-tracker)
  (let ((failed-attempts (get-pos attempt-tracker (second source) (third source))))
    (failed-attempt-move-exists failed-attempts)))
    
(defun failed-attempt-move-exists (failed-attempts)
  (let ((directions '(:n :e :s :w)))
    (some #'null
          (mapcar  #'(lambda (direction)
                       (failed-attempt-made? failed-attempts direction))
                   directions))))
                   
(defstruct try-dfs
  (move-tracker)
  (move-trivial-tracker)
  (board-state)
  (attempt-tracker)
  (endpoint-start)
  (endpoint-end)
  (endpoint-tracker))
  
(defun board-invalid? (board)
  (or (empty-squares? board)
      (invalid-values? board)
      (missing-values? board)
      (unordered-values? board)))
    
(defun empty-squares? (board)
  (let ((values (flatten-board board)))
    (not (eq (length (remove-empty values)) (length values)))))

(defun neighbors-sequential? (neighbor1 neighbor2)
  (= 2 (abs (- neighbor1 neighbor2))))
  
(defun try-dfs-search-endpoints (board start end &optional move-tracker board-state attempt-tracker endpoint-tracker endpoint-recalculated)
  (debug-message "SEARCH-ENDPOINTS: start: ~a end: ~a~%move-tracker: ~a~%attempt-tracker: ~a~%endpoint-recalculated: ~a ~%"
                 start end move-tracker attempt-tracker endpoint-recalculated)
  (let ((unknown-identity nil)
        (origin nil)
        (goal nil)
        (value-difference nil)
        (short-solution-found nil)
        (solution-impossible nil))
    (loop do
         (multiple-value-bind (origin-result 
                               goal-result 
                               endpoint-tracker-result 
                               board-state-result 
                               unknown-identity-result
                               solution-impossible-result)
             (try-dfs-search-endpoints-prepare
              (if endpoint-recalculated
                  board-state
                  board) 
              start end endpoint-tracker)
           (when (and origin 
                      goal 
                      (or (not (equal origin origin-result))
                          (not (equal goal goal-result))))
             (setf endpoint-recalculated t))
           (setf origin origin-result)
           (setf goal goal-result)
           (setf endpoint-tracker endpoint-tracker-result)
           (setf board-state board-state-result)
           (setf unknown-identity unknown-identity-result)
           (setf value-difference (1- (endpoints-value-difference origin goal)))
           (setf solution-impossible solution-impossible-result)
           (debug-message "~%SEARCH-ENDPOINTS: ~a ~a ~a ~a ~a ~a~%~a~%~a~%~%" 
                          start end origin goal value-difference 
                          short-solution-found endpoint-tracker-result board-state)
           (debug-message "solution-impossible: ~a" solution-impossible)
           (when (not solution-impossible)
             (multiple-value-bind (move-tracker-result board-state-result attempt-tracker-result short-solution-found-result)
                 (try-dfs-search board-state origin goal
                                             (unless endpoint-recalculated move-tracker)
                                             nil
                                             (unless endpoint-recalculated attempt-tracker))
               (setf move-tracker move-tracker-result)
               (setf board-state board-state-result)
               (setf attempt-tracker attempt-tracker-result)
               (setf short-solution-found short-solution-found-result)
               (unless (or move-tracker short-solution-found)
                 (progn
                   (setf endpoint-recalculated (not (null (endpoint-tracker-invalidate-position endpoint-tracker))))
                   (debug-message "board-state:~%~a~%" board-state)
                   (debug-message "ENDPOINT-RECALCULATED: ~a~%~a~%" endpoint-tracker endpoint-recalculated))))))
         (error-if-duplicate-values board-state "TRY-DFS-SEARCH-ENDPOINTS")
       while
         (and (null move-tracker)
              (endpoint-tracker-positions-unsolved? endpoint-tracker)
              (not short-solution-found)
              (not solution-impossible)))
    (if (and (or move-tracker short-solution-found) (not solution-impossible))
        (if (eq unknown-identity :start)
            (setf move-tracker (append move-tracker (list origin)))
            (setf move-tracker (append (list goal) move-tracker)))
        (progn
          (setf board-state board)))
    (when solution-impossible
      (let ((fully-invalidated (if (move-known? origin) origin end))
            (attempt-tracker 
             (if attempt-tracker 
                 attempt-tracker 
                 (try-dfs-prepare-attempt-tracker board-state))))
        (debug-message "origin: ~a~%end: ~a~%fully-invalidated: ~a~%attempt-tracker: ~a" 
                       origin end fully-invalidated attempt-tracker)
        (attempt-tracker-mark-all-failed fully-invalidated attempt-tracker)      
        (debug-message "SOLUTION-IMPOSSIBLE: move-tracker ~a~%board-state ~a~% attempt-tracker ~a~% endpoint-tracker"
                       move-tracker board-state attempt-tracker endpoint-tracker)))
        ;(error "SOLUTION-IMPOSSIBLE")))
    (values
     move-tracker
     board-state
     attempt-tracker
     endpoint-tracker)))
     
(defun try-dfs-search-endpoints-prepare (board start end &optional endpoint-tracker)
  (let* ((unknown (copy-list (if (move-unknown? start) start end)))
         (known (copy-list (if (move-unknown? end) start end)))
         (unknown-identity (if (equal unknown start) :start :end))
         (order-ascending (plusp (endpoints-value-difference unknown known)))
         (endpoint-tracker (if (null endpoint-tracker)
                               (try-dfs-search-endpoints-endpoint-tracker board unknown known)
                               endpoint-tracker))
         (resolved (endpoint-tracker-update-unknown endpoint-tracker unknown))
         (origin (if order-ascending resolved known))
         (goal (if order-ascending known resolved))
         (board-state (copy-board *size* board))
         (solution-impossible nil))
    (debug-message "ENDPOINTS-PREPARE:~%board: ~a~%start: ~a~%end: ~a~%endpoint-tracker: ~a~%resolved: ~a~%"
                   board start end endpoint-tracker resolved)
    (if (move-known? resolved)
        (apply-solutions board-state (list resolved))
        (setf solution-impossible t))
    (endpoints-clear-invalid board-state resolved endpoint-tracker)
    (values
     origin
     goal
     endpoint-tracker
     board-state
     unknown-identity
     solution-impossible)))
     
(defun try-dfs-search-endpoints-endpoint-tracker (board unknown known)
  (let ((max-distance (1- (abs (endpoints-value-difference unknown known))))
        (endpoint-tracker
         (copy-board 
          *size* board nil
          (type-of (make-endpoint-attempt)) nil)))
    (dotimes (i *size*)
      (dotimes (j *size*)
        (let ((endpoint-attempt (make-endpoint-attempt))
              (distance (endpoints-distance known (list nil i j)))
              (value (get-pos board i j)))
          (when (and (<= distance max-distance)
                     (value-empty? value))
            (setf (endpoint-attempt-potential endpoint-attempt) t))
          (set-pos endpoint-tracker i j endpoint-attempt))))
    endpoint-tracker)) 
    
(defstruct endpoint-attempt
  (potential)
  (invalid))
  
(defun endpoint-tracker-update-unknown (endpoint-tracker unknown)
  (let* ((position-next-available (endpoint-tracker-next-available endpoint-tracker))
         (position-next-available-x (first position-next-available))
         (position-next-available-y (second position-next-available)))
    (setf (elt unknown 1) position-next-available-x)
    (setf (elt unknown 2) position-next-available-y)
    unknown))

(defun endpoint-tracker-next-available (endpoint-tracker)
  (dotimes (i *size*)
    (dotimes (j *size*)
      (let* ((endpoint-attempt (get-pos endpoint-tracker i j)))
        (when (and (endpoint-attempt-potential endpoint-attempt)
                   (not (endpoint-attempt-invalid endpoint-attempt)))
          (return-from endpoint-tracker-next-available (list i j)))))))
              
(defun endpoints-clear-invalid (board-state unknown endpoint-tracker)
  (let ((value (first unknown)))
    (dotimes (i *size*)
      (dotimes (j *size*)
        (let ((endpoint-attempt (get-pos endpoint-tracker i j)))
          (when (and (or (not (endpoint-attempt-potential endpoint-attempt))
                         (endpoint-attempt-invalid endpoint-attempt))
                     (= value (get-pos board-state i j)))
            (set-pos board-state i j 0)))))))

(defun endpoint-tracker-invalidate-position (endpoint-tracker)
  (dotimes (i *size*)
    (dotimes (j *size*)
      (let* ((endpoint-attempt (get-pos endpoint-tracker i j)))
        (when (and (endpoint-attempt-potential endpoint-attempt)
                   (not (endpoint-attempt-invalid endpoint-attempt)))
          (setf (endpoint-attempt-invalid endpoint-attempt) t)
          (return-from endpoint-tracker-invalidate-position (list i j)))))))
          
(defun endpoint-tracker-positions-unsolved? (endpoint-tracker)
  (dotimes (i *size*)
    (dotimes (j *size*)
      (let* ((endpoint-attempt (get-pos endpoint-tracker i j)))
        (when (and (endpoint-attempt-potential endpoint-attempt)
                   (not (endpoint-attempt-invalid endpoint-attempt)))
          (when (null (endpoint-attempt-invalid endpoint-attempt))
            (return-from endpoint-tracker-positions-unsolved? t)))))))
            
(defun failed-attempt-mark-next (failed-attempts)
  (let ((directions '(:n :e :s :w)))
    (mapcar #'(lambda (direction)
                (when (not (failed-attempt-made? failed-attempts direction))
                  (failed-attempt-mark failed-attempts direction)
                  (return-from failed-attempt-mark-next direction)))
            directions)
    nil))
    
 (defun direction-neighbor (source neighbor)
  (let* ((x-source (second source))
         (y-source (third source))
         (x-neighbor (second neighbor))
         (y-neighbor (third neighbor))
         (offset (list (- x-neighbor x-source) (- y-neighbor y-source)))
         (n (list 0 -1))
         (e (list 1 0))
         (s (list 0 1))
         (w (list -1 0)))
    (cond ((equal offset n) :n)
          ((equal offset e) :e)
          ((equal offset s) :s)
          ((equal offset w) :w))))   

(defun invalid-values? (board)
  (consp (invalid-values board)))
  
(defun invalid-values (board)
  "Returns those valuse in BOARD that are too large or too small for 
   the given board definition."
  (make-board-traversal-expression invalid-value))
  

(defmacro make-board-traversal-expression (fun)
  `(let ((values ()))
     (dotimes (y *size*)
       (dotimes (x *size*)
         (let ((value (,fun board x y)))
           (when (integerp value)
             (setf values (append values (list value)))))))
         values))
         
(defun invalid-value (board x y)
  (let ((value (get-pos board x y))
        (min-value 1)
        (max-value *num-squares*))
     (when (or (> value max-value)
               (< value min-value))
       value)))
       
(defun missing-values (board)
  (let ((values (flatten-board board))
        (target-values (make-target-values)))
     (set-difference target-values values)))

(defun missing-values? (board)
  (consp (missing-values board)))
  
(defun make-target-values ()
  (let ((target-values ()))
    (dotimes (i *num-squares*)
      (setf target-values (append target-values (list (1+ i)))))
      target-values))
 
(defun unordered-values? (board)
  (consp (unordered-values board)))

(defun unordered-values (board)
  (make-board-traversal-expression check-neighbors))

(defun check-neighbors (board x y)
  (let ((endpoint-upper *num-squares*)
        (endpoint-lower 1)
        (value (get-pos board x y))
        (neighbors (get-neighbors board x y))
        (neighbors-ok t))
    (cond 
      ((eq endpoint-lower endpoint-upper)
       (setf neighbors-ok (eq value endpoint-lower)))
      ((eq value endpoint-lower)
       (setf neighbors-ok (has-neighbor-next value neighbors)))
      ((eq value endpoint-upper)
       (setf neighbors-ok (has-neighbor-prev value neighbors)))
      (t
       (setf neighbors-ok (and (has-neighbor-next value neighbors) 
                               (has-neighbor-prev value neighbors)))))
    (unless neighbors-ok value)))
    
(defun has-neighbor-next (value neighbors)
  (check-neighbor (find (1+ value) neighbors)))
  
(defun check-neighbor (neighbor)
  (and (integerp neighbor) (> neighbor 0)))
  
  
(defun has-neighbor-prev (value neighbors)
  (check-neighbor (find (1- value) neighbors)))

(defun has-neighbor-next (value neighbors)
  (check-neighbor (find (1+ value) neighbors)))
  
(defun complete-sequence (neighbor1 neighbor2)
  (if (< neighbor1 neighbor2)
      (1+ neighbor1)
      (1+ neighbor2)))
 
(defun easy-solution-multi-cell-sequence (start end common-axis distance)
  (let* ((x-coord-start (second start))
         (y-coord-start (third start))
         (x-coord-end (second end))
         (y-coord-end (third end))
         (value-start (first start))
         (direction-positive 
          (ecase (first common-axis)
            (:x (plusp (- y-coord-end y-coord-start)))
            (:y (plusp (- x-coord-end x-coord-start)))))
         (coord-start 
          (ecase (first common-axis)
            (:x (if direction-positive 
                    (min y-coord-start y-coord-end) 
                    (max y-coord-start y-coord-end)))
            (:y (if direction-positive 
                    (min x-coord-start x-coord-end) 
                    (max x-coord-start x-coord-end)))))
         (solution-values 
          (build-dynamic-value-list distance value-start 1))
         (solution-dynamic-coords 
          (build-dynamic-value-list distance coord-start (if direction-positive 1 -1)))
         (solution-static-coords 
          (build-static-value-list distance (second common-axis)))
         (x nil)
         (y nil))
    (ecase (first common-axis)
      (:x (progn
            (setf x solution-static-coords)
            (setf y solution-dynamic-coords)))
      (:y (progn
            (setf x solution-dynamic-coords)
            (setf y solution-static-coords))))
    (mapcar #'(lambda (value x y) (list value x y)) 
            solution-values
            x
            y)))
            
(defun build-static-value-list (distance coord &optional coords)
  (cond
    ((= distance 0) coords)
    (t (build-static-value-list (1- distance) coord (append coords (list coord))))))
    
(defun get-max-neighbors (position-type)
  (getf *position-types-neighbors* position-type))
  
(defun attempt-tracker-mark-all-failed (square attempt-tracker)
  (let ((failed-attempts (get-pos attempt-tracker (second square) (third square)))
        (directions '(:n :e :s :w)))
    (mapcar #'(lambda (direction)
                (failed-attempt-mark failed-attempts direction))
            directions)))

  
        
    
