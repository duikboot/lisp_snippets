(defparameter *number* 0)
(defparameter *small* 1)
(defparameter *big* 100)

(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

(defun smaller ()
  (setf *big* (1+ (guess-my-number)))
  (print *big*)
  (guess-my-number))

(defun bigger ()
  (setf *small* (1- (guess-my-number)))
  (print *small*)
  (guess-my-number))

(defun start-over ()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (defparameter *number* 100)
  (defparameter *number* (random 100))
  (guess-my-number)
  (main))

(defun q(a)
  (print "yes")
  (print a)
  (quit)
  )

(defun main ()
  ; (if argv
  ;   (setf *number* argv)
  (setf *number* (random 100))
  ; )
  (print *number*)
  (loop 
    (if (= *number* (guess-my-number))
      (q *number*)
      (if (< *number* (guess-my-number))
        (smaller)
        (bigger)
        ))))
