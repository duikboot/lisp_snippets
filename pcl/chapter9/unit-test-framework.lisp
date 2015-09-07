(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defmacro check (&body form)
  `(progn
     ,@(loop for f in form collect `(report-result ,f ',f))))


(defun tests ()
  (check
    (= (+ 1 2 3) 6)
    (= (+ 1 -2) -1)
    (= (+ 2 -2) -1)
    (= (+ 1 2) 3)))
