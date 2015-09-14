(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro check (&body form)
  `(progn
     ,@(loop for f in form collect `(report-result ,f ',f))))

(defun tests ()
  (check
    (= (+ 1 2 3) 6)
    (= (+ 1 -2) -1)
    (= (+ 2 -2) -1)
    (= (+ 3 -2) -1)
    (= (+ 1 2) 3)))
