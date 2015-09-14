(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro check (&body form)
  `(combine-results
     ,@(loop for f in form collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun test-+ ()
  (check
    (= (+ 1 2 3) 6)
    (= (+ 1 -2) -1)
    (= (+ 2 -2) -1)
    (= (+ 3 -2) -1)
    (= (+ 1 2) 3)))

(defun test-* ()
  (check
    (= (* 1 2 3) 6)
    (= (* 1 -2) -2)
    (= (* 2 -2) -4)
    (= (* 3 -2) -1)
    (= (* 1 2) 3)))

(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
