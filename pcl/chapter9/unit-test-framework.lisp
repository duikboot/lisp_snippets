(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defmacro check (form)
  `(report-result ,form ',form))


(defun tests ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ 1 -2) -1))
  (check (= (- 1 2) 0)))
