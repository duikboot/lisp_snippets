(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  )
