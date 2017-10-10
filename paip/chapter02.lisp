;;;; Exercise 2

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))

(defun random-elt (choices)
  (elt choices (random (length choices))))

