
;; question 7

(defun summit (lst)
  (let ((l (remove nil lst)))
    (apply #'+ l)))
