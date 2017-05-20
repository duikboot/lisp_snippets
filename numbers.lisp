;;;; list every number in a list

(defun list-numbers (lst)
  (cond ((null lst) '())
        ((numberp (first lst))
         (cons (first lst) (list-numbers (rest lst))))
        ((atom (first lst)) (list-numbers (rest lst)))
        ((consp (first lst))
         (cons (list-numbers (first lst)) (list-numbers (rest lst))))
        (t (list-numbers (rest lst)))))
