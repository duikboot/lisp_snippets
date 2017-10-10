;;;; list every number in a list

(defun list-numbers (lst)
  (cond ((null lst) '())
        ((numberp (first lst))
         (cons (first lst) (list-numbers (rest lst))))
        ((atom (first lst)) (list-numbers (rest lst)))
        ((consp (first lst))
         (cons (list-numbers (first lst)) (list-numbers (rest lst))))
        (t (list-numbers (rest lst)))))

(defun double-odds (l)
 (let ((f (first l))
       (r (rest l)))
  (cond
    ((null l) nil)
    ((oddp f) (cons (* 2 f) (double-odds r)))
    (t (cons f (double-odds r))))))


; call vlime#plugin#ConnectREPL('127.0.0.1', 4005)
