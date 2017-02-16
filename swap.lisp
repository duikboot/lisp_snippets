
(defun swap (x y list &key (test #'eql))
  (cond
    ((null list) '())
    ((funcall test x (first list))
     (cons y (swap x y (rest list) :test test)))
    (t
      (cons (first list) (swap x y (rest list) :test test)))))

(defun swap-iter (x y list &key (test #'eql))
  (let ((result nil))
    (dolist (item list)
      (if (funcall test item x) (push y result) (push item result)))
    (reverse result)))
