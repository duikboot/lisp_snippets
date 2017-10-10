;;;; Exercise 1.1

(defparameter *suffixes*
  '(MD Jr. III))

(defun last-name (name)
  "Select the last name from a name represented as list."
  (let ((last-word (first (last name))))
    (if (member last-word *suffixes*)
      (last-name (butlast name))
      name)))

(defparameter *titles*
  '(Mr Miss Mrs Ms Dr))

(defun first-name (name)
  "Select first name represented as a list"
  (let ((first-word (first name)))
    (if (member first-word *titles*)
      (first-name (rest name))
      first-word)))

(defun power (n m)
  "Raise n to the power m."
  (cond
    ((= m 0) 1)
    (t (* n (power n (- m 1))))))

(defun power-more-efficient (n m)
  "Raise n to the power m."
  (cond
    ((= m 0) 1)
    ((evenp m)
     (let ((m/2 (/ m 2)))
       (power-more-efficient (* n n) m/2)))
    (t (* n (power-more-efficient n (- m 1))))))

(defun count-atoms (l)
  (cond
     ((null l) 0)
     ((atom l) 1)
     (t (+ (count-atoms (car l))
           (count-atoms (cdr l))))))

(defun dot-product (l1 l2)
  (reduce #'+ (mapcar #'* l1 l2)))
