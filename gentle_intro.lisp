(defun myfun (a b)
  (list (list a) b))

(defun my-abs (x)
  (if (< x 0)
    (- x) x))

(defun evenize (x)
  (if (oddp x)
    (1+ x)
    x))

(defun my-compare (x y)
  (cond ((equal x y) 'numbers-are-the-same)
  ((> x y) 'first-is-bigger)
  ((< x y) 'second-is-bigger)))


(defun is-in (place)
  (cond ((equal place 'london) 'england)
        ((equal place 'beijin) 'china)
        ((equal place 'amsterdam) 'the-netherlands)
        ((equal place 'paris) 'france)
        (T 'no-freaking-idea)))

(defun small-positive-oddp (x)
  (and (< x 100)
       (> x 0)
       (oddp x)))

(defun how-alike (a b)
  (cond
    ((equal a b) 'the-same)
    ((and (oddp a) (oddp b)) 'both-odd)
    ((and (not (oddp a))
            (not (oddp b)))
            'both-even)
    ((and (< a 0)
            (< b 0))
            'both-negative)
    (t 'not-alike)))

(defun same-sign (x y)
  (or (and (zerop x)(zerop y)
           (and (> x 0)(> y 0))
           (and (< x 0)(< y 0))
           )))

(defun play (x y)
  (cond
    ((and (equal x 'scissors)(equal y 'rock)) 'second-wins)
    ((and (equal x 'scissors)(equal y 'paper)) 'firs-wins)
    ((and (equal y 'paper)(equal x 'scissors)) 'second-wins)))
