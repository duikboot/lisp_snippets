(setf words
      '((one un)
        (two deux)
        (three trois)
        (four quatre)
        (five cinq)))

(defun translate (x)
  (second (assoc x words)))


(setf things
      '((object1 large green shiny cube)
        (object2 small red dull metal cube)))

(defun description (x)
  (rest (assoc x things)))


(defun differences (x y)
  (set-exclusive-or (description x)
                    (description y)))

(defun all-odd (x)
  (every #'oddp x))
