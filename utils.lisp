(defun atom? (x)
  (not (listp x)))

(defun lat? (l)
  (cond ((null l) t)
        ((atom? (car l)) (lat? (cdr l)))
        (t nil)))

(defun member? (a l)
  (cond ((null l) nil)
        ((or (equalp a (car l)) (member? a (cdr l))))))

        ; ((equalp a (car l)) t)
        ; ((member? a (cdr l)))))


(defun rember (a l)
  (cond ((null l) (quote ()))
        ((equalp a (car l)) (cdr l))
        (t (cons (car l) (rember a (cdr l))))))

(defun multirember (a l)
  (cond ((null l) (quote ()))
        ((equalp a (car l)) (multirember a (cdr l)))
        (t (cons (car l) (rember a (cdr l))))))

(defun firsts (l)
  (cond ((null l) (quote ()))
        (t (cons
             (car (car l)) (firsts (cdr l))))))

(defun insertR (new old lat)
  (cond
    ((null lat) (quote ()))
    ((equalp old (car lat)) (cons old (cons new (cdr lat))))
    (t (cons (car lat) (insertR new old (cdr lat))))))


(defun multiinsertR (new old lat)
  (cond
    ((null lat) (quote ()))
    ((equalp old (car lat)) (cons old
                                  (cons new (multiinsertR new old (cdr lat)))))
    (t (cons (car lat) (multiinsertR new old (cdr lat))))))

(defun insertL (new old lat)
  (cond
    ((null lat) (quote ()))
    ((equalp old (car lat)) (cons new lat))
    (t (cons (car lat) (insertL new old (cdr lat))))))

(defun multiinserL (new old lat)
  (cond
    ((null lat) (quote ()))
    ((equalp old (car lat)) (cons new
                                  (cons old (multiinserL new old (cdr lat)))))
    (t (cons (car lat) (multiinserL new old (cdr lat))))))

(defun substr (new old lat)
  (cond
    ((null lat) (quote ()))
    ((equalp old (car lat)) (cons new (cdr lat)))
    (t (cons (car lat) (substr new old (cdr lat))))))

(defun substr2 (new old1 old2 lat)
  "Replace either old1 or old2 with new"
  (cond
    ((null lat) (quote ()))

    ((or (equalp old1 (car lat))
         (equalp old2 (car lat)))
     (cons new (cdr lat)))

    (t (cons (car lat) (substr2 new old1 old2 (cdr lat))))))

(defun xx (&rest numbers)
  (cond ((null numbers) 1)
    (t (*(car numbers) (apply #'xx (cdr numbers))))))
