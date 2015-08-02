(defun atom? (x)
  "Is it a atom?"
  (not (listp x)))

(defun lat? (l)
  "Is it a list of atoms?"
  (cond ((null l) t)
        ((atom? (car l)) (lat? (cdr l)))
        (t nil)))

(defun member? (a lat)
  "Is 'a' a member of lat?"
  (cond ((null lat) nil)
        ((or (equalp a (car lat)) (member? a (cdr lat))))))

        ; ((equalp a (car lat)) t)
        ; ((member? a (cdr lat)))))

(defun rember (a lat)
  "Replace first member of lat with a"
  (cond ((null lat) (quote ()))
        ((equalp a (car lat)) (cdr lat))
        (t (cons (car lat) (rember a (cdr lat))))))

(defun multirember (a lat)
  "Replace all members of lat with a"
  (cond ((null lat) (quote ()))
        ((equalp a (car lat)) (multirember a (cdr lat)))
        (t (cons (car lat) (rember a (cdr lat))))))

(defun firsts (lat)
  "Make a lists of first elements of list of lists"
  (cond ((null lat) (quote ()))
        (t (cons
             (car (car lat)) (firsts (cdr lat))))))

(defun insertR (new old lat)
  "Insert new right of first occurrence old in lat"
  (cond
    ((null lat) (quote ()))
    ((equalp old (car lat)) (cons old (cons new (cdr lat))))
    (t (cons (car lat) (insertR new old (cdr lat))))))


(defun multiinsertR (new old lat)
  "Insert new right of all occurrences old in lat"
  (cond
    ((null lat) (quote ()))
    ((equalp old (car lat)) (cons old
                                  (cons new (multiinsertR new old (cdr lat)))))
    (t (cons (car lat) (multiinsertR new old (cdr lat))))))

(defun insertL (new old lat)
  "Insert new lift of first occurrence old in lat"
  (cond
    ((null lat) (quote ()))
    ((equalp old (car lat)) (cons new lat))
    (t (cons (car lat) (insertL new old (cdr lat))))))

(defun multiinserL (new old lat)
  "Insert new lift of all occurrences old in lat"
  (cond
    ((null lat) (quote ()))
    ((equalp old (car lat)) (cons new
                                  (cons old (multiinserL new old (cdr lat)))))
    (t (cons (car lat) (multiinserL new old (cdr lat))))))

(defun substr (new old lat)
  "Replace first old with new"
  (cond
    ((null lat) (quote ()))
    ((equalp old (car lat)) (cons new (cdr lat)))
    (t (cons (car lat) (substr new old (cdr lat))))))

(defun multisubstr (new old lat)
  "Replace all old with new"
  (cond
    ((null lat) (quote ()))
    ((equalp old (car lat)) (cons new (multisubstr new old (cdr lat))))
    (t (cons (car lat) (multisubstr new old (cdr lat))))))

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

(defun o+ (n m)
  (cond
    ((zerop m) n)
    (t (1+ (o+ n (1- m))))))

(defun o- (n m)
  (cond
    ((zerop m) n)
    (t (1- (o- n (1- m))))))

(defun addtup (tup)
  (cond
     ((null tup) 0)
     (t (o+ (car tup) (addtup (cdr tup))))))

(defun x (n m)
  (cond ((zerop m) 0)
        (t (+ n (* (x n (1- m)))))))
