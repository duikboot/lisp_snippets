
(defclass lock ()
  ((name :initarg :name :reader lock-name))
  (:documentation "The foundation of all locks."))


(defclass null-lock (lock)
  ()
  (:documentation "A lock that's always free."))


(defclass simple-lock (lock)
  ((owner :initform nil :accessor lock-owner))
  (:documentation "A lock that's either free or busy."))

(defun make-null-lock (name)
  (make-instance 'null-lock :name name))

(defun make-simple-lock (name)
  (make-instance 'simple-lock :name name))

(setq nl (make-null-lock "Null lock"))
(setq sl (make-simple-lock "Simple lock"))

(type-of nl)


