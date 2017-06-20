
(defgeneric withdraw (account amount)
  (:documentation "Withdraw account with amount,
                   Signal error if current balance is less than amount"))

(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))


(defmethod withdraw ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))

;; Closure
(let ((l-n 0))
  (defun plus()
    (incf l-n))
  (defun minus()
    (decf l-n)))

(let ((x 0))
  (defun adder (y)
    (setq x (+ x y))))
