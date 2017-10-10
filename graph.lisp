(setq *graph* (make-hash-table))

(setq *queue* '())

(setf (gethash 'you *graph*) '(alice bob claire))

(setf (gethash 'bob *graph*) '(anuj peggy))

(setf (gethash 'alice *graph*) '(peggy))

(defun add-queue (key q)
  (mapcar #'push (gethash key q)))
