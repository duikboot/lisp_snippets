(defun schwartzian-transform (list costly-function predicate)
  "sort a list of objects over the value of a function applied to them,
  by applying the Schwartzian Transform
  (https://en.wikipedia.org/wiki/Schwartzian_transform)
  the parameters are the list, the function, and the predicate for the sort."
  (mapcar #'cdr
          (stable-sort (mapcar (lambda (x)
                                 (cons (funcall costly-function x) x))
                               list)
                       predicate
                       :key #'car)))

(require :sb-posix)
(schwartzian-transform
  (directory "/etc/*")
  (lambda (x) (sb-posix:stat-mtime (sb-posix:stat x)))
  #'<=)
