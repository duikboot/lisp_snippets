(load "~/.sbclrc")
(ql:quickload :hunchentoot)
(use-package :hunchentoot)

(defun main ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080))
  (sb-thread:join-thread (find-if
                          (lambda (th)
                            (string= (sb-thread:thread-name th)  "hunchentoot-listener-*:8080"))
                          (sb-thread:list-all-threads))))
