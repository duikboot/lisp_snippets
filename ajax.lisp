
(ql:quickload '(:hunchentoot :cl-who :parenscript :smackjack))

(defpackage :jank-repl
  (:use :cl :hunchentoot :cl-who :parenscript :smackjack))
(in-package :jank-repl)

; Allow cl-who and parenscript to work together
(setf *js-string-delimiter* #\")

(defparameter *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/repl-api"))

(defun-ajax echo (data) (*ajax-processor* :callback-data :response-text)
  (concatenate 'string "echo: " data))

(define-easy-handler (repl :uri "/repl") ()
  (with-html-output-to-string (s)
    (:html
      (:head
        (:title "Jank REPL")
        (str (generate-prologue *ajax-processor*))
        (:script :type "text/javascript"
          (str
            (ps
              (defun callback (response)
                (alert response))
              (defun on-click ()
                (chain smackjack (echo (chain document
                                              (get-element-by-id "data")
                                              value)
                                       callback)))))))
      (:body
        (:p
          (:input :id "data" :type "text"))
        (:p
          (:button :type "button"
                   :onclick (ps-inline (on-click))
                   "Submit!"))))))

(defparameter *server*
  (start (make-instance 'easy-acceptor :address "localhost" :port 8080)))

(setq *dispatch-table* (list 'dispatch-easy-handlers
                             (create-ajax-dispatcher *ajax-processor*)))
