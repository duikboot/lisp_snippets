(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *word-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))
