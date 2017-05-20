
(defun bin-search (neelde haystack)
  (let* ((index (ash (length haystack) -1))
         (res (nth index haystack)))
    (cond ((= res neelde) (values t neelde))
          ((> neelde (first (last haystack))) (values nil neelde))
          ((< neelde (first haystack)) (values nil  neelde))
          ((< res neelde) (bin-search neelde (subseq haystack index)))
          ((> res neelde) (bin-search neelde (subseq haystack 0 index))))))
