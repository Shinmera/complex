(in-package #:org.shirakumo.complex)

(defun linear-find (item sequence &key (key #'identity) (test #'eql) (start 0) end)
  (do-sequence (current sequence :start start :end end)
    (when (funcall test
                   (funcall key current)
                   item)
      (return current))))

(defun binary-find (item sequence &key (order '<) (key #'identity) (test #'eql) (start 0) end)
  (let ((end (or end (length sequence))))
    (assert (sorted-p sequence order :start start :end end)
            () "SEQUENCE most be sorted by ~a" order)
    (loop
      (let* ((middle (middle start end))
             (found (funcall key (elt sequence middle))))
        (cond
          ((funcall test found item)
           (return found))
          ((= start (1- end))
           (return NIL))
          ((funcall order found item)
           (setf start middle))
          (T
           (setf end middle)))))))
