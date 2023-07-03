(in-package #:org.shirakumo.complex)

(defun median (sequence &key (predicate #'<) (key #'identity))
  (let ((sorted (insertion-sort sequence predicate :key key)))
    (elt sorted (middle 0 (length sequence)))))

(defun median-of-medians (sequence &key (predicate #'<) (key #'identity) (median-count 5))
  (let ((medians (make-array median-count))
        (partition-size (floor (/ (length sequence) median-count))))
    (loop for i from 0 below median-count
          for partition = (subseq sequence
                                  (* i partition-size)
                                  (min (* (1+ i) partition-size) (length sequence)))
          do (setf (elt medians i)
                   (median partition :predicate predicate :key key)))
    (median medians)))
