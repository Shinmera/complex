#|
 This file is a part of Complex
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.complex)

(defun bubble-sort (sequence predicate &key (key #'identity))
  (loop for sorted = T
        do (do-sequence ((index item) sequence :start 1)
             (when (funcall predicate
                            (funcall key item)
                            (funcall key (elt sequence (1- index))))
               (setf sorted NIL)
               (rotatef (elt sequence index)
                        (elt sequence (1- index)))))
        until sorted)
  sequence)

(defun insertion-sort (sequence predicate &key (key #'identity))
  (flet ((descend (from)
           (loop for index downfrom from above 0
                 until (funcall predicate
                                (funcall key (elt sequence (1- index)))
                                (funcall key (elt sequence index)))
                 do (rotatef (elt sequence (1- index))
                             (elt sequence index)))))
    (do-sequence ((index item) sequence)
      (descend index)))
  sequence)

(defun selection-sort (sequence predicate &key (key #'identity))
  (do-sequence ((index item) sequence)
    (let ((minimum index)
          (minitem item))
      (do-sequence ((index item) sequence :start (1+ index))
        (when (funcall predicate
                       (funcall key item)
                       (funcall key minitem))
          (setf minimum index
                minitem item)))
      (unless (= minimum index)
        (rotatef (elt sequence index)
                 (elt sequence minimum)))))
  sequence)


(defun merge-sort (sequence predicate &key (key #'identity) (start 0) end target)
  (let* ((end (or end (length sequence)))
         (target (or target (make-like sequence (- end start)))))
    (flet ((merge-into (start middle end)
             (loop with left = start
                   with right = middle
                   for i from start below end
                   do (setf (elt target i)
                            (if (and (< left middle)
                                     (or (= right end)
                                         (funcall predicate
                                                  (funcall key (elt sequence left))
                                                  (funcall key (elt sequence right)))))
                                (prog1 (elt sequence left)
                                  (incf left))
                                (prog1 (elt sequence right)
                                  (incf right)))))))
      (when (< 1 (- end start))
        (let ((middle (middle start end)))
          (merge-sort sequence predicate :key key :start start :end middle :target target)
          (merge-sort sequence predicate :key key :start middle :end end :target target)
          (merge-into start middle end)
          (copy-into target sequence :start start :end end))))
    sequence))

(defun heap-sort (sequence predicate &key (key #'identity))
  (let ((end (length sequence)))
    (flet ((trickle (start end)
             (loop for index = start then swap
                   for swap = index
                   for left = (1+ (* index 2))
                   for right = (1+ left)
                   do (when (and (< left end)
                                 (not (funcall predicate
                                               (funcall key (elt sequence left))
                                               (funcall key (elt sequence swap)))))
                        (setf swap left))
                      (when (and (< right end)
                                 (not (funcall predicate
                                               (funcall key (elt sequence right))
                                               (funcall key (elt sequence swap)))))
                        (setf swap right))
                      (if (/= swap index)
                          (rotatef (elt sequence swap)
                                   (elt sequence index))
                          (return)))))
      ;; Create heap order
      (loop for index downfrom (middle 0 end) to 0
            do (trickle index end))
      (print sequence)
      ;; Sort
      (decf end)
      (loop while (< 0 end)
            do (rotatef (elt sequence end)
                        (elt sequence 0))
               (decf end)
               (trickle 0 end))))
  sequence)

(defun quick-sort (sequence predicate &key (key #'identity) (start 0) (pivot-func #'middle) end)
  ;; For ease of writing we use inclusive ranges for start/end for once.
  (let ((end (or end (1- (length sequence)))))
    (flet ((partition (start end)
             (let* ((pivot (funcall pivot-func start end))
                    (pivot-item (elt sequence pivot)))
               (rotatef (elt sequence pivot)
                        (elt sequence end))
               (let ((store start))
                 (do-sequence ((index item) sequence :start start :end end)
                   (when (funcall predicate
                                  (funcall key item)
                                  (funcall key pivot-item))
                     (rotatef (elt sequence index)
                              (elt sequence store))
                     (incf store)))
                 (rotatef (elt sequence store)
                          (elt sequence end))
                 store))))
      (when (< start end)
        (let ((index (partition start end)))
          (quick-sort sequence predicate :key key :start start :end (1- index))
          (quick-sort sequence predicate :key key :start (1+ index) :end end)))))
  sequence)

(defun bucket-sort (sequence predicate &key (key #'identity) (bucket-count 10) (sub-sort #'insertion-sort))
  (let ((buckets (make-array bucket-count :initial-element ())))
    (do-sequence (item sequence)
      (push item (aref buckets (mod (funcall key item) bucket-count))))
    (loop for i from 0 below bucket-count
          for bucket = (aref buckets i)
          do (setf (aref buckets i)
                   (funcall sub-sort bucket predicate :key key)))
    (etypecase sequence
      (list (loop with bucket = 0
                  for cons on sequence
                  do (loop until (aref buckets bucket)
                           do (incf bucket))
                     (setf (car cons)
                           (pop (aref buckets bucket)))))))
  sequence)

(defun radix-sort (sequence predicate length &key (key #'identity) (base 10) (bucket-sub-sort #'insertion-sort))
  (dotimes (index (1- length))
    (setf sequence (bucket-sort sequence predicate
                                :key (lambda (item)
                                       (ldb (byte base index)
                                            (funcall key item)))
                                :bucket-count base
                                :sub-sort bucket-sub-sort)))
  sequence)
