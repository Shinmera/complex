#|
 This file is a part of Complex
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.complex)

(defun map-sequence (sequence function &key (start 0) end)
  (let ((end (or end (length sequence))))
    (etypecase sequence
      (list
       (loop for item in sequence
             for i from 0 below end
             when (<= start i)
             do (funcall function i item)))
      (vector
       (loop for i from start below end
             for item = (elt sequence i)
             do (funcall function i item))))))

(defmacro do-sequence ((var sequence &key (start 0) end) &body body)
  (let ((var (if (listp var)
                 var
                 (list (gensym "i") var))))
    `(block NIL
       (map-sequence ,sequence (lambda ,var
                                 (declare (ignorable ,(first var) ,(second var)))
                                 ,@body)
                     :start ,start :end ,end))))

(defun sorted-p (sequence predicate &key (start 0) end)
  (let ((prev (elt sequence start)))
    (do-sequence (item sequence :start (1+ start) :end end)
      (if (funcall predicate prev item)
          (setf prev item)
          (return-from sorted-p NIL)))
    sequence))

(defun make-like (sequence &optional (size (length sequence)))
  (etypecase sequence
    (list (make-list size))
    (vector (make-array size))))

(defun copy-into (from to &key (start 0) end)
  (let ((end (or end (length to))))
    (do-sequence ((index item) from :start start :end end)
      (setf (elt to index) item)))
  to)

(defun test (a b predicate &optional (key #'identity))
  (funcall predicate
           (funcall key a)
           (funcall key b)))

(defun middle (start end &optional (round #'floor))
  (funcall round (+ start (/ (- end start) 2))))

(defun empty-p (sequence)
  (etypecase sequence
    (cons T)
    (null NIL)
    (vector (/= 0 (length sequence)))))

(defun minimize (sequence &key (key #'identity))
  (unless (empty-p sequence)
    (let* ((min (elt sequence 0))
           (mineff (funcall key min)))
      (do-sequence (el sequence :start 1)
        (let ((eleff (funcall key el)))
          (when (< eleff mineff)
            (setf min el mineff eleff))))
      (values min mineff))))
