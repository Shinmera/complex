#|
 This file is a part of Complex
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.complex)

(defclass dijkstra-node (fibonacci-heap-cell)
  ((neighbors :initarg :neighbors :accessor neighbors)
   (predecessor :initform NIL :accessor predecessor)
   (name :initarg :name :accessor name))
  (:default-initargs
   :name NIL
   :neighbors ()
   :value MOST-POSITIVE-FIXNUM))

(defmethod print-object ((node dijkstra-node) stream)
  (print-unreadable-object (node stream :type T)
    (format stream "~a" (name node))))

(defmethod distance ((node dijkstra-node))
  (value node))

(defmethod (setf distance) (value (node dijkstra-node))
  (setf (value node) value))

(defun dijkstra (start end)
  (let ((unvisited (make-instance 'fibonacci-heap)))
    (labels ((traverse (node)
               (loop for (neighbor . weight) in (neighbors node)
                     do (insert neighbor unvisited)
                        (traverse neighbor))))
      (traverse start))
    (setf (distance start) 0)
    (loop for node = start
          then (nth-value 1 (delete-minimum unvisited))
          while node
          do (loop for (neighbor . weight) in (neighbors node)
                   for tentative-distance = (+ weight (distance node))
                   do (when (< tentative-distance (distance neighbor))
                        (decrease-key neighbor tentative-distance)
                        (setf (predecessor neighbor)
                              (cons node weight)))))
    (let ((path (list (list end))))
      (loop for vertex = (predecessor end)
            then (predecessor (car vertex))
            do (setf (cdr (first path)) (cdr vertex))
            until (eql (car vertex) start)
            do (push (list (car vertex)) path))
      path)))

(defun make-dijkstra-graph (edges)
  (let ((vertices (make-hash-table :test 'equal)))
    (flet ((vert (name)
             (or (gethash name vertices)
                 (setf (gethash name vertices)
                       (make-instance 'dijkstra-node :name name)))))
      (loop for (from to weight) in edges
            do (push (cons (vert to) weight)
                     (neighbors (vert from)))))
    vertices))

(defmacro with-dijkstra (start end &body edges)
  (let ((vertices (gensym "VERTICES")))
    `(let ((,vertices (make-dijkstra-graph ',edges)))
       (dijkstra (gethash ',start ,vertices)
                 (gethash ',end ,vertices)))))
