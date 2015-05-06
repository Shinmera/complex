#|
 This file is a part of Complex
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.complex)

(defclass disjoint-set-cell ()
  ((parent :initarg :parent :accessor parent)
   (value :initarg :value :accessor value)
   (rank :initarg :rank :accessor rank))
  (:default-initargs
   :value (error "VALUE required.")
   :rank 1))

(defmethod initialize-instance :after ((cell disjoint-set-cell) &key)
  (unless (slot-boundp cell 'parent)
    (setf (parent cell) cell)))

(defmethod print-object ((cell disjoint-set-cell) stream)
  (print-unreadable-object (cell stream :type T)
    (format stream "~s" (value cell))))

(defmethod union-set ((a disjoint-set-cell) (b disjoint-set-cell))
  (let ((a (find-set a))
        (b (find-set b)))
    (unless (eql a b)
      (cond ((< (rank a) (rank b))
             (union b a))
            (T
             (when (= (rank a) (rank b))
               (incf (rank a)))
             (setf (parent b) a))))))

(defmethod find-set ((a disjoint-set-cell))
  (unless (eql a (parent a))
    (setf (parent a) (find-set (parent a))))
  (parent a))

(defclass edge ()
  ((vertices :initarg :vertices :accessor vertices)
   (weight :initarg :weight :accessor weight))
  (:default-initargs
   :vertices (cons NIL NIL)
   :weight 0))

(defmethod print-object ((edge edge) stream)
  (print-unreadable-object (edge stream :type T)
    (format stream "~a ~d"
            (vertices edge) (weight edge))))

(defun make-edge-map (edges)
  (loop for (a b w) in edges
        collect (make-instance 'edge :vertices (cons a b) :weight w)))

(defun kruskal (vertices edges)
  (let ((edges (sort (copy-list edges) #'< :key #'weight))
        ;; Create an associated disjoint set cell for each vertex.
        (cells (let ((table (make-hash-table :test 'eql)))
                 (dolist (vertex vertices table)
                   (setf (gethash vertex table) (make-instance 'disjoint-set-cell :value vertex))))))
    (loop for edge in edges
          for a = (gethash (car (vertices edge)) cells)
          for b = (gethash (cdr (vertices edge)) cells)
          unless (eql (find-set a)
                      (find-set b))
          collect (progn (union-set a b)
                         edge))))
