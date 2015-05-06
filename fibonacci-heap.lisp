#|
 This file is a part of Complex
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.complex)

(defclass fibonacci-heap ()
  ((child :initarg :child :accessor child))
  (:default-initargs
   :child NIL))

(defclass fibonacci-heap-cell (fibonacci-heap)
  ((parent :initarg :parent :accessor parent)
   (left :initarg :left :accessor left)
   (right :initarg :right :accessor right)
   (rank :initarg :rank :accessor rank)
   (marker :initarg :marker :accessor marker)
   (value :initarg :value :accessor value))
  (:default-initargs
   :parent NIL
   :rank 0
   :marker NIL
   :value (error "VALUE required.")))

(defmethod initialize-instance :after ((cell fibonacci-heap-cell) &key)
  (unless (slot-boundp cell 'left)
    (setf (left cell) cell))
  (unless (slot-boundp cell 'right)
    (setf (right cell) cell)))

(defmethod print-object ((cell fibonacci-heap-cell) stream)
  (print-unreadable-object (cell stream :type T)
    (format NIL "~s" (value cell))))

(defmethod insert (value (heap fibonacci-heap))
  (let ((new-heap (make-instance 'fibonacci-heap-cell :value value :parent heap)))
    (setf (child heap)
          (if (child heap)
              (merge-heaps (child heap) new-heap)
              new-heap))))

(defmethod merge-heaps ((a fibonacci-heap-cell) (b fibonacci-heap-cell))
  (setf (left a) b
        (right b) a
        (right (left a)) (right b)
        (left (right b)) (left a))
  (if (< (value a) (value b))
      a
      b))

(defmethod minimum ((heap fibonacci-heap))
  (and (child heap)
       (value (child heap))))

(defun map-fibonacci-children (heap function)
  (let* ((end (left (child heap))))
    (loop for current = (child heap) then next
          for next = (right current)
          do (funcall function current)
          until (eql end current))
    heap))

(defmacro do-fibonacci-children ((current heap) &body body)
  `(block NIL (map-fibonacci-children ,heap (lambda (,current) ,@body))))

(defmethod find-cell (value (heap fibonacci-heap))
  (do-fibonacci-children (current heap)
    (when (= (value current) value)
      (return current))
    (when (child current)
      (let ((found (find-cell value current)))
        (when found (return found))))))

(defun remove-child-directly (child)
  (setf (right (left child)) (right child)
        (left (right child)) (left child))
  child)

(defun fuse-fibonacci-heap (heap)
  (let ((rank (make-array 5 :adjustable T :initial-element NIL))
        (minimum (child heap)))
    (do-fibonacci-children (current heap)
      (when (< (value current) (value minimum))
        (setf minimum current))
      ;; Make sure our rank array has space
      (when (< (length rank) (rank current))
        (adjust-array rank (rank current)))
      (let ((ranked (aref rank (rank current))))
        (cond ;; Nothing there yet, register.
          ((null ranked)
           (setf (aref rank (rank current)) current))
          ;; Determine smaller, turn larger into child, reconnect.
          (T
           (setf (aref rank (rank current)) NIL)
           (flet ((order (smaller larger)
                    ;; First, tie its neighbors
                    (remove-child-directly larger)
                    ;; Merge it into the child-array
                    (if (child smaller)
                        (setf (child smaller) (merge-heaps (child smaller) larger))
                        (setf (child smaller) larger))
                    (setf (parent larger) smaller)
                    ;; Update metadata
                    (setf (marker smaller) NIL)
                    (incf (rank smaller))))
             (if (< (value current) (value ranked))
                 (order ranked current)
                 (order current ranked)))))))
    (setf (child heap) minimum))
  heap)

(defun find-minimum-neighbor (heap)
  (let ((min heap))
    (loop for next = (right heap) then (right next)
          until (eql next heap)
          when (< (value next)
                  (value min))
          do (setf min next))
    min))

(defmethod delete-minimum ((heap fibonacci-heap))
  (let ((child (child heap)))
    (when child
      (cond ((eql child (right child))
             (setf (child heap) (child child)))
            ((not (child child))
             (remove-child-directly child)
             ;; ??? This seems like an oversight in the
             ;; algorithm. Inserting and removing directly
             ;; will result in O(n) for delete-minimum...
             (setf (child heap) (find-minimum-neighbor (left child))))
            (T
             (merge-heaps (right child) (child child))
             (setf (child heap) (right child))
             (fuse-fibonacci-heap heap)))
      (value child))))

(defmethod decrease-key ((cell fibonacci-heap-cell) amount)
  (setf (marker cell) T)
  (let ((to-insert ())
        (root cell))
    (loop until (not (typep root 'fibonacci-heap-cell))
          do (cond ;; Already marked! Unmark, remove, and note for insertion.
                   ((marker root)
                    (setf (marker root) NIL)
                    (remove-child-directly root)
                    (push root to-insert))
                   ;; Unmarked, we can stop now.
                   (T
                    (setf (marker root) T)
                    ;; Ok, we're done, ascend the rest
                    (loop until (not (typep root 'fibonacci-heap-cell))
                          do (setf root (parent root)))
                    (return)))
             (setf root (parent root)))
    ;; Merge in and relink minimum.
    (let ((minimum (child root)))
      (dolist (current to-insert)
        (when (< (value root) (value minimum))
          (setf minimum root))
        (merge-heaps current (child root))
        (setf (parent current) root))
      (setf (child root) minimum))
    (values cell root)))

(defmethod delete-cell ((cell fibonacci-heap-cell))
  (let ((value (value cell)))
    (decrease-key cell most-positive-fixnum)
    ;; We must have arrived at the root.
    (delete-minimum (parent cell))
    value))
