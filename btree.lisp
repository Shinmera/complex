#|
 This file is a part of Complex
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.complex)

(defclass node ()
  ((item :initarg :item :accessor item))
  (:default-initargs
   :item (error "ITEM required.")))

(defclass child-node (node)
  ((parent :initarg :parent :accessor parent))
  (:default-initargs
   :parent NIL))

(defclass binary-node (child-node)
  ((left :initarg :left :accessor left)
   (right :initarg :right :accessor right))
  (:default-initargs
   :left NIL :right NIL))

(defclass tree ()
  ((root :initarg :root :accessor root))
  (:default-initargs
   :root (error "ROOT required.")))

(defun map-tree-preorder (node function)
  (labels ((mapt (node)
             (funcall function node)
             (when (left node)
               (mapt (left node)))
             (when (right node)
               (mapt (right node)))))
    (mapt (if (typep node 'tree)
              (root node)
              node)))
  node)

(defun map-tree-inorder (node function)
  (labels ((mapt (node)
             (when (left node)
               (mapt (left node)))
             (funcall function node)
             (when (right node)
               (mapt (right node)))))
    (mapt (if (typep node 'tree)
              (root node)
              node)))
  node)

(defun map-tree-postorder (node function)
  (labels ((mapt (node)
             (when (left node)
               (mapt (left node)))
             (when (right node)
               (mapt (right node)))
             (funcall function node)))
    (mapt (if (typep node 'tree)
              (root node)
              node)))
  node)

(defclass btree (tree)
  ((key :initarg :key :accessor key)
   (equality :initarg :equality :accessor equality)
   (comparison :initarg :comparsion :accessor comparison))
  (:default-initargs
   :key #'identity :equality #'= :comparison #'<))

(defun btree= (btree a b)
  (funcall (equality btree)
           (funcall (key btree) a)
           (funcall (key btree) b)))

(defun btree< (btree a b)
  (funcall (comparison btree)
           (funcall (key btree) a)
           (funcall (key btree) b)))

(defun btree-find (item tree)
  (loop for node = (root tree)
        then (if (btree< tree item (item node))
                 (or (left tree)
                     (return (values NIL node :left)))
                 (or (right tree)
                     (return (values NIL node :right))))
        while node
        do (when (btree= tree item (item node))
             (return node))))

(defun btree-insert (item tree)
  (multiple-value-bind (item node place) (btree-find item tree)
    (when item
      (error "~s already exists in tree!" item))
    (let ((child (make-instance 'binary-node :item item :parent node)))
      (ecase place
        (:left (setf (left node) child))
        (:right (setf (right node) child)))
      (values child node place))))

(defun bnode-replace (node replacement)
  (setf (parent replacement)
        (parent node))
  (when (eq node (left (parent node)))
    (setf (left (parent node)) replacement))
  (when (eq node (right (parent node)))
    (setf (right (parent node)) replacement)))
