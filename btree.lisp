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

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type T)
    (format stream "~a" (item node))))

(defclass child-node (node)
  ((parent :initarg :parent :accessor parent))
  (:default-initargs
   :parent NIL))

(defclass bnode (child-node)
  ((left :initarg :left :accessor left)
   (right :initarg :right :accessor right))
  (:default-initargs
   :left NIL :right NIL))

(defun bnode-left-p (node)
  (when (and (parent node)
             (not (bnode-root-p node)))
    (eql node (left (parent node)))))

(defun bnode-right-p (node)
  (when (and (parent node)
             (not (bnode-root-p node)))
    (eql node (right (parent node)))))

(defun bnode-root-p (node)
  (typep (parent node) 'tree))

(defun bnode-parent-p (node)
  (or (left node)
      (right node)))

(defun bnode-replace (node replacement)
  (setf (parent replacement)
        (parent node))
  (cond ((bnode-left-p node)
         (setf (left (parent node)) replacement))
        ((bnode-right-p node)
         (setf (right (parent node)) replacement))
        ((bnode-root-p node)
         (setf (root (parent node)) replacement))))

(defun bnode-rotate-left (node)
  (let ((replacement (right node)))
    (bnode-replace node replacement)
    (setf (right node) (left replacement))
    (when (right node)
      (setf (parent (right node)) node))
    (setf (left replacement) node)
    (setf (parent node) replacement))
  (parent node))

(defun bnode-rotate-right (node)
  (let ((replacement (left node)))
    (bnode-replace node replacement)
    (setf (left node) (right replacement))
    (when (left node)
      (setf (parent (left node)) node))
    (setf (right replacement) node)
    (setf (parent node) replacement))
  (parent node))

(defclass tree ()
  ((root :initarg :root :accessor root))
  (:default-initargs
   :root NIL))

(defmethod initialize-instance :after ((tree tree) &key)
  (unless (or (typep (root tree) '(or null node)))
    (setf (root tree) (make-instance 'node :item (root tree)))))

(defclass btree (tree)
  ((key :initarg :key :accessor key)
   (equality :initarg :equality :accessor equality)
   (comparison :initarg :comparison :accessor comparison))
  (:default-initargs
   :key #'identity :equality #'= :comparison #'<))

(defmethod initialize-instance :after ((tree btree) &key)
  (unless (typep (root tree) '(or null node))
    (setf (root tree) (make-instance 'bnode :item (root tree)))))

(defun btree= (btree a b)
  (funcall (equality btree)
           (funcall (key btree) a)
           (funcall (key btree) b)))

(defun btree< (btree a b)
  (funcall (comparison btree)
           (funcall (key btree) a)
           (funcall (key btree) b)))

(defun map-btree-preorder (node function)
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

(defun map-btree-inorder (node function)
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

(defun map-btree-postorder (node function)
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

(defun btree-find (item tree)
  (loop for node = (root tree)
        then (if (btree< tree item (item node))
                 (or (left node)
                     (return (values NIL node :left)))
                 (or (right node)
                     (return (values NIL node :right))))
        while node
        do (when (btree= tree item (item node))
             (return node))))

(defun btree-insert (item tree)
  (if (root tree)
      (multiple-value-bind (found node place) (btree-find item tree)
        (when found
          (error "~s already exists in tree!" item))
        (let ((child (make-instance 'bnode :item item :parent node)))
          (ecase place
            (:left (setf (left node) child))
            (:right (setf (right node) child)))
          child))
      (setf (root tree) (make-instance 'bnode :item item :parent tree))))

(defun make-btree (&rest items)
  (let ((tree (make-instance 'btree)))
    (dolist (item items)
      (btree-insert item tree))
    tree))

(defmethod label ((node node))
  (item node))

(defun render-btree (tree output)
  (let ((temp (merge-pathnames "btree-render.dot" (uiop:temporary-directory)))
        (nilcount 0))
    (with-open-file (stream temp :direction :output :if-exists :supersede)
      (format stream "digraph btree {~%")
      (labels ((render-node (node)
                 (let ((item (item node)))
                   (format stream "N_~a [label=\"~a\"]" item (label node))
                   (unless (bnode-root-p node)
                     (format stream "N_~a -> N_~a;" item (item (parent node))))
                   (cond ((left node)
                          (format stream "N_~a -> N_~a;" item (item (left node)))
                          (render-node (left node)))
                         (T
                          (format stream "NIL_~a [shape=point];~%N_~a -> NIL_~:*~:*~a;"
                                  (incf nilcount) item)))
                   (cond ((right node)
                          (format stream "N_~a -> N_~a;" item (item (right node)))
                          (render-node (right node)))
                         (T
                          (format stream "NIL_~a [shape=point];~%N_~a -> NIL_~:*~:*~a;"
                                  (incf nilcount) item))))))
        (render-node (root tree)))
      (format stream "~&}"))
    (uiop:run-program (format NIL "dot -Tsvg \"~a\" > \"~a\""
                              (uiop:native-namestring temp)
                              (uiop:native-namestring output)))
    output))
