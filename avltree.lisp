#|
 This file is a part of Complex
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.complex)

(defclass avlnode (bnode)
  ((balance :initarg :balance :accessor balance))
  (:default-initargs
   :balance 0))

(defmethod label ((node avlnode))
  (format NIL "~a | ~a" (item node) (balance node)))

(defclass avltree (btree)
  ())

(defun make-avltree (&rest items)
  (let ((tree (make-instance 'avltree)))
    (dolist (item items)
      (avltree-insert item tree))
    tree))

(defun avltree-find (item tree)
  (btree-find item tree))

(defun avl-balance-in-left (node)
  (let ((parent (parent node)))
    (case (balance parent)
      ( 1 (decf (balance parent)))
      ( 0 (decf (balance parent))
       (avl-balance-in parent))
      (-1
       (case (balance node)
         (-1 (bnode-rotate-right parent)
          (setf (balance node) 0
                (balance parent) 0))
         ( 1 (bnode-rotate-left node)
          (decf (balance node) 2)
          (bnode-rotate-right (parent (parent node)))
          (setf (balance (parent node)) 0
                (balance (right (parent node))) 0))))))
  node)

(defun avl-balance-in-right (node)
  (let ((parent (parent node)))
    (case (balance parent)
      (-1 (incf (balance parent)))
      ( 0 (incf (balance parent))
       (avl-balance-in parent))
      ( 1
       (case (balance node)
         ( 1 (bnode-rotate-left parent)
          (setf (balance node) 0
                (balance parent) 0))
         (-1 (bnode-rotate-right node)
          (incf (balance node) 2)
          (bnode-rotate-left (parent (parent node)))
          (setf (balance (parent node)) 0
                (balance (left (parent node))) 0))))))
  node)

(defun avl-balance-in (node)
  (cond ((bnode-left-p node)
         (avl-balance-in-left node))
        ((bnode-right-p node)
         (avl-balance-in-right node))))

(defun avltree-insert (item tree)
  (let ((node (btree-insert item tree)))
    (change-class node 'avlnode :balance 0)
    (unless (bnode-root-p node)
      (cond ((bnode-left-p node)
             (decf (balance (parent node))))
            ((bnode-right-p node)
             (incf (balance (parent node)))))
      (unless (= 0 (balance (parent node)))
        (avl-balance-in (parent node)))))
  tree)

(defun avl-balance-out-left (node)
  )

(defun avl-balance-out-right (node)
  )

(defun avl-balance-out (node)
  (cond ((bnode-left-p node)
         (avl-balance-out-left node))
        ((bnode-right-p node)
         (avl-balance-out-right node))))

(defun avltree-remove (item tree)
  (let ((node (typecase item
                (node item)
                (T (btree-find item tree)))))
    (cond ((and (left node)
                (right node))
           ;; Replace, and remove shifted.
           (setf (item node) (item (left node)))
           (avltree-remove (left node) tree))
          ((left node)
           (bnode-replace node (left node))
           (avl-balance-out (parent node)))
          ((right node)
           (bnode-replace node (right node))
           (avl-balance-out (parent node)))
          (T
           (if (bnode-root-p node)
               (setf (root (parent node)) NIL
                     (parent node) NIL)
               (let ((parent (parent node)))
                 (cond ((bnode-left-p node)
                        (setf (left parent) NIL)
                        (cond ((not (right parent))
                               (incf (balance parent))
                               (avl-balance-out (parent node)))
                              ((not (bnode-parent-p (right parent)))
                               (incf (balance parent)))
                              (T
                               ;; mumble mumble check balance
                               (bnode-rotate-right parent)
                               (incf (balance parent))
                               (avl-balance-out (parent node)))))
                       ((bnode-right-p node)
                        (setf (right parent) NIL)
                        (cond ((not (left parent))
                               (incf (balance parent))
                               (avl-balance-out (parent node)))
                              ((not (bnode-parent-p (left parent)))
                               (incf (balance parent)))
                              (T
                               (bnode-rotate-left parent)
                               (incf (balance parent))
                               (avl-balance-out (parent node)))))))))))
  tree)
