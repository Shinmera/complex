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
    (format stream "~s" (value cell))))

(defmethod insert ((new-heap fibonacci-heap-cell) (heap fibonacci-heap))
  (setf (parent new-heap) heap)
  (setf (child heap)
        (if (child heap)
            (merge-heaps (child heap) new-heap)
            new-heap)))

(defmethod insert (value (heap fibonacci-heap))
  (insert (make-instance 'fibonacci-heap-cell :value value) heap))

(defmethod merge-heaps ((a fibonacci-heap-cell) (b fibonacci-heap-cell))
  (let ((ra (right a))
        (lb (left b)))
    (setf (right a) b
          (left b) a
          (left ra) lb
          (right lb) ra))
  (if (< (value a) (value b))
      a
      b))

(defmethod minimum ((heap fibonacci-heap))
  (and (child heap)
       (value (child heap))))

(defun map-fibonacci-children (heap function)
  (when (child heap)
    (let* ((end (left (child heap))))
      (mapcar function
              ;; Save it, for it might be modified.
              (loop for current = (child heap) then (right current)
                    collect current
                    until (eql end current)))
      heap)))

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
  (let ((left (left child))
        (right (right child)))
    (when (eql (child (parent child)) child)
      (setf (child (parent child))
            (unless (eql left child) left)))
    (setf (left right) left
          (right left) right)
    (setf (left child) child
          (right child) child))
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
                    (incf (rank smaller))
                    (setf (aref rank (rank smaller)) smaller)))
             (if (< (value current) (value ranked))
                 (order current ranked)
                 (order ranked current)))))))
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
             (when (child child)
               (setf (parent (child child)) heap))
             (setf (child heap) (child child)))
            ((not (child child))
             (let ((neighbor (left child)))
               (remove-child-directly child)
               (setf (child heap) neighbor)))
            (T
             (let ((right (right child)))
               (remove-child-directly child)
               (merge-heaps right (child child))
               (setf (child heap) right))))
      (fuse-fibonacci-heap heap)
      (values (value child)
              child))))

(defmethod decrease-key ((cell fibonacci-heap-cell) amount)
  (setf (marker cell) T
        (value cell) amount)
  (let ((to-insert ())
        (root cell))
    (loop while (typep root 'fibonacci-heap-cell)
          do (cond ;; Already marked! Unmark, remove, and note for insertion.
               ((marker root)
                (setf (marker root) NIL)
                (remove-child-directly root)
                (push root to-insert))
               ;; Unmarked, we can stop now.
               (T
                (setf (marker root) T)
                ;; Ok, we're done, ascend the rest
                (loop while (typep root 'fibonacci-heap-cell)
                      do (setf root (parent root)))
                (return)))
             (setf root (parent root)))
    ;; Relink back top. This will automatically ensure proper parents and minimum.
    (dolist (current to-insert)
      (insert current root))
    (values cell root)))

(defmethod delete-cell ((cell fibonacci-heap-cell))
  (let ((value (value cell)))
    (decrease-key cell most-negative-fixnum)
    ;; We must have arrived at the root.
    (delete-minimum (parent cell))
    value))

(defmethod label ((cell fibonacci-heap-cell))
  (value cell))

(defun render-fibonacci-heap (heap output)
  (let ((temp (merge-pathnames "fibonacci-tree-render.dot" (uiop:temporary-directory))))
    (with-open-file (stream temp :direction :output :if-exists :supersede)
      (format stream "digraph fib {~%")
      (labels ((render-cell (parent)
                 (format stream "N_~a -> N_~a [style=\"bold\"];"
                         (if (typep parent 'fibonacci-heap-cell) (value parent) "ROOT")
                         (value (child parent)))
                 (do-fibonacci-children (node parent)
                   (let ((id (value node)))
                     (format stream "N_~a [label=\"~a\"]" id (label node))
                     (format stream "N_~a -> N_~a;" id (value (left node)))
                     (format stream "N_~a -> N_~a;" id (value (right node)))
                     (when (child node)
                       (render-cell node))))))
        (render-cell heap))
      (format stream "~&}"))
    (uiop:run-program (format NIL "dot -Tsvg \"~a\" > \"~a\""
                              (uiop:native-namestring temp)
                              (uiop:native-namestring output)))
    output))
