(in-package #:org.shirakumo.complex)

(defvar +empty+ NIL)
(defvar +deleted+ (make-symbol "DELETED"))

(defclass hash-set ()
  ((items :initarg :items :initform (error "ITEMS required.") :reader items)
   (hashing-function :initarg :hashing-function :initform (error "HASHING-FUNCTION required.") :reader hashing-function)
   (probing-function :initarg :probing-function :initform (error "PROBING-FUNCTION required.") :reader probing-function)))

(defmethod print-object ((set hash-set) stream)
  (print-unreadable-object (set stream :type T)
    (format stream ":ITEMS ~s" (items set))))

(defun make-hash-set (size &key (hashing-function #'modulo-hash) (probing-function #'linear-probing))
  (make-instance 'hash-set
                 :items (make-array size :initial-element +empty+)
                 :hashing-function hashing-function
                 :probing-function probing-function))

(defun hash-item (item hash-set)
  (funcall (hashing-function hash-set)
           item (set-size hash-set)))

(defun probe-item (item hash-set attempts)
  (funcall (probing-function hash-set)
           item hash-set attempts))

(defun set-size (hash-set)
  (length (items hash-set)))

(defun insert-hash (item hash-set)
  (loop for attempts from 0
        for pos = (hash-item item hash-set)
        then (probe-item item hash-set attempts)
        for found = (aref (items hash-set) pos)
        do (when (or (eql found +empty+)
                     (eql found +deleted+))
             (return
               (values
                (setf (aref (items hash-set) pos) item)
                pos)))))

(defun find-hash (item hash-set)
  (loop for attempts from 0
        for pos = (hash-item item hash-set)
        then (probe-item item hash-set attempts)
        for found = (aref (items hash-set) pos)
        do (when (eql found +empty+)
             (return NIL))
           (when (eql found item)
             (return (values item pos)))))

(defun remove-hash (item hash-set)
  (loop for attempts from 0
        for pos = (hash-item item hash-set)
        then (probe-item item hash-set attempts)
        for found = (aref (items hash-set) pos)
        do (when (eql found +empty+)
             (return NIL))
           (when (eql found item)
             (setf (aref (items hash-set) pos)
                   +deleted+)
             (return (values item pos)))))


(defun modulo-hash (item size)
  (mod item size))

(defun linear-probing (item hash-set attempts)
  (mod (+ (hash-item item hash-set) attempts)
       (set-size hash-set)))

(defun quadratic-probing (item hash-set attempts)
  (mod (+ (hash-item item hash-set) (expt attempts 2))
       (set-size hash-set)))

(defun double-hash-probing (item hash-set attempts)
  (mod (+ (mod item (- (set-size hash-set) 2)) attempts)
       (set-size hash-set)))
