#|
 This file is a part of Complex
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:complex
  (:nicknames #:org.shirakumo.complex)
  (:use #:cl)
  ;; find.lisp
  (:export
   #:linear-find
   #:binary-find)
  ;; sequence.lisp
  (:export
   #:map-sequence
   #:do-sequence
   #:sorted-p
   #:make-like
   #:copy-into
   #:middle)
  ;; sort.lisp
  (:export
   #:bubble-sort
   #:insertion-sort
   #:selection-sort
   #:merge-sort
   #:heap-sort
   #:quick-sort
   #:bucket-sort
   #:radix-sort))

