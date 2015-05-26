#|
 This file is a part of Complex
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem complex
  :name "Complex"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A collection of datastructures and algorithms for illustrative purposes."
  :homepage "https://github.com/Shinmera/complex"
  :version "1.0.0"
  :serial T
  :components ((:file "package")
               (:file "sequence")
               (:file "find")
               (:file "sort")
               (:file "median")
               (:file "hash")
               (:file "btree")
               (:file "avltree")
               (:file "kruskal")
               (:file "fibonacci-heap")
               (:file "dijkstra"))
  :depends-on ())
