#|
 This file is a part of Complex
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem complex
  :name "Complex"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A collection of datastructures and algorithms for illustrative purposes."
  :homepage "https://Shinmera.github.io/complex/"
  :bug-tracker "https://github.com/Shinmera/complex/issues"
  :source-control (:git "https://github.com/Shinmera/complex.git")
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
