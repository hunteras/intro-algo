;;;; intro-algo.asd

(asdf:defsystem #:intro-algo
  :serial t
  :description "Describe intro-algo here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:numbers :utilities)
  :components ((:file "package")
               (:file "intro-algo")
               (:file "bstree")
               (:file "benchmark")))

