;;;; intro-algo.lisp

(in-package #:intro-algo)

;;; "intro-algo" goes here. Hacks and glory await!

(defmacro time-sort-fn (fn lst)
  "time sort fn with lst."
  `(progn (time (,fn ,lst))
          '(,fn done)))
 
(defun time-sort (fn lst)
  "time sort method fn with n elements."
  (time (funcall fn lst))
  'done)

(defun sorted-p (lst)
  "check lst is whether sorted."
  (labels ((iter (l n)
             (cond ((null l) t)
                   ((<= n (car l)) (iter (cdr l) (car l)))
                   (t nil))))
    (iter (cdr lst) (car lst))))



(defun t-tree-delete (tree k)
  "test tree delete, delete node with k, then print the tree with the node deleted."
  (let ((d-tree (tree-delete (tree-search tree k))))
    (format *standard-output* "DEL: ~A ~%" k)
    (tree-print d-tree #'preorder-tree-walk)
    (format *standard-output* "~%")
    d-tree))

(defun t-list-tree-delete ()
  "test tree delete with a list."
  (let* ((lst (random-distribute-list 10))
         (tree (list->tree lst)))
    (format *standard-output* "list: ~{~A ~}~%" lst)
    (labels ((iter (l tree k)
               (if (null l)
                   'done
                   (iter (cdr l) (t-tree-delete tree k) (car l)))))
      (iter (cdr lst) tree (car lst)))))
