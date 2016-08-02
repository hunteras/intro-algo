;;;; intro-algo.lisp

(in-package #:intro-algo)

;;; "intro-algo" goes here. Hacks and glory await!

(defun insert (lst n)
  "lst is sorted, insert n at proper position where it is larger than former and less than or equal to latter."
  (labels ((iter (l acc)
             (cond ((null l) (reverse (cons n acc)))
                   ((< n (car l)) (append (reverse (cons n acc)) l))
                   (t (iter (cdr l) (cons (car l) acc))))))
    (iter lst nil)))

(defun insertion-sort (lst &optional (insert-fn #'insert))
  "sort lst with insertion sort method."
  (labels ((iter (sorted left)
             (cond ((null left) sorted)
                   ((null sorted) (iter (cons (car left) sorted) (cdr left)))
                   (t (iter (funcall insert-fn sorted (car left)) (cdr left))))))
    (iter nil lst)))


(defun divide (lst)
  "Divide the n elements sequence into two subsequece of n/2 elements each."
  (labels ((iter (n l acc)
             (if (= n 0)
                 (values (reverse acc) l)
                 (iter (- n 1) (cdr l) (cons (car l) acc)))))
    (iter (floor (/ (length lst) 2)) lst nil)))

(defun merge-list (left right)
  "Merge the two sorted subsequences to produce the sorted answer."
  (labels ((iter (l r acc)
             (cond ((null l) (append (reverse acc) r))
                   ((null r) (append (reverse acc) l))
                   ((< (car l) (car r)) (iter (cdr l) r (cons (car l) acc)))
                   (t (iter l (cdr r) (cons (car r) acc))))))
    (iter left right nil)))

(defun merge-sort (lst)
  "sort lst with merge sort method."
  (if (not (single lst))
      (multiple-value-bind (left right) (divide lst)
        (merge-list (merge-sort left) (merge-sort right)))
      lst))
    

(defun bubble (lst)
  "bubble the largest item of the lst."
  (labels ((iter (l n acc)
             (cond ((null l) (values (reverse acc) n))
                   ((<= n (car l)) (iter (cdr l) (car l) (cons n acc)))
                   (t (iter (cdr l) n (cons (car l) acc))))))
    (iter (cdr lst) (car lst) nil)))

(defun bubble-sort (lst)
  "bubble until lst is empty. Accumulate the sorted items."
  (labels ((iter (l sorted)
             (if (null l)
                 sorted
                 (multiple-value-bind (left m) (bubble l)
                   (iter left (cons m sorted))))))
    (iter lst nil)))


(defun partition (lst)
  "divide lst to three sub list, less than, equal to and greater than. "
  (labels ((iter (l n left middle right)
             (cond ((null l) (values left middle right))
                   ((< (car l) n) (iter (cdr l) n (cons (car l) left) middle right))
                   ((= (car l) n) (iter (cdr l) n left (cons (car l) middle) right))
                   (t (iter (cdr l) n left middle (cons (car l) right))))))
    (iter lst (car lst) nil nil nil)))

(defun quick-sort (lst)
  "quick sort lst."
  (unless (null lst)
    (multiple-value-bind (left middle right) (partition lst)
      (append (quick-sort left) middle (quick-sort right)))))
