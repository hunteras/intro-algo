;;;; intro-algo.lisp

(in-package #:intro-algo)

;;; "intro-algo" goes here. Hacks and glory await!

(eval-when (:compile-toplevel :execute :load-toplevel)

  (defun as-keyword (sym)
    "translate a symbol to the corresponding keyword symbol."
    (intern (string sym) :keyword))

  (defun slot->defclass-slot (slot)
    "take a slot and returns a DEFCLASS slot specifier."
    `(,slot :initarg ,(as-keyword slot) :accessor ,slot))
  )

(defmacro define-type (name slots)
  `(defclass ,name ()
     ,(mapcar #'slot->defclass-slot slots)))

(define-type node (key left right parent))

(defmethod print-object ((obj node) out)
  (labels ((key-str (o)
             (format nil "~A" (if (null o) nil (key o)))))
    (print-unreadable-object (obj out :type t)
      (format out "<key:~A> <left:~A> <right:~A> <parent:~A>"
              (key obj) (key-str (left obj)) (key-str (right obj)) (key-str (parent obj))))))

(defun make-node (k &optional (l nil) (r nil) (p nil))
  "make a new node."
  (make-instance 'node :key k :left l :right r :parent p))

;; comes binary search tree

(defun tree-insert (tree node)
  "insert a node to tree. return the node inserted or equal to."
  (cond ((= (key node) (key tree)) tree)
        ((< (key node) (key tree)) (if (null (left tree))
                                       (setf (left tree) node
                                             (parent node) tree)
                                       (tree-insert (left tree) node)))
        (t (if (null (right tree))
               (setf (right tree) node
                     (parent node) tree)
               (tree-insert (right tree) node)))))
  

(defun inorder-tree-walk (tree fn)
  "walk the root of a tree between walking its left subtree and its right subtree."
  (unless (null tree)
    (inorder-tree-walk (left tree) fn)
    (funcall fn tree)
    (inorder-tree-walk (right tree) fn)))
  
(defun preorder-tree-walk (tree fn)
  "walk the root of a tree before walking its left subtree and its right subtree."
  (unless (null tree)
    (funcall fn tree)
    (preorder-tree-walk (left tree) fn)
    (preorder-tree-walk (right tree) fn)))

(defun postorder-tree-walk (tree fn)
  "walk the root of a tree after walking its left subtree and its right subtree."
  (unless (null tree)
    (postorder-tree-walk (left tree) fn)
    (postorder-tree-walk (right tree) fn)
    (funcall fn tree)))

(defun tree-print (tree walk)
  "print tree by walking method."
  (funcall walk tree #'(lambda (node) (format *standard-output* "~A~%" node))))

(defun list->tree (lst)
  "build a tree from a list."
  (labels ((iter (l root)
             (if (null l)
                 root
                 (progn (tree-insert root (make-node (car l)))
                        (iter (cdr l) root)))))
    (let ((tree (make-node (car lst))))
      (iter (cdr lst) tree)
      tree)))

(defun tree-search (tree k)
  "search node with key k in the tree."
  (cond ((or (null tree) (= k (key tree))) tree)
        ((< k (key tree)) (tree-search (left tree) k))
        (t (tree-search (right tree) k))))

(defun tree-root (tree)
  "back to the root of a tree."
  (cond ((null tree) nil)
        ((null (parent tree)) tree)
        (t (tree-root (parent tree)))))

(defun tree-minimum (tree)
  "return minimum items in the tree."
  (if (null (left tree))
      tree
      (tree-minimum (left tree))))

(defun tree-maximum (tree)
  "return maximum items in the tree."
  (if (null (right tree))
      tree
      (tree-maximum (right tree))))

(defun tree-successor (tree)
  "If the right subtree of node is nonempty, it's successor is just the leftmost node of it's right subtree.
   If the right subtree of node x is empty and it has a successor y, then y is the lowest ancestor of x whose
   left child is also an ancestor of x."
  (labels ((iter (x y)
             (cond ((null y) y)
                   ((and (not (null y))
                         (right-p x y))
                    (iter y (parent y)))
                   (t y))))
    (if (not (null (right tree)))
        (tree-minimum (right tree))
        (iter tree (parent tree)))))
      
(defun tree-predecessor (tree)
  "If the left subtree of node is nonempty, it's predecessor is just the rightmost node of it's left subtree.
   If the left subtree of node x is empty and it has a predecessor y, then y is the lowest ancestor of x whose
   right child is also an ancestor of x."
  (labels ((iter (x y)
             (cond ((null y) y)
                   ((and (not (null y))
                         (left-p x y))
                    (iter y (parent y)))
                   (t y))))
    (if (not (null (left tree)))
        (tree-maximum (left tree))
        (iter tree (parent tree)))))

(defun left-p (u v)
  "check whether u is left of v."
  (eq u (left v)))

(defun right-p (u v)
  "check whether u is right of v."
  (eq u (right v)))

(defun parent-p (u v)
  "check whether u is parent of v."
  (eq u (parent v)))

(defun tree-delete (node)
  "delete node from tree. return the tree's root after node deleted."
  (labels ((transplant (u v)
             "replaces the subtree rooted at node u with the subtree rooted at node v. 
              if u is the root of the tree, v could be it's direct left subtree, 
              direct right subtree or deepper node."
             (let ((res (cond ((null (parent u))
                               (if (or (left-p v u)
                                       (right-p v u))
                                   v
                                   (progn
                                     (unless (null (left u)) (setf (parent (left u)) v))
                                     (unless (null (right u)) (setf (parent (right u)) v))
                                     (setf (left v) (left u)
                                           (right v) (right u))
                                     v)))
                              ((left-p u (parent u)) (progn (setf (left (parent u)) v) (parent u)))
                              (t (progn (setf (right (parent u)) v) (parent u))))))
               (unless (null v)
                 (setf (parent v) (parent u)))
               res)))
    (tree-root 
     (cond ((null (left node)) (transplant node (right node)))
           ((null (right node)) (transplant node (left node)))
           (t (let ((y (tree-minimum (right node))))
                (unless (parent-p node y)
                  (transplant y (right y))
                  (setf (right y) (right node)
                        (parent (right y)) y))
                (transplant node y)
                (setf (left y) (left node)
                      (parent (left y)) y)
                y))))))

