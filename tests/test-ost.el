;;; -*- lexical-binding: t; -*-
(require 'ost)

(defun ost--random-tree (n)
  "Build a random red-black tree with N nodes."
  (let ((tree (make-ost-tree)))
    (dotimes (_ n)
      (ost-insert tree (random 1000)))
    tree))

(describe
 "ost"
 (it "rotates correctly"
     (let* ((root (make-ost-node))
            (tree (make-ost-tree :root root))
            (left (make-ost-node :parent root))
            (right (make-ost-node :parent left))
            (a (make-ost-node :parent left))
            (b (make-ost-node :parent right))
            (c (make-ost-node :parent right)))
       (setf (ost-node-left root) left
             (ost-node-left left) a
             (ost-node-right left) right
             (ost-node-left right) b
             (ost-node-right right) c)
       (ost--rotate tree left 'left)
       (expect (ost-tree-root tree) :to-equal root)
       (expect (ost-node-left root) :to-equal right)
       (expect (ost-node-left left) :to-equal a)
       (expect (ost-node-right left) :to-equal b)
       (expect (ost-node-left right) :to-equal left)
       (expect (ost-node-right right) :to-equal c)
       (expect (ost-node-parent a) :to-equal left)
       (expect (ost-node-parent b) :to-equal left)
       (expect (ost-node-parent c) :to-equal right)
       (expect (ost-node-parent left) :to-equal right)
       (expect (ost-node-parent right) :to-equal root)))

 (it "does not have two consecutive nodes that are red"
     (defun ost--no-consecutive-red-p (node &optional parent-red)
       "Return t if subtree rooted at NODE has no consecutive red nodes."
       (if (null node)
           t
         (let ((red (not (ost-node-black node))))
           (if (and parent-red red)
               nil
             (and (ost--no-consecutive-red-p (ost-node-left node) red)
                  (ost--no-consecutive-red-p (ost-node-right node) red))))))

     (let ((tree (ost--random-tree 1000)))
       (expect (ost--no-consecutive-red-p (ost-tree-root tree))
               :to-be t))))
              
