;;; -*- lexical-binding: t; -*-
(require 'ost)

(describe
 "ost"
 (it "rotates correctly"
     (let* ((root (make-ost-node))
            (tree (make-ost-tree :root root))
            (left (make-ost-node))
            (right (make-ost-node))
            (a (make-ost-node))
            (b (make-ost-node))
            (c (make-ost-node)))
       (ost--set-parent-child :child left :parent root :direction 'left)
       (ost--set-parent-child :child a :parent left :direction 'left)
       (ost--set-parent-child :child right :parent left :direction 'right)
       (ost--set-parent-child :child b :parent right :direction 'left)
       (ost--set-parent-child :child c :parent right :direction 'right)
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

 (it "insert rebalances correctly"
     (let* ((root (make-ost-node :black t :data "P"))
            (tree (make-ost-tree :root root))
            (L (make-ost-node :data "L"))
            (a (make-ost-node :data "a")))
       (ost--insert tree L root 'left)
       (ost--insert tree a L 'left)
       )
     )

 (it "inserted keys ordered correctly"
     (let ((tree (make-ost-tree))
           (n 10)
           nodes)
       (dotimes (i n)
         (push (ost-insert tree i) nodes))
       (dotimes (i (1- n))
         (let* ((j (- n i 1))
                (n (nth j nodes))
                (k (ost-node-key n)))
           (when-let ((left (ost-node-left n)))
             (expect (ost-node-key left) :to-be-weakly-less-than k))
           (when-let ((right (ost-node-right n)))
             (expect (ost-node-key right) :to-be-weakly-greater-than k))
           ))))

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
               :to-be t)))

              
