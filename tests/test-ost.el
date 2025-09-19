;;; -*- lexical-binding: t; -*-
(require 'ost)

(defun ost--no-consecutive-red-p (node &optional parent-red)
  "Return t if subtree rooted at NODE has no consecutive red nodes."
  (if (null node)
      t
    (let ((red (not (ost-node-black node))))
      (if (and parent-red red)
          nil
        (and (ost--no-consecutive-red-p (ost-node-left node) red)
             (ost--no-consecutive-red-p (ost-node-right node) red))))))

(defun ost--black-height (node)
  "Return the black height of NODE if the subtree satisfies the black property,
or nil if the property is violated."
  (if (null node)
      1  ;; nil nodes count as black
    (let ((left-height  (ost--black-height (ost-node-left node)))
          (right-height (ost--black-height (ost-node-right node)))
          (is-black     (ost-node-black node)))
      (if (and left-height right-height
               (= left-height right-height))
          (+ left-height (if is-black 1 0))
        nil))))

(defun ost--black-property-p (tree)
  "Return t if TREE satisfies the black property."
  (when tree
    (not (null (ost--black-height (ost-tree-root tree))))))

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

 ;; Does not have two consecutive nodes that are red
 (it "red property holds"
     (let ((tree (ost--random-tree 1000)))
       (expect (ost--no-consecutive-red-p (ost-tree-root tree))
               :to-be t)))

 ;; Every path from node to descendant leave has same numberof black nodes
 (it "black property holds"
     (let ((tree (ost--random-tree 1000)))
       (expect (ost--black-property-p tree) :to-be t)))
 
 (it "finds correct successor"
     (let ((tree (make-ost-tree))
           (n 100)
           nodes)
       ;; No built-in way to shuffle a list in elisp!!!!!!!!!!!!!
       (dotimes (i n)
         (push (ost-insert tree i) nodes))
       (dotimes (i (1- n))
         (let ((j (- n i 1)))
           (expect (ost-node-key (ost--successor (nth j nodes)))
                   :to-equal (1+ i))))))

 (it "swaps values"
     (let ((n1 (make-ost-node :key 1 :data '(:a 1)))
           (n2 (make-ost-node :key 2 :data '(:a 2))))
       (ost--swap-values n1 n2)
       (expect (ost-node-key n1) :to-equal 2)
       (expect (ost-node-key n2) :to-equal 1)
       (expect (ost-node-data n1) :to-equal '(:a 2))
       (expect (ost-node-data n2) :to-equal '(:a 1))))

 (it "search works"
     (let* ((n 30)
            (tree (ost--sequence-tree n)))
       (dotimes (i n)
         (expect (ost-node-key (ost-search tree i)) :to-equal i))
       (expect (ost-search tree (1+ n)) :to-be nil)))

 ;; For deletion tests: https://en.wikipedia.org/wiki/File:BST_node_deletion.png
 
 (it "deletes correctly, two children, successor is right"
     ;; Note because we swap values when deleted node has two children, we
     ;; cannot rely on equality testing.
     (let* ((tree (make-ost-tree))
            (_ (ost-insert tree 1 "_"))
            (q (ost-insert tree 2 "q"))
            (l (ost-insert tree 3 "l"))
            (z (ost-insert tree 4 "z"))
            (y (ost-insert tree 5 "y"))
            (x (ost-insert tree 6 "x")))
       
       (ost-remove tree z)

       (let* ((node (ost-tree-root tree))
              (left (ost-node-left node))
              (right (ost-node-right node)))
         (expect (ost-node-data node) :to-equal "q")
         (expect (ost-node-data left) :to-equal "_")
         (expect (ost-node-data right) :to-equal "y")

         (expect (ost-node-data (ost-node-left right)) :to-equal "l")
         (expect (ost-node-data (ost-node-right right)) :to-equal "x")
         )))

 (it "deletes correctly (exhaustive)"
     (let* ((n 1000)
            (tree (ost--sequence-tree n))
            done key)
       (dotimes (i 30)
         (while (member (setq key (random n)) done))
         (let ((node (ost-search tree key)))
           (ost-remove tree node)
           (push key done)
           (expect (ost--no-consecutive-red-p (ost-tree-root tree))
                   :to-be t)
           (expect (ost--black-property-p tree) :to-be t)))))
 )


              
