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

(defun ost--verify-sizes-recursive (node)
  "Recursively verify that the size of NODE and all its descendants is correct.
For any given node, its size must be 1 + the size of its left
child + the size of its right child. Returns t if all sizes are
correct, nil otherwise."
  (if (not node)
      t
    (let ((correct-size (+ 1
                           (ost--node-size (ost-node-left node))
                           (ost--node-size (ost-node-right node))))
          (stored-size (ost-node-size node)))
      (and (= correct-size stored-size)
           (ost--verify-sizes-recursive (ost-node-left node))
           (ost--verify-sizes-recursive (ost-node-right node))))))

(defun ost-verify-sizes (tree)
  "Verify that all node sizes in TREE are correct.
Returns t if the tree is valid, nil otherwise."
  (ost--verify-sizes-recursive (ost-tree-root tree)))

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
     (let* ((root (make-ost-node :black t :id "P"))
            (tree (make-ost-tree :root root))
            (L (make-ost-node :id "L"))
            (a (make-ost-node :id "a")))
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
     (let ((n1 (make-ost-node :key 1 :id '(:a 1)))
           (n2 (make-ost-node :key 2 :id '(:a 2))))
       (ost--swap-values n1 n2)
       (expect (ost-node-key n1) :to-equal 2)
       (expect (ost-node-key n2) :to-equal 1)
       (expect (ost-node-id n1) :to-equal '(:a 2))
       (expect (ost-node-id n2) :to-equal '(:a 1))))

 (it "search works"
     (let* ((n 30)
            (tree (ost--sequence-tree n)))
       (dotimes (i n)
         (expect (ost-node-key (ost-search tree i)) :to-equal i))
       (expect (ost-search tree (1+ n)) :to-be nil)))

 (it "search works with id tiebreaker"
     (let* ((tree (make-ost-tree))
            (n1 (ost-insert tree 1 "a"))
            (n2 (ost-insert tree 1 "b")))
       (expect (ost-search tree 1 "a") :to-be n1)
       (expect (ost-search tree 1 "b") :to-be n2)))

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
         (expect (ost-node-id node) :to-equal "q")
         (expect (ost-node-id left) :to-equal "_")
         (expect (ost-node-id right) :to-equal "y")

         (expect (ost-node-id (ost-node-left right)) :to-equal "l")
         (expect (ost-node-id (ost-node-right right)) :to-equal "x")
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

 (it "sizes correct after basic insertion"
     (let ((tree (make-ost-tree)))
       (expect (zerop (ost-tree-size tree)))
       
       ;; Insert first node
       (ost-insert tree 10 10)
       (expect (ost-tree-size tree) :to-equal 1)
       (expect (ost-verify-sizes tree) :to-be t)
       
       ;; Insert second node (left child)
       (ost-insert tree 5 5)
       (expect (ost-tree-size tree) :to-equal 2)
       (expect (ost-verify-sizes tree) :to-be t)

       ;; Insert third node (right child)
       (ost-insert tree 15 15)
       (expect (ost-tree-size tree) :to-equal 3)
       (expect (ost-verify-sizes tree) :to-be t)
       (let ((root (ost-tree-root tree)))
         (expect (ost-node-size root) :to-equal 3)
         (expect (ost-node-size (ost-node-left root)) :to-equal 1)
         (expect (ost-node-size (ost-node-right root)) :to-equal 1))))

 (it "size correct after various deletion scenarios"
  (let ((tree (ost--from-keys '(10 5 15 3 7 12 18))))
    (expect (ost-tree-size tree) :to-equal 7)
    (expect (ost-verify-sizes tree) :to-be t)

    ;; Case 1: Delete a leaf node (key 3)
    (ost-remove tree (ost-search tree 3))
    (expect (ost-tree-size tree) :to-equal 6)
    (expect (ost-verify-sizes tree) :to-be t))

    ;; Case 2: Delete a node with one child (key 18). First, let's restore the
    ;; tree and delete 15 instead, which has one child (12)
    (setq tree (ost--from-keys '(10 5 15 3 7 12 18)))
    (ost-remove tree (ost-search tree 15))
    (expect (ost-tree-size tree) :to-equal 6)
    (expect (ost-verify-sizes tree) :to-be t)

    ;; Case 3: Delete a node with two children (key 5). This involves swapping
    ;; with the successor (7)
    (setq tree (ost--from-keys '(10 5 15 3 7 12 18)))
    (ost-remove tree (ost-search tree 5))
    (expect (ost-tree-size tree) :to-equal 6)
    (expect (ost-verify-sizes tree) :to-be t)
    
    ;; Case 4: Delete the root node (key 10)
    ;; This involves swapping with successor (12)
    (setq tree (ost--from-keys '(10 5 15 3 7 12 18)))
    (ost-remove tree (ost-search tree 10))
    (expect (ost-tree-size tree) :to-equal 6)
    (expect (ost-verify-sizes tree) :to-be t)

    ;; Delete remaining nodes
    (dotimes (i (ost-tree-size tree))
      (ost-remove tree (ost-select tree 0)))

    )

  (it "selects and ranks correctly (0-based)"
     (let* ((n 100)
            ;; Create a tree with keys 0, 1, 2, ..., 99
            (tree (ost--from-keys (number-sequence 0 (1- n)))))
       (dotimes (i n)
         ;; Select the node that *should* be at index i
         (let ((node-at-i (ost-select tree i)))
           ;; Verify its key is i
           (expect (ost-node-key node-at-i) :to-equal i)
           ;; Verify that the rank of that node is indeed i
           (expect (ost-rank tree node-at-i) :to-equal i)))))

 (it "selects and ranks on a more complex tree (0-based)"
     (let* (;; Insert keys in a shuffled order to create a balanced tree
            (keys '(10 5 15 3 7 12 18 0 4 6 9 11 14 17 20))
            (sorted-keys (sort keys #'<))
            (tree (ost--from-keys keys)))
       (dotimes (i (length sorted-keys))
         (let* ((expected-key (nth i sorted-keys))
                (selected-node (ost-select tree i))
                (node-to-rank (ost-search tree expected-key)))
           (expect (ost-node-key selected-node) :to-equal expected-key)
           (expect (ost-rank tree node-to-rank) :to-equal i)))))

 (it "select throws error for out-of-bounds index"
     (let ((tree (ost--from-keys '(10 20 30))))
       (expect (ost-node-key (ost-select tree 0)) :to-equal 10)
       (expect (ost-node-key (ost-select tree 1)) :to-equal 20)
       (expect (ost-node-key (ost-select tree 2)) :to-equal 30)
       
       ;; And confirm error with `condition-case' for a generic test
       (expect (ost-select tree 3) :to-throw 'error)
       (expect (ost-select tree -1) :to-throw 'error)))

 (it "keeps tree nodes hashmap in sync"
     (let* ((tree (make-ost-tree))
            (n1 (ost-tree-insert tree 1 "a"))
            (n2 (ost-tree-insert tree 2 "b"))
            (n3 (ost-tree-insert tree 3 "c")))
       (expect (hash-table-count (ost-tree-nodes tree)) :to-equal 3)
       (expect (ost-tree-node-by-id tree "a") :to-be n1)
       (expect (ost-tree-node-by-id tree "b") :to-be n2)
       (expect (ost-tree-node-by-id tree "c") :to-be n3)

       ;; Remove lowest (index 0)
       (ost-tree-remove tree (ost-select tree 0))
       (expect (ost-select tree 0) :to-be (ost-tree-node-by-id tree "b"))
       (ost-tree-remove tree (ost-select tree 0))
       (expect (ost-select tree 0) :to-be (ost-tree-node-by-id tree "c"))
       
       ;; Swap updates nodes hashmap correctly
       (setq tree (ost--sequence-tree 5 'with-map))
       ;; node with id/key 3 has 2 children, so will be swapped with right
       ;; child, which is node 4.
       (setq n1 (ost-search tree 3))
       (expect (ost-node-id n1) :to-be 3)
       (ost-tree-remove tree n1)
       (expect (ost-node-id n1) :to-be 4)
       (expect (gethash 3 (ost-tree-nodes tree)) :to-be nil)
       (expect (ost-node-key (gethash 4 (ost-tree-nodes tree))) :to-equal 4)))
 )
              
