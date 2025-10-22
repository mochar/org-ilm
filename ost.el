;;; ost.el --- Order statistic tree with red-black B-tree base -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: M Charrout
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: 

;;; Commentary:

;; This package implements order statistic trees with a red-black binary search
;; tree as a self-balancing base. An order statistic tree holds an additional
;; value in each node: the size of the subtree rooted at that node.

;; Red-Black Tree properties:
;; - Node Color: Each node is either red or black.
;; - Root Property: The root of the tree is always black. (contested?)
;; - Red Property: Red nodes cannot have red children (no two consecutive
;;   red nodes on any path).
;; - Black Property: Every path from a node to its descendant null nodes
;;   (leaves) has the same number of black nodes.
;; - Leaf/nil Property: All leaves (null nodes) are black.

;; Insert and delete operations need corrections as violations can occur after
;; these operations.

;; References:
;; - https://en.wikipedia.org/wiki/Red-black_tree
;; - https://en.wikipedia.org/wiki/Order_statistic_tree
;; - https://en.wikipedia.org/wiki/Binary_search_tree

;;; Code:

(require 'cl-lib)

;;;; Node

(cl-defstruct ost-node
  "A node of a ordered statistic, red-black, binary search tree."
  (black
   nil
   :documentation "The color of the node, nil is red, non-nil is black."
   :type boolean)
  (key
   nil
   :documentation "The value on which to sort. Does not need to be unique."
   :type number)
  (id
   nil
   :documentation "Optional identifier of the node.")
  (left
   nil
   :documentation "The left child `ost-node'."
   :type ost-node)
  (right
   nil
   :documentation "The right child `ost-node'."
   :type ost-node)
  (parent
   nil
   :documentation "The parent `ost-node'. Only nil if root node."
   :type ost-node)
  (size
   1
   :documentation "Number of nodes in the subtree."
   :type number)
  )

;; TODO DOESNT WORK
(cl-defmethod cl-print-object ((s ost-node) stream)
  (with-slots (black key id left right parent size) s
    (princ
     (format "#(ost-node %s %s %s %s %s %s %s)"
                   black key id size
                   (when left (ost-node-id left))
                   (when right (ost-node-id right))
                   (when parent (ost-node-id parent)))
           stream)))

(defun ost-node-child (node direction)
  "Get the child of NODE in DIRECTION ('left or 'right)."
  (if (eq direction 'left)
      (ost-node-left node)
    (ost-node-right node)))

(defun ost-node-red (node)
  (not (ost-node-black node)))

(defun ost--node-size (node)
  "Return the size of NODE's subtree, or 0 if NODE is nil."
  (if node
      (ost-node-size node)
    0))

(defun ost-node-direction (node)
  "Return the direction ('left or 'right) of NODE relative to its parent."
  (let ((parent (ost-node-parent node)))
    (when parent
      (if (eq node (ost-node-right parent))
          'right
        'left))))

(defun ost--< (key1 key2 &optional id1 id2)
  "Returns t if KEY1 < KEY2.
If given, equality is handled by further comparing ID1 < ID2."
  (cond
   ((< key1 key2) t)
   ((> key1 key2) nil)
   (t ; Equal, compare by ID
    (when (and id1 id2)
      (if (stringp id1)
          (string< id1 id2)
        (< id1 id2))))))

(defun ost--node-< (n1 n2)
  "Returns t if N1 < N2.
First orders by key, then by id."
  (ost--< (ost-node-key n1) (ost-node-key n2)
          (ost-node-id n1) (ost-node-id n2)))

;;;; Tree

(cl-defstruct ost-tree
  "An ordered statistic, red-black, binary search tree."
  (root
   nil
   :documentation "The root node of the tree."
   :type ost-node)
  (dynamic
   nil
   :documentation "Dynamic tree nodes do not hold keys - the rank act as the key."
   :type boolean)
  (nodes
   (make-hash-table :test #'equal)
   :documentation "Hashmap id -> node."))

(cl-defmethod cl-print-object ((s ost-tree) stream)
  (princ (format "#s(ost-tree %s %s #%s)"
                 (ost-tree-root s)
                 (ost-tree-dynamic s)
                 (hash-table-count (ost-tree-nodes s)))
         stream))

(defun ost-tree-size (tree)
  "Return the total number of nodes in TREE."
  (ost--node-size (ost-tree-root tree)))

(defun ost-tree-node-by-id (tree id)
  "Return the node with id ID."
  (gethash id (ost-tree-nodes tree)))

(defun ost-tree-insert (tree key-or-rank id)
  "Insert a new node into the TREE.
The node will be stored in the nodes slot of TREE.
Returns the node."
  (let ((node (ost-insert tree key-or-rank id)))
    (puthash id node (ost-tree-nodes tree))
    node))

(defun ost-tree-remove (tree id-or-node)
  "Remove a node from TREE by id or node reference.

Note: If a node reference is passed, it may end up references another
node due to the swap operation. In that case, the symbol 'swap is
returned."
  (let (node id swapped)
    (if (ost-node-p id-or-node)
        (setq node id-or-node
              id (ost-node-id id-or-node))
      (setq id id-or-node
            node (ost-tree-node-by-id tree id-or-node)))
    (cl-assert (ost-node-p node))
    (cl-assert (gethash id (ost-tree-nodes tree)))
    (when (eq 'swap (setq swapped (ost-remove tree node)))
      ;; The node wasn't deleted directly. Instead its values (id and key) were
      ;; swapped with its successor, and the successor node was deleted
      ;; instead. Thus `node' now references the successor node, which we need
      ;; to update in the hashmap.
      (puthash (ost-node-id node) node (ost-tree-nodes tree)))
    (remhash id (ost-tree-nodes tree))
    swapped))

(defun ost-tree-move (tree node new-rank)
  "Move NODE in TREE to NEW-RANK."
  ;; Size cannot increase when moving
  (cl-assert (<= 0 new-rank (1- (ost-tree-size tree))))
  (unless (ost-node-p node)
    (setq node (ost-tree-node-by-id tree node)))
  (cl-assert (ost-node-p node))
  ;; Extract id first in case node gets swapped
  (let ((id (ost-node-id node))
        (swap-p (ost-tree-remove tree node))
        (key-or-rank new-rank))
    (unless (ost-tree-dynamic tree)
      (setq key-or-rank (ost--key-for-rank tree new-rank)))
    ;; Returns new node (useful in case of swap)
    (ost-tree-insert tree key-or-rank id)))

(defun ost-tree-move-many (tree new-ranks-alist)
  "Atomically move multiple nodes to new ranks in TREE.

NEW-RANKS-ALIST is an alist of (ID . NEW-RANK) pairs."
  (when new-ranks-alist
    (let ((tree-size (ost-tree-size tree))
          (nodes (ost-tree-nodes tree))
          (ids (mapcar #'car new-ranks-alist))
          (new-ranks (mapcar #'cdr new-ranks-alist)))

      ;; Ensure there are no duplicate destination ranks
      (when (> (length new-ranks)
               (length (seq-uniq new-ranks)))
        (error "Duplicate new-ranks found in NEW-RANKS-ALIST, which is ambiguous"))

      (dolist (pair new-ranks-alist)
        (let ((id (car pair))
              (rank (cdr pair)))
          ;; Ensure id exists
          (unless (gethash id nodes)
            (error "Node with ID %S not found in tree" id))
          ;; Ensure new rank is within bound
          (when (or (< rank 0) (>= rank tree-size))
            (error "New rank %d is out of bounds for tree of size %d"
                   rank tree-size))))

      ;; Remove all the nodes that need to be moved.
      (dolist (id ids)
        (ost-tree-remove tree id))

      ;; Sort the moves by their target rank to ensure orderly insertion.
      (let ((sorted-moves (sort (copy-alist new-ranks-alist) :lessp #'< :key #'cdr)))
        (dolist (pair sorted-moves)
          (let* ((id (car pair))
                 (new-rank (cdr pair))
                 (key-or-rank new-rank))
            (unless (ost-tree-dynamic tree)
              (setq key-or-rank (ost--key-for-rank tree new-rank)))
            (ost-tree-insert tree key-or-rank id)))))))

;;;; Utilities

(defun ost--opposite (direction)
  (if (eq direction 'left) 'right 'left))

(defun ost-print (node &optional level direction)
  "Print out node and descendants in hierarchical fashion."
  (if (ost-tree-p node)
      (ost-print (ost-tree-root node))
    (if (null node)
        (message "Node is nil")
      (let* ((level (or level 0))
             (indent (* 2 level)))
        (message "%s%sNode %s (%s) S:%s"
                 (make-string indent ?\s)
                 (if direction (concat (capitalize (symbol-name direction)) ": ") "")
                 (ost-node-id node)
                 (if (ost-node-black node) "B" "R")
                 (ost-node-size node))
        (when-let ((left (ost-node-left node)))
          (ost-print left (1+ level) 'left))
        (when-let ((right (ost-node-right node)))
          (ost-print right (1+ level) 'right))))))

(defun ost--node-from (tree-or-node)
  (if (ost-tree-p tree-or-node)
      (ost-tree-root tree-or-node)
    tree-or-node))

(defun ost-size (tree-or-node)
  (ost--node-size (ost--node-from tree-or-node)))

(defun ost--key-for-rank (tree rank)
  "Calculate a key that would place a node at RANK in TREE.
This is a helper for move/set-rank operations. It assumes the tree is in
an intermediate state and we are about to insert a new node. RANK is the
rank in the final, larger tree."
  (let ((current-size (ost-tree-size tree)))
    (cond
     ;; Inserting at the very beginning
     ((= rank 0)
      (if (zerop current-size)
          0.0 ; Tree is empty, any key is fine.
        (- (ost-node-key (ost-select tree 0)) 1.0)))

     ;; Inserting at the very end
     ((= rank current-size)
      (+ (ost-node-key (ost-select tree (1- current-size))) 1.0))

     ;; Inserting in the middle: Use midpoint
     (t
      (let* ((pred-node (ost-select tree (1- rank)))
             (succ-node (ost-select tree rank))
             (pred-key (ost-node-key pred-node))
             (succ-key (ost-node-key succ-node)))
        (/ (+ (float pred-key) succ-key) 2.0))))))

;;;; Operations

(cl-defun ost--set-parent-child (&key child parent direction)
  "Set the child of PARENT in DIRECTION to CHILD, and update CHILD to have PARENT as parent."
  (when parent
    (if (eq direction 'left)
        (setf (ost-node-left parent) child)
      (setf (ost-node-right parent) child)))
  (when child
    (setf (ost-node-parent child) parent)))

(defun ost--rotate (tree node direction)
  "Rotate subtree with root NODE in TREE in DIRECTION, one of 'left or 'right.

               P                  
              /                   
            =L=                    
            / \                   
           a   R                  
              / \                 
             b   c               

Left rotation          ^
     |                 |
     |                 |
     v           Right rotation
             
               P
              /
            =R=
            / \
           L   c
          / \
         a   b
"
  (cl-assert (member direction '(left right)))
  
  (let* ((opposite (ost--opposite direction))
         (parent (ost-node-parent node))
         ;; In a rotation, a child of NODE gets replaced by a child of the new
         ;; root. Wether its a right or left child depends on the rotation.
         (new-root (ost-node-child node opposite))
         new-child)

    (unless new-root
      (error "Pivot node (child opposite of rotation direction) is nil"))
    (setq new-child (ost-node-child new-root direction))

    ;; Move the new child: Assign the left/right node of NODE and update the
    ;; child node's parent if the child node is an actual node (not nil).
    (ost--set-parent-child :child new-child :parent node :direction opposite)

    ;; Update the child of the new root to be NODE.
    (ost--set-parent-child :child node :parent new-root :direction direction)

    ;; The new root becomes the parent of NODE, so update NODE, the new root,
    ;; and the parent of NODE to reflect these changes.
    (setf (ost-node-parent new-root) parent)
    (when parent
      (let ((dir (if (eq (ost-node-left parent) node) 'left 'right)))
        (ost--set-parent-child :child new-root :parent parent :direction dir)))
    
    (unless parent
      ;; If NODE was the root of the tree, set new-root to be the root.
      (cl-assert (eq node (ost-tree-root tree)))
      (setf (ost-tree-root tree) new-root))

    ;; Update sizes. The new root's size becomes the old size of the original
    ;; node. The original node's size must be recalculated based on its new
    ;; children.
    (setf (ost-node-size new-root) (ost-node-size node))
    (setf (ost-node-size node) (+ 1
                                  (ost--node-size (ost-node-left node))
                                  (ost--node-size (ost-node-right node))))
    ))

(defun ost--successor (node)
  "Find the in-order successor of NODE.

This is the node with the smallest key larger than NODE's key.

1. If NODE has right child:
   Successor is the leftmost child of its right subtree.
2. Otherwise:
   Successor is lowest ancestor for which NODE lies in its left subtree."
  (if-let ((right (ost-node-right node)))
      ;; NODE has right child: Find leftmost child of right subtree
      (let ((successor right)
            current)
        (when successor
          (while (setq current (ost-node-left successor))
            (setq successor current))
          successor))
    ;; Node has no right child: Find lowest ancestor for which NODE lies in its
    ;; left subtree.
    (let ((current node)
          ancestor)
      (while (and (setq ancestor (ost-node-parent current))
                  (eq (ost-node-direction current) 'right))
        (setq current ancestor))
      ancestor)))

(defun ost--swap-values (n1 n2)
  "Swap the values of two nodes.
For now this means the 'key' and 'id' properties."
  (cl-rotatef (ost-node-id n1) (ost-node-id n2))
  (cl-rotatef (ost-node-key n1) (ost-node-key n2)))

;;;; Lookup

(cl-defun ost-search (tree-or-node key &optional id)
  "Find node with KEY in TREE.
An additional ID may be given as a tie braker in case of duplicate
keys. Note however that when using the `ost-tree-insert' and
`ost-tree-remove' commands, a id -> hashmap object stored in `ost-tree'
is kept in sync with the ost, so that ID may be used to directly
retrieve a node without search."
  (let ((node (ost--node-from tree-or-node)))
    (while (and node
                (or (not (= key (ost-node-key node)))
                    (and id (not (equal id (ost-node-id node))))))
      (setq node (if (ost--< key (ost-node-key node)
                             id (ost-node-id node))
                     (ost-node-left node)
                   (ost-node-right node))))
    node))

(cl-defun ost-select (tree index)
  "Return the node with the INDEX-th smallest key in TREE (0-based).
This is the node at a given 0-based index in the sorted sequence
of all nodes in the tree. Returns nil or signals an error if
INDEX is out of bounds."
  (let* ((root (ost-tree-root tree))
         (total-size (ost--node-size root)))
    (when (or (< index 0) (>= index total-size))
      (error "Index %d is out of bounds for tree of size %d (valid is 0 to %d)"
             index total-size (1- total-size)))

    (let ((node root))
      (while node
        (let ((left-size (ost--node-size (ost-node-left node))))
          (cond
           ;; The index of the current node is `left-size`.
           ((= index left-size)
            (cl-return-from ost-select node))

           ;; The target is in the left subtree. The index remains the same
           ;; relative to the left subtree.
           ((< index left-size)
            (setq node (ost-node-left node)))

           ;; The target is in the right subtree. We have skipped `left-size`
           ;; nodes in the left subtree, plus the current node (1).
           (t
            (setq index (- index (1+ left-size)))
            (setq node (ost-node-right node))))))
      ;; This part should not be reached if the bounds check is correct.
      nil)))

(defun ost-rank (tree node)
  "Return the 0-based rank (index) of NODE in TREE. NODE may also be its ID.
The rank is its position in the in-order traversal of the tree,
starting from 0. Requires NODE to be a node within TREE."
  (unless (ost-node-p node)
    (setq node (ost-tree-node-by-id tree node)))
  (cl-assert (ost-node-p node) nil "Input must be a valid node")
  ;; The rank starts as the number of nodes in its own left subtree.
  (let ((rank (ost--node-size (ost-node-left node)))
        (current node))
    ;; Now, walk up to the root.
    (while (ost-node-parent current)
      ;; If we are coming from a right child, it means we have
      ;; surpassed all the nodes in the parent's left subtree, plus
      ;; the parent itself.
      (when (eq (ost-node-direction current) 'right)
        (let ((parent (ost-node-parent current)))
          (setq rank (+ rank
                        (1+ (ost--node-size (ost-node-left parent)))))))
      (setq current (ost-node-parent current)))
    rank))

;;;; Insert

(cl-defun ost--insert-fixup (tree node)
  "Rebalance tree after inserting NODE.

When a new node is inserted, it is always colored red. After insertion,
following rules might be violated: 1) Root must be black, or 2) Red
nodes cannot have red children."
  (let (parent grandparent uncle direction)
    ;; Loop while the parent exists and is red (a violation)
    (while (and (setq parent (ost-node-parent node))
                (not (ost-node-black parent)))
    
      (setq grandparent (ost-node-parent parent))

      ;; Case #4: Parent is the root (and is red). Color it black and done.
      (unless grandparent
        (setf (ost-node-black parent) t)
        (cl-return-from ost--insert-fixup))
      
      (setq direction (ost-node-direction parent))
      (setq uncle (ost-node-child grandparent (ost--opposite direction)))

      (if (and uncle (not (ost-node-black uncle)))
          ;; Case 2: Uncle is red.
          (progn
            (setf (ost-node-black parent) t
                  (ost-node-black uncle) t
                  (ost-node-black grandparent) nil)
            (setq node grandparent)) ; Move up the tree
        
        ;; In the remaining two contains, the grandparent is black, and the uncle
        ;; is black or is missing. (although not tested for explicitely, we know the
        ;; grandparent is black as the previous insert operations guarentee
        ;; this). The cases differ in if the node and its parent share
        ;; directions or not.
            
        ;; Case #5: Node direction is opposite of parent direction. This means
        ;; we have to do a rotation to the right if parent is a right child, or
        ;; to the left if parent is a left child. NODE then becomes the parent
        ;; (child of grandparent), therefore parent becomes its child. Thus we
        ;; swap 'node' to refer to 'parent', and vice versa.
        (when (not (eq direction (ost-node-direction node)))
          (ost--rotate tree parent direction)
          (cl-rotatef node parent))

        ;; Case #6
        (ost--rotate tree grandparent (ost--opposite direction))
        (setf (ost-node-black parent) t
              (ost-node-black grandparent) nil)
        
        ;; After rotation and recolor, the tree is fixed.
        (cl-return-from ost--insert-fixup))
      )))

(defun ost--insert (tree node parent direction)
  "Insert NODE into TREE as a DIRECTION child of PARENT."
  (cl-assert (null (ost-node-black node))) ; Inserted nodes must always be red
  (cl-assert (member direction '(left right)))
  
  ;; Attach the new node. New node is always inserted as a leaf node, so we are
  ;; never replacing an existing child.
  (when parent
    (cl-assert (null (ost-node-child parent direction))))
  (ost--set-parent-child :child node :parent parent :direction direction)
  
  (if (not parent)
      ;; NODE is root, no need for fixups
      (setf (ost-tree-root tree) node)
    
    ;; Fix RB tree violations
    (ost--insert-fixup tree node))

  ;; Ensure root is always black
  (setf (ost-node-black (ost-tree-root tree)) t))

(defun ost-insert-key (tree key &optional id)
  "Insert a new node into TREE by its KEY, returns node."
  (let ((node (make-ost-node :key key :id id)))
    ;; Standard BST Insert. New node is insert as a leaf node by going down the
    ;; root, comparing the key to each node, going left if it is smaller and
    ;; right otherwise.
    (let ((parent nil)
          (current (ost-tree-root tree))
          (direction 'left)) ; Default for empty tree
      (while current
        (setq parent current)

        ;; As we traverse down the tree to find the insertion point, we
        ;; increment the size of each node along the path, because the new node
        ;; will be part of their subtrees.
        (cl-incf (ost-node-size current))
        
        (if (ost--node-< node current)
            (setq current (ost-node-left current)
                  direction 'left)
          (setq current (ost-node-right current)
                direction 'right)))

      (ost--insert tree node parent direction))
    node))

(defun ost-insert-rank (tree rank id)
  "Insert new node with ID at position RANK in a dynamic TREE."
  (cl-assert (and (ost-tree-dynamic tree)
                  (<= 0 rank (ost-size tree))))
  (let ((node (make-ost-node :id id))
        (direction 'left)
        (current (ost-tree-root tree))
        parent)
    (while current
      (setq parent current)
      ;; Increment size of ancestors on the path down.
      (cl-incf (ost-node-size current))

      (let ((left-size (ost--node-size (ost-node-left current))))
        (if (<= rank left-size)
            ;; Target index is in the left subtree
            (setq current (ost-node-left current)
                  direction 'left)
          ;; Skip left subtree + current node
          (setq rank (- rank (1+ left-size))
                current (ost-node-right current)
                direction 'right))))
    (ost--insert tree node parent direction)
    (puthash id node (ost-tree-nodes tree))
    node))

(defun ost-insert (tree key-or-rank &optional id)
  "Insert new node into the tree.
Dynamic trees require a rank, else a key."
  (cl-assert (numberp key-or-rank))
  (when (and id (ost-tree-node-by-id tree id))
    (error "Node with ID already exists."))
  (if (not (ost-tree-dynamic tree))
      (ost-insert-key tree key-or-rank id)
    (cl-assert id)
    (ost-insert-rank tree key-or-rank id)))

;;;; Remove

(defun ost--decrement-ancestor-sizes (node)
  "Walk up from NODE and decrement the size of each ancestor."
  (while node
    (cl-decf (ost-node-size node))
    (setq node (ost-node-parent node))))

(cl-defun ost--remove-fixup (tree node parent direction)
  "Rebalance TREE after removal of black NODE with no children.

This assumes PARENT has already replace NODE with nil."
  (while parent
    (let* ((opposite (ost--opposite direction))
           (sibling (ost-node-child parent opposite))
           (close-nephew (when sibling (ost-node-child sibling direction)))
           (distant-nephew (when sibling (ost-node-child sibling opposite))))

      (when (and sibling (ost-node-red sibling))
        ;; Case #3
        (ost--rotate tree parent direction)
        (setf (ost-node-black parent) nil
              (ost-node-black sibling) t)

        ;; Reassign value after rotation
        (setq sibling (ost-node-child parent opposite))
        (setq close-nephew (when sibling (ost-node-child sibling direction))
              distant-nephew (when sibling (ost-node-child sibling opposite))))

      (when (or (and distant-nephew (ost-node-red distant-nephew))
                (and close-nephew (ost-node-red close-nephew)))

        (when (and close-nephew
                   (ost-node-red close-nephew)
                   (or (null distant-nephew)
                       (ost-node-black distant-nephew)))
          ;; Case #5
          (ost--rotate tree sibling opposite)
          (setf (ost-node-black sibling) nil
                (ost-node-black close-nephew) t)

          ;; Update sibling and distant nephew for case #6
          (setq distant-nephew sibling)
          (setq sibling close-nephew))

        ;; Case #6
        (ost--rotate tree parent direction)
        (setf (ost-node-black sibling) (ost-node-black parent))
        (setf (ost-node-black parent) t)
        (when distant-nephew
          (setf (ost-node-black distant-nephew) t))
        (cl-return-from ost--remove-fixup))

      (when (ost-node-red parent)
        ;; Case #4
        (setf (ost-node-black sibling) nil
              (ost-node-black parent) t)
        (cl-return-from ost--remove-fixup))

      ;; Case #2
      (setf (ost-node-black sibling) nil)
      (setq node parent)
      (setq parent (ost-node-parent node))
      (when parent
        (setq direction (ost-node-direction node)))

      )
    ))


(defun ost-remove (tree node)
  "Remove NODE from TREE."
  (cl-assert (ost-tree-p tree))
  (cl-assert (ost-node-p node))
  
  (let ((parent (ost-node-parent node))
        (direction (ost-node-direction node))
        (left (ost-node-left node))
        (right (ost-node-right node)))
    (cond
     ;; No children. In standard BST, simply set to nil.
     ((and (null left) (null right))
      (if (eq (ost-tree-root tree) node)
          ;; NODE is root. Set root to nil.
          (setf (ost-tree-root tree) nil)
        
        ;; Otherwise set the child property of its parent to nil
        (ost--set-parent-child :child nil :parent parent :direction direction)

        ;; Decrement size of all ancestors
        (ost--decrement-ancestor-sizes parent)

        ;; When node is black, we have an inbalance that needs to be fixed
        (when (ost-node-black node)
          (ost--remove-fixup tree node parent direction))))
      
     ;; Two children: Swap its value with its in-order successor (which in this
     ;; case, has to be the leftmost child of the right subtree), and then
     ;; delete the successor. Since the successor is leftmost, it can only have
     ;; a right child or no child at all.
     ((and left right)
      (let ((successor (ost--successor node)))
        (ost--swap-values node successor)
        
        ;; Node size decrements handled in recursive call
        (ost-remove tree successor)

        ;; Return value indicates swapping so that any outside state can be
        ;; synced. Swapping will make any swapped node reference out of sync.
        'swap))

     ;; One child. Replace NODE with child and color black
     (t
      (let ((child (or left right)))
        ;; Child must be red and NODE must be black
        (cl-assert (null (ost-node-black child)))
        (cl-assert (ost-node-black node))

        ;; Decrement size of all ancestors. Since NODE is being replaced by 
        ;; child, the counts don't change below PARENT.
        (ost--decrement-ancestor-sizes parent)

        ;; (ost--set-parent-child :child child :parent parent :direction direction)
        
        ;; Replace node with its child.
        (if parent
            ;; If there's a parent, connect child to it.
            (ost--set-parent-child :child child :parent parent :direction direction)
          ;; Otherwise, NODE was the root. The child is the new root.
          (progn
            (setf (ost-tree-root tree) child)
            ;; The new root has no parent.
            (setf (ost-node-parent child) nil)))

        ;; The replacing child must become black.
        (setf (ost-node-black child) t))))))

;;;; Build

(defun ost--random-tree (n)
  "Build a random red-black tree with N nodes."
  (let ((tree (make-ost-tree)))
    (dotimes (_ n)
      (let ((key (random 1000)))
        (ost-insert tree key key)))
    tree))

(defun ost--sequence-tree (n &optional with-map print-p)
  "Build a tree with sequence of ordered keys."
  (let ((tree (make-ost-tree)))
    (dotimes (key n)
      (if with-map
          (ost-tree-insert tree key key)
        (ost-insert tree key key))
      (when print-p (ost-print tree)))
    tree))

(defun ost--from-keys (keys)
  (let ((tree (make-ost-tree)))
    (dolist (key keys tree)
      (ost-insert tree key key))))

;;;; Storage

(defun ost-write (tree file)
  (let (nodes)
    (maphash
     (lambda (id node)
       (let* ((parent (ost-node-parent node))
              (left (ost-node-left node))
              (right (ost-node-right node)))
         (push (list id
                     (ost-node-key node)
                     (ost-node-black node)
                     (ost-node-size node)
                     (when parent (ost-node-id parent))
                     (when left (ost-node-id left))
                     (when right (ost-node-id right)))
               nodes)))
     (ost-tree-nodes tree))
    (with-temp-file file
      (let* ((print-level nil)
             (print-length nil)
             (root (ost-tree-root tree))
             (data (list :root (when root (ost-node-id root))
                         :dynamic (ost-tree-dynamic tree)
                         :nodes nodes)))
        (prin1 data (current-buffer)))))
  nil)

(defun ost-read (file)
  (when (file-exists-p file)
    (let* ((data (with-temp-buffer
                   (insert-file-contents file)
                   (read (current-buffer))))
           (tree (make-ost-tree :dynamic (plist-get data :dynamic))))

      ;; Pass 1: Create nodes without linking them
      (dolist (node-data (plist-get data :nodes))
        (cl-destructuring-bind (id key black size parent left right) node-data
          (let ((node (make-ost-node :black black :key key :id id :size size
                                     :left left :right right :parent parent)))
            (puthash id node (ost-tree-nodes tree)))))

      ;; Pass 2: Link nodes together
      (dolist (node-data (plist-get data :nodes))
        (let* ((id (car node-data))
               (node (gethash id (ost-tree-nodes tree))))
          (setf (ost-node-left node) (gethash (ost-node-left node) (ost-tree-nodes tree))
                (ost-node-right node) (gethash (ost-node-right node) (ost-tree-nodes tree))
                (ost-node-parent node) (gethash (ost-node-parent node) (ost-tree-nodes tree)))))

      ;; Tree root node
      (when-let ((root-id (plist-get data :root)))
        (setf (ost-tree-root tree) (gethash root-id (ost-tree-nodes tree))))

      tree)))
    
    

;;;; Footer

(provide 'ost)

;;; ost.el ends here

