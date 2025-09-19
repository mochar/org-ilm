;;; ost.el --- Ordered statistic tree with red-black B-tree base -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: M Charrout
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: 

;;; Commentary:

;; This package implements ordered statistic trees with a red-black binary
;; search tree as a self-balancing base.

;; An ordered statistic tree holds an additional value in each node: the size of
;; the subtree rooted at that node (number of nodes below it).

;; References:
;; - https://en.wikipedia.org/wiki/Red-black_tree
;; - https://en.wikipedia.org/wiki/Order_statistic_tree
;; - https://en.wikipedia.org/wiki/Binary_search_tree

;;; Code:

(require 'cl-lib)

;;;; Node
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
  (data
   nil
   :documentation "Additional data to store in each node.")
  )

(cl-defstruct ost-tree
  "An ordered statistic, red-black, binary search tree."
  (root
   nil
   :documentation "The root node of the tree. Need not be black."
   :type ost-node))

(defun ost--node-print (node &optional level direction)
  "Print out node and descendants in hierarchical fashion."
  (if (ost-tree-p node)
      (ost--node-print (ost-tree-root node))
    (let* ((level (or level 0))
           (indent (* 2 level)))
      (message "%s%sNode (%s) %s"
               (make-string indent ?\s)
               (if direction (concat (capitalize (symbol-name direction)) ": ") "")
               (if (ost-node-black node) "B" "R")
               (ost-node-data node))
      (when-let ((left (ost-node-left node)))
        (ost--node-print left (1+ level) 'left))
      (when-let ((right (ost-node-right node)))
        (ost--node-print right (1+ level) 'right)))))

(defun ost--node-child (node direction)
  "Get the child of NODE in DIRECTION ('left or 'right)."
  (if (eq direction 'left)
      (ost-node-left node)
    (ost-node-right node)))

(defun ost-node-red (node)
  (not (ost-node-black node)))

(cl-defun ost--set-parent-child (&key child parent direction)
  "Set the child of PARENT in DIRECTION to CHILD, and update CHILD to have PARENT as parent."
  (when parent
    (if (eq direction 'left)
        (setf (ost-node-left parent) child)
      (setf (ost-node-right parent) child)))
  (when child
    (setf (ost-node-parent child) parent)))

(defun ost--direction (node)
  "Return the direction ('left or 'right) of NODE relative to its parent."
  (let ((parent (ost-node-parent node)))
    (when parent
      (if (eq node (ost-node-right parent))
          'right
        'left))))

(defun ost--opposite (direction)
  (if (eq direction 'left) 'right 'left))

;;;; Operations

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
         (new-root (ost--node-child node opposite))
         new-child)

    (unless new-root
      (error "Pivot node (child opposite of rotation direction) is nil"))
    (setq new-child (ost--node-child new-root direction))

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
      (setf (ost-tree-root tree) new-root))))

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
                  (eq (ost--direction current) 'right))
        (setq current ancestor))
      ancestor)))

(defun ost--swap-values (n1 n2)
  "Swap the values of two nodes.
For now this means the 'key' and 'data' properties."
  (cl-rotatef (ost-node-data n1) (ost-node-data n2))
  (cl-rotatef (ost-node-key n1) (ost-node-key n2)))

;;;; Search

(cl-defun ost-search (tree key)
  "Find node with KEY in TREE."
  (let ((node (ost-tree-root tree)))
    (while (and node (not (= key (ost-node-key node))))
      (setq node (if (< key (ost-node-key node))
                     (ost-node-left node)
                   (ost-node-right node))))
    node))

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
      
      (setq direction (ost--direction parent))
      (setq uncle (ost--node-child grandparent (ost--opposite direction)))

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
        (when (not (eq direction (ost--direction node)))
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
    (cl-assert (null (ost--node-child parent direction))))
  (ost--set-parent-child :child node :parent parent :direction direction)
  
  (if (not parent)
      ;; NODE is root, no need for fixups
      (setf (ost-tree-root tree) node)
    
    ;; Fix RB tree violations
    (ost--insert-fixup tree node))

  ;; Ensure root is always black
  (setf (ost-node-black (ost-tree-root tree)) t))

(defun ost-insert (tree key &optional data)
  "Insert a new node into TREE by its KEY, returns node."
  (let ((node (make-ost-node :key key :data data)))
    ;; Standard BST Insert. New node is insert as a leaf node by going down the
    ;; root, comparing the key to each node, going left if it is smaller and
    ;; right otherwise.
    (let ((parent nil)
          (current (ost-tree-root tree))
          (direction 'left)) ; Default for empty tree
      (while current
        (setq parent current)
        (if (< (ost-node-key node) (ost-node-key current))
            (setq current (ost-node-left current)
                  direction 'left)
          (setq current (ost-node-right current)
                direction 'right)))

      (ost--insert tree node parent direction))
    node))

;;;; Remove

(cl-defun ost--remove-fixup (tree node parent direction)
  "Rebalance TREE after removal of black NODE with no children.

This assumes PARENT has already replace NODE with nil."
  (while parent
    (let* ((opposite (ost--opposite direction))
           (sibling (ost--node-child parent opposite))
           (close-nephew (when sibling (ost--node-child sibling direction)))
           (distant-nephew (when sibling (ost--node-child sibling opposite))))

      (when (and sibling (ost-node-red node))
        ;; Case #3
        (ost--rotate tree parent direction)
        (setf (ost-node-black parent) nil
              (ost-node-black sibling) t)

        ;; Reassign value after rotation
        (setq sibling (ost--node-child parent opposite))
        (setq close-nephew (when sibling (ost--node-child sibling direction))
              distant-nephew (when sibling (ost--node-child sibling opposite))))

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
        (setq direction (ost--direction node)))

      )
    ))

(defun ost-remove (tree node)
  "Remove NODE from TREE."
  (cl-assert (ost-tree-p tree))
  (cl-assert (ost-node-p node))
  
  (let ((parent (ost-node-parent node))
        (direction (ost--direction node))
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
        (ost-remove tree successor)))

     ;; One child. Replace NODE with child and color black
     (t
      (let ((child (or left right)))
        ;; Child must be red and NODE must be black
        (cl-assert (null (ost-node-black child)))
        (cl-assert (ost-node-black node))
        (ost--set-parent-child :child child :parent parent :direction direction)
        (setf (ost-node-black child) t))))))

;;;; Build

(defun ost--random-tree (n)
  "Build a random red-black tree with N nodes."
  (let ((tree (make-ost-tree)))
    (dotimes (_ n)
      (let ((key (random 1000)))
        (ost-insert tree key key)))
    tree))

(defun ost--sequence-tree (n &optional print-p)
  "Build a tree with sequence of ordered keys."
  (let ((tree (make-ost-tree)))
    (dotimes (key n)
      (ost-insert tree key key)
      (when print-p (ost--node-print tree)))
    tree))

;;;; Footer

(provide 'ost)

;;; ost.el ends here

