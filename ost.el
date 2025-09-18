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
;; - https://www.geeksforgeeks.org/dsa/introduction-to-red-black-tree/


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

(defun ost--node-child (node direction)
  "Get the child of NODE in DIRECTION ('left or 'right)."
  (if (eq direction 'left)
      (ost-node-left node)
    (ost-node-right node)))

(cl-defun ost--node-child-set (&key child parent direction)
  "Set the child of PARENT in DIRECTION to CHILD."
  (if (eq direction 'left)
      (setf (ost-node-left parent) child)
    (setf (ost-node-right parent) child)))

(defun ost--direction (node)
  "Return the direction ('left or 'right) of NODE relative to its parent."
  (let ((parent (ost-node-parent node)))
    (when parent
      (if (eq node (ost-node-right parent))
          'right
        'left))))

(defun ost--opposite (direction)
  (if (eq direction 'left) 'right 'left))

(defun ost--rotate (tree node direction)
  "Rotate subtree with root NODE in TREE in DIRECTION, one of 'left or 'right."
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
    (ost--node-child-set :child new-child :parent node :direction opposite)
    (when new-child
      (setf (ost-node-parent new-child) node))

    ;; Update the child of the new root to be NODE.
    (ost--node-child-set :child node :parent new-root :direction direction)

    ;; The new root becomes the parent of NODE, so update NODE, the new root,
    ;; and the parent of NODE to reflect these changes.
    (setf (ost-node-parent new-root) parent
          (ost-node-parent node) new-root)
    (if parent
        (ost--node-child-set :child new-root :parent parent
                             :direction (if (eq (ost-node-left parent) node)
                                            'left 'right))
      ;; If NODE was the root of the tree, set new-root to be the root.
      (cl-assert (eq node (ost-tree-root tree)))
      (setf (ost-tree-root tree) new-root))))

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
        
        ;; In the remaining two contains, both the grandparent and the uncle
        ;; are black (although not tested for explicitely, we know the
        ;; grandparent is black as the previous insert operations guarentee
        ;; this). The cases differ in if the node and its parent share
        ;; directions or not.
            
        ;; Case #5: Node direction is opposite of parent direction. This
        ;; means we have to do a rotation to the right if parent is a right
        ;; child, or to the left if parent is a left child. The consequence
        ;; is that NODE and PARENT now swap places.
        (when (not (eq direction (ost--direction node)))
          (ost--rotate tree parent direction)
          (setq node parent
                parent (ost--node-child grandparent direction)))

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
  
  ;; Attach the new node
  (setf (ost-node-parent node) parent)
  
  (if (not parent)
      ;; NODE is root, no need for fixups. Set to black and done.
      (setf (ost-tree-root tree) node
            (ost-node-black node) t)
    
    ;; New node is always insert as a leaf node, so we are never replacing an
    ;; existing child.
    (ost--node-child-set :child node :parent parent :direction direction)

    ;; Fix RB tree violations
    (ost--insert-fixup tree node)

    ;; Ensure root is always black
    (setf (ost-node-black (ost-tree-root tree)) t)))

(defun ost-insert (tree key &optional data)
  "Insert a new node into TREE by its KEY."
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

      (ost--insert tree node parent direction))))


;;;; Footer

(provide 'ost)

;;; ost.el ends here

