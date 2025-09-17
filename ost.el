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

;;;; Red-black tree
;; Red-Black Tree properties:
;; - Node Color: Each node is either red or black.
;; - Root Property: The root of the tree is always black. (contested?)
;; - Red Property: Red nodes cannot have red children (no two consecutive
;;   red nodes on any path).
;; - Black Property: Every path from a node to its descendant null nodes
;;   (leaves) has the same number of black nodes.
;; - Leaf/nil Property: All leaves (NIL nodes) are black.

;; Insert and delete operations need corrections as violations can occur after
;; these operations.

;; Insertion
;; When a new node is inserted, it is always colored red.
;; Potential violations:
;; 1. Root must be black
;; 2. Red nodes cannot have red children

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
  )

(cl-defstruct ost-tree
  "An ordered statistic, red-black, binary search tree."
  (root
   nil
   :documentation "The root node of the tree. Need not be black."
   :type ost-node))

(defun ost-rotate (tree node direction)
  "Rotate subtree with root NODE in TREE in DIRECTION, one of 'left or 'right."
  (cl-assert (member direction '(left right)))
  (let* ((right-p (eq direction 'right))
         (parent (ost-node-parent node))
         ;; In a rotation, a child of NODE gets replaced by a child of the new
         ;; root. Wether its a right or left child depends on the rotation,
         ;; hence the ifs everywhere.
         (new-root (if right-p (ost-node-left node) (ost-node-right node)))
         (new-child (if right-p (ost-node-right new-root) (ost-node-left new-root))))

    ;; Move the new child: Assign the left/right node of NODE and update the
    ;; child node's parent if the child node is an actual node (not nil).
    (setf (if right-p (ost-node-left node) (ost-node-right node)) new-child)
    (when new-child
      (setf (ost-node-parent new-child) node))

    ;; Update the child of the new root to be NODE.
    (setf (if right-p (ost-node-right new-root) (ost-node-left new-root)) node)

    ;; The new root becomes the parent of NODE, so update NODE, the new root,
    ;; and the parent of NODE to reflect these changes.
    (setf (ost-node-parent new-root) parent
          (ost-node-parent node) new-root)
    (if parent
        (setf (if (eq (ost-node-left parent) node)
                  (ost-node-left parent)
                (ost-node-right parent)) new-root)
      ;; If NODE was the root of the tree, set new-root to be the root.
      (cl-assert (eq node (ost-tree-root tree)))
      (setf (ost-tree-root tree) new-root))
  ))


;;;; Footer

(provide 'ost)

;;; ost.el ends here

