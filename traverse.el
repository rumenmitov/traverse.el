(define-minor-mode traverse-mode
  "Toggle Traverse mode.

When enabled, `xref-find-definitions' will push the current symbol onto a tree structure.
`xref-go-back' will move a level up the tree.

A tree visualization can be toggled with `traverse-toggle-tree-visualization'.
"
  :global nil
  :init-value nil
  :lighter " Traverse"
  :interactive '( 'prog-mode )

  (if traverse-mode
      (progn
        (setq traverse--root-node
              (traverse--tree-root-init))
        
        (setq traverse--current-node traverse--root-node)
        
        (advice-add 'xref-find-definitions :before 'traverse--goto-child)
        (advice-add 'xref-go-back :after 'traverse--goto-parent))
    
    (progn
      (setq traverse--root-node nil)
      (setq traverse--current-node nil)
      (advice-remove 'xref-find-definitions 'traverse--goto-child)
      (advice-remove 'xref-go-back 'traverse--goto-parent))))


(require 'cl-lib)


(cl-defstruct traverse--tree
  "Tree structure for traversing a codebase.

`symbol'    A node's symbol.
`parent'    The scope in which `symbol' resides.
`file'      The file in which `symbol' resides.
`line'      The line number on which `symbol' resides.
`children'  Nodes referenced within the scope of `symbol'."
  
  symbol
  parent
  file
  line
  children)


(defvar traverse--root-node nil
  "The root node of the traverse tree.")

(defvar traverse--current-node nil
  "The current node of the traverse tree.")


(defun traverse--tree-root-init ()
  "Create a new `traverse--tree' root node."
  (make-traverse--tree
   :symbol 'root
   :parent nil
   :file nil
   :line nil
   :children '()))


(defun traverse--goto-child (&rest rest)
  "Save the current context into a child node.
Update `traverse--current-node' to the new child node."
  
  (let*
      ;; Create the child node with the current context.
      ((child (make-traverse--tree
               :symbol (thing-at-point 'line t)
               :parent traverse--current-node
               :file (buffer-file-name)
               :line (line-number-at-pos)
               :children '())))

    ;; Add the child node as a child to `traverse--current-node'.
    (setf (traverse--tree-children traverse--current-node)
          (cons child
                (traverse--tree-children traverse--current-node)))

    ;; Update `traverse--current-node' to the new child.
    (setq traverse--current-node child)))


(defun traverse--goto-parent (&rest rest)
  "Update `traverse--current-node' to refer to its parent node.
If `traverse--current-node' is the root node, don't do anything."

  (let*
      ((parent (traverse--tree-parent traverse--current-node)))
    (if parent
        (setq traverse--current-node parent)
      (message "traverse.el: Tree root has no parent!"))))


(defun traverse-toggle-tree-visualization ()
  "Visualize the traverse tree."
  (interactive)
  (print traverse--root-node t))



