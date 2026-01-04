(define-minor-mode traverse-mode
  "Toggle Traverse mode.

When enabled, `xref-find-definitions' will push the current context (i.e. line)
onto a tree structure. `xref-go-back' will move a level up the tree.

A tree visualization can be toggled with `traverse-toggle-tree-visualization'."
  :global nil
  :init-value nil
  :lighter " Traverse"
  :interactive t

  (if traverse-mode
      (progn
        (setq traverse--root-node
              (traverse--tree-root-init))
        
        (setq traverse--current-node traverse--root-node)
        
        (advice-add 'xref-find-definitions :before #'traverse--goto-child)
        (advice-add 'xref-go-back :after #'traverse--goto-parent))
    
    (progn
      (setq traverse--root-node nil)
      (setq traverse--current-node nil)
      (advice-remove 'xref-find-definitions 'traverse--goto-child)
      (advice-remove 'xref-go-back 'traverse--goto-parent))))


(require 'cl-lib)


(cl-defstruct traverse--tree
  "Tree structure for traversing a codebase.

`context'   A node's context (i.e. the line contents).
`parent'    The scope in which `context' resides.
`file'      The file in which `context' resides.
`line'      The line number on which `context' resides.
`children'  Nodes referenced within the scope of `context'."
  
  context
  parent
  file
  line
  children)


(defvar traverse--root-node nil
  "The root node of the traverse tree.")

(defvar traverse--current-node nil
  "The current node of the traverse tree.")


(defun traverse--tree-root-init ()
  "Creates a new `traverse--tree' root node."
  (make-traverse--tree
   :context 'root
   :parent nil
   :file nil
   :line nil
   :children '()))


(defun traverse--goto-child (&rest rest)
  "If the current context is present in the tree,
sets `traverse--current-node' to that node.

Otherwise, creates a new node and updates `traverse--current-node'
to point to the new node."
  
  (let*
      ((context             (thing-at-point 'line t))
       (file                (buffer-file-name))
       (line                (line-number-at-pos))
       (existent-node       (traverse--find-node traverse--root-node context file line)))

    (setq traverse--current-node
          (if existent-node
              existent-node

            ;; If node does not exist, create it!
            (traverse--insert-node
             traverse--current-node context file line)))))


(defun traverse--find-node (root context file line)
  "Returns the node which contains a matching
`context', `file' and `line'. If no such node exists,
returns nil.

The search begins from the node `root'."

  ;; INFO
  ;; Use DFS instead of BFS as the appeal of `traverse-mode' is
  ;; in its ability to visualize deep backtraces.

  (let*
      ((root-context      (traverse--tree-context  root))
       (root-file         (traverse--tree-file     root))
       (root-line         (traverse--tree-line     root))
       (root-children     (traverse--tree-children root)))
    
    (if (and
         (stringp root-context)
         (stringp root-file)
         (numberp line)
         (string= context root-context)
         (string= file    root-file)
         (=       line    root-line))
        ;; If `context', `file', and `line' match, return `root'.
        root

      ;; Check if node exists in the sub-trees.
      (seq-some
            (lambda (child)
              "Recurse on child."
              (traverse--find-node child context file line))
            root-children))))


(defun traverse--insert-node (parent context file line)
  "Saves the `context', `file', and `line' into a new node, and
adds it as a child to the node `parent'. Returns the new node."
  
  (let*
      ;; Create the child node.
      ((child (make-traverse--tree
               :context context
               :parent parent
               :file file
               :line line
               :children '())))

    ;; Add the child node as a child to `parent'.
    (setf (traverse--tree-children parent)
          (cons child
                (traverse--tree-children parent)))

    ;; Return the new node.
    child))


(defun traverse--goto-parent (&rest rest)
  "Updates `traverse--current-node' to refer to its parent node.
If `traverse--current-node' is the root node, nothing happens."

  (let*
      ((parent (traverse--tree-parent traverse--current-node)))
    (if parent
        (setq traverse--current-node parent)
      (message "traverse.el: Tree root has no parent!"))))


(defun traverse-toggle-tree-visualization ()
  "Visualizes the traverse tree."
  (interactive)
  (print traverse--root-node t))

