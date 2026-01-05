(define-minor-mode traverse-mode
  "Toggle Traverse mode.

When enabled, `xref-find-definitions' will push the current context (i.e. line)
onto a graph structure. `xref-go-back' will move a level up the graph.

A graph visualization can be toggled with `traverse-toggle-graph-visualization'."
  :global nil
  :init-value nil
  :lighter " Traverse"
  :interactive t

  (if traverse-mode
      (progn
        (setq traverse--root-node (traverse--root-init))
        (setq traverse--all-child-nodes nil)
        
        (advice-add 'xref-find-definitions :around #'traverse--traverse)
        (advice-add 'xref-go-back :around #'traverse--traverse))
    
    (progn
      (advice-remove 'xref-find-definitions 'traverse--traverse)
      (advice-remove 'xref-go-back 'traverse--traverse))))


(require 'cl-lib)


(cl-defstruct traverse--graph
  "Graph structure for traversing a codebase.

`context'   A node's context (i.e. the line contents).
`parents'   Scopes in which `context' resides.
`file'      The file in which `context' resides.
`line'      The line number on which `context' resides.
`children'  Nodes referenced within the scope of `context'."
  
  context
  parents
  file
  line
  children)


(defvar traverse--root-node nil
  "The starting node. It does not have a parent.")

(defvar traverse--all-child-nodes nil
  "All non-root nodes.")


(defun traverse--root-init ()
  "Creates an initialized root node."
  (make-traverse--graph
   :context      'root
   :parents      nil
   :file         nil
   :line         nil
   :children     nil))


(defun traverse--traverse (oldfunc &rest rest)
  "Handles traversal via `oldfunc'. The current context is
registered as a parent node. The context after the traversal
step is registered as a child node."

  (let*
      ((parent (traverse--handle-context-at-point))
       (_      (apply oldfunc rest))
       (child  (traverse--handle-context-at-point)))

    (traverse--connect parent child)))


(defun traverse--handle-context-at-point ()
  "Finds (or creates) a node that describes the
current context at point."
  
  (let*
      ((context          (thing-at-point 'line t))
       (file             (buffer-file-name))
       (line             (line-number-at-pos))
       (existent-node    (traverse--find context file line)))

    (if existent-node
        existent-node
      (traverse--create context file line)))) 


(defun traverse--find (context file line)
  "Returns the node which contains a matching
`context', `file' and `line'. If no such node exists,
returns nil."

  (message "content: %s\nfile: %s\nline: %d\n" context file line)
  (seq-some (lambda (node)
                (let*
                    ((node-context      (traverse--graph-context  node))
                     (node-file         (traverse--graph-file     node))
                     (node-line         (traverse--graph-line     node)))
                  
                  (if (and
                       (string= context node-context)
                       (string= file    node-file)
                       (=       line    node-line))
                      node
                    nil)))
            
            traverse--all-child-nodes))


(defun traverse--create (context file line)
  "Returns a new node with the provided `context', `file', and `line'."
  
      (make-traverse--graph
       :context context
       :parents nil
       :file file
       :line line
       :children nil))


(defun traverse--connect (parent child)
  "Connects `parent' and `child' amongst themselves,
as well as to the overall graph.

A child cannot be its own parent and vice-versa! If `child'
is the same as `parent', returns `nil'. Returns `t'
otherwise.

If the `parent' node has no parents,
sets `traverse--root-node' as its parent.

Establishes the `parent' node as the parent of the `child',
and the `child' node as a child of the `parent'."

  (if (equal parent child)
      nil
    (progn
      (add-to-list 'traverse--all-child-nodes parent)
      (add-to-list 'traverse--all-child-nodes child)

      (if (not (traverse--graph-parents parent))
          (progn
            (setf (traverse--graph-parents parent) (list traverse--root-node))
            (setf (traverse--graph-children traverse--root-node)
                  (cons parent (traverse--graph-children traverse--root-node)))))
      
      (setf (traverse--graph-parents child)
            (cons parent (traverse--graph-parents child)))

      (setf (traverse--graph-children parent)
            (cons child (traverse--graph-children parent)))
      
      t)))


(defun traverse-toggle-graph-visualization ()
  "Visualizes the traverse graph."
  (interactive)
  (print traverse--root-node t))

