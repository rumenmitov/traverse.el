;; -- Welcome to Emacs --


(require 'cl-lib)

(cl-defstruct BacktraceTree
  symbol    ;; The node's symbol.
  parent 		;; The parent function in which `symbol' resides.
  file      ;; The file in which `symbol' resides.
  children  ;; Functions called within the scope of `symbol'.
  )

(setq btTree (make-BacktraceTree :symbol nil :parent nil :file nil :children '()))

;; Insert new node.
(setf (BacktraceTree-symbol btTree) (substring-no-properties (thing-at-point 'defun)))

;; Insert child node.
(setq childTree (make-BacktraceTree :symbol "this is the child" :parent btTree :file nil :children '()))
(setf (BacktraceTree-children btTree) (cons childTree (BacktraceTree-children btTree)))

(setq btTree childTree)
(print btTree t)


