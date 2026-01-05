(require 'erc)
(load-file "./traverse.el")

(ert-deftest test--tree-root-init ()
  "Tests the creation of a new tree root."
  (let*
      ((root (traverse--tree-root-init)))
    (should (traverse--tree-p root))
    (should (equal (traverse--tree-context root)   'root))
    (should (equal (traverse--tree-parent root)    nil))
    (should (equal (traverse--tree-file root)      nil))
    (should (equal (traverse--tree-line root)      nil))
    (should (equal (traverse--tree-children root)  '()))))


(ert-deftest test--create-child ()
  "Tests the creation of a new node."
  (let*
      ((foo (make-traverse--tree
             :context "foo()"
             :parent nil
             :file "foo.c"
             :line 1
             :children nil))

       (bar (make-traverse--tree
             :context "bar()"
             :parent nil
             :file "bar.c"
             :line 2
             :children nil))

       (baz (make-traverse--tree
             :context "baz()"
             :parent bar
             :file "baz.c"
             :line 3
             :children nil))
            
       (root (make-traverse--tree
              :context 'root
              :parent nil
              :file nil
              :line nil
              :children (list foo bar)))
       
       (context "foobar()")
       (file    "foo.c")
       (line    4))

    (setf (traverse--tree-parent foo) root)
    (setf (traverse--tree-parent bar) root)
    (setf (traverse--tree-children bar) (list baz))

    (should (equal (traverse--find root context file line) nil))
    (traverse--insert root context file line)
    (should (not
             (equal (traverse--find root context file line) nil)))))


(ert-deftest test--find-child ()
  "Tests finding an already-existent node."
  (let*
      ((foo (make-traverse--tree
             :context "foo()"
             :parent nil
             :file "foo.c"
             :line 1
             :children nil))

       (bar (make-traverse--tree
             :context "bar()"
             :parent nil
             :file "bar.c"
             :line 2
             :children nil))

       (baz (make-traverse--tree
             :context "baz()"
             :parent bar
             :file "baz.c"
             :line 3
             :children nil))
            
       (root (make-traverse--tree
              :context 'root
              :parent nil
              :file nil
              :line nil
              :children (list foo bar))))

    (setf (traverse--tree-parent foo) root)
    (setf (traverse--tree-parent bar) root)
    (setf (traverse--tree-children bar) (list baz))

    (setf (traverse--tree-children bar) (list baz))

    (should (not
             (equal (traverse--find root
                                    (traverse--tree-context baz)
                                    (traverse--tree-file baz)
                                    (traverse--tree-line baz))
                    nil)))))

    
