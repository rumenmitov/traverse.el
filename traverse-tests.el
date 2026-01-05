(require 'erc)
(load-file "./traverse.el")

(defun traverse--tests--reset-state ()
    "Resets global variables for running tests."
  
  (setq traverse--root-node (traverse--root-init))
  (setq traverse--all-child-nodes nil))


(ert-deftest test--root-init ()
  "Tests the creation of a new graph root."

  (traverse--tests--reset-state)
  (let*
      ((root (traverse--root-init)))
    (should (traverse--graph-p root))
    (should (equal (traverse--graph-context root)   'root))
    (should (equal (traverse--graph-parents root)   nil))
    (should (equal (traverse--graph-file root)      nil))
    (should (equal (traverse--graph-line root)      nil))
    (should (equal (traverse--graph-children root)  nil))))


(ert-deftest test--create ()
  "Tests the creation of a new node."

  (traverse--tests--reset-state)
  (let*
      ((node (traverse--create "foo()" "foo.c" 1)))

    (should (traverse--graph-p node))
    (should (string= (traverse--graph-context node) "foo()"))
    (should (string= (traverse--graph-file node)    "foo.c"))
    (should (=       (traverse--graph-line node)    1))))


(ert-deftest test--connect ()
  "Tests connecting a parent and a child with the graph."

  (traverse--tests--reset-state)
  (let*
      ((foo (traverse--create "foo()" "foo.c" 1))
       (bar (traverse--create "bar()" "bar.c" 2)))

    (traverse--connect foo bar)

    ;; NOTE: Should be able to create cycles of degree > 1.
    (traverse--connect bar foo)
    
    (should (seq-contains-p (traverse--graph-parents foo) bar))
    (should (seq-contains-p (traverse--graph-children foo) bar))

    (should (seq-contains-p (traverse--graph-parents bar) foo))
    (should (seq-contains-p (traverse--graph-children bar) foo))

    ;; NOTE: Cycles of degree = 1 are NOT allowed!
    (should (not (traverse--connect foo foo)))))


(ert-deftest test--find ()
  "Tests finding an existent node."

  (traverse--tests--reset-state)  
  (let*
      ((foo (traverse--create "foo()" "foo.c" 1))
       (bar (traverse--create "bar()" "bar.c" 2))
       (baz (traverse--create "baz()" "baz.c" 3))
       (_   (traverse--connect foo bar))
       (_   (traverse--connect bar baz))
       (_   (traverse--connect baz bar)) ;; Create a cycle bar <---> baz.
       (context "baz()")
       (file    "baz.c")
       (line    3))

    ;; NOTE:
    ;; `traverse--find' should work when we have cycles of degree > 1.

    (should (traverse--find context file line))))

