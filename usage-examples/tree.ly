\version "2.19.38"
\include "oll-core.ily"

#(use-modules (oll-core scheme tree))

% create tree object
mytree = #(tree-create 'my-tree)

#(tree-set! mytree '(x y z) #f) % set value #f at x/y/z
#(tree-set! mytree '(a b c) #t) % set value #t at a/b/c
#(tree-set! mytree '(a b) 42) % set value 42 at a/b
#(tree-set! mytree '(global) 24) % set value 24 at global
#(display mytree) % display tree

#(display "-----------------------------------------")
#(newline)
#(display "(tree-get mytree '(a)) : ")
#(display (tree-get mytree '(a)))
#(newline)
#(display "(tree-get mytree '(a b)) : ")
#(display (tree-get mytree '(a b)))
#(newline)
#(display "(tree-get mytree '(a b c)) : ")
#(display (tree-get mytree '(a b c)))
#(newline)
#(display "(tree-get mytree '(x y)) : ")
#(display (tree-get mytree '(x y)))
#(newline)
#(display "(tree-get-node mytree '(x y)) : ")
#(display (tree-get-node mytree '(x y)))
#(newline)
#(display "(tree-get mytree '(x y z)) : ")
#(display (tree-get mytree '(x y z)))
#(newline)
#(display "(tree-get-node mytree '(x y z)) : ")
#(display (tree-get-node mytree '(x y z)))
#(newline)
#(display "-----------------------------------------")
#(newline)
% 
#(display "(tree-get-from-path mytree '(a b c d e f) 'b) : ")
#(display (tree-get-from-path mytree '(a b c d e f) 'b))
#(newline)
#(display "(tree-get-node-from-path mytree '(a b c d e f) 'b) : ")
#(display (tree-get-node-from-path mytree '(a b c d e f) 'b))
#(newline)
#(display "(tree-get-node-from-path mytree '(a b c d e f) 'not-found) : ")
#(display (tree-get-node-from-path mytree '(a b c d e f) 'not-found))
#(newline)
#(display "(tree-get-node-from-path mytree '(a b c d e f) 'x #f) : ")
#(display (tree-get-node-from-path mytree '(a b c d e f) 'x #f))
#(newline)

% return pair with extra-path and value fond within path
#(display "(tree-dispatch mytree '(a b c d e f)) : ")
#(display (tree-dispatch mytree '(a b c d e f)))
#(newline)
% collect all values found on path
#(display "(tree-collect mytree '(a b c d e f)) : ")
#(display (tree-collect mytree '(a b c d e f)))
#(newline)


