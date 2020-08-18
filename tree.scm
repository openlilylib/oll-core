;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%                                                                             %
;% This file is part of openLilyLib,                                           %
;%                      ===========                                            %
;% the community library project for GNU LilyPond                              %
;% (https://github.com/openlilylib)                                            %
;%              -----------                                                    %
;%                                                                             %
;% Library: oll-core                                                           %
;%          ========                                                           %
;%                                                                             %
;% openLilyLib is free software: you can redistribute it and/or modify         %
;% it under the terms of the GNU General Public License as published by        %
;% the Free Software Foundation, either version 3 of the License, or           %
;% (at your option) any later version.                                         %
;%                                                                             %
;% openLilyLib is distributed in the hope that it will be useful,              %
;% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
;% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
;% GNU General Public License for more details.                                %
;%                                                                             %
;% You should have received a copy of the GNU General Public License           %
;% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
;%                                                                             %
;% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
;% and others.                                                                 %
;%       Copyright Jan-Peter Voigt, Urs Liska, 2016                            %
;%                                                                             %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define-module (oll-core tree))

(use-modules
 (oop goops)
 (lily)
 (srfi srfi-1)
 (oll-core stack))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tree

; a tree implementation
; every tree-node has a hashtable of children and a value
; main methods are:
; tree-set! <tree> path-list val: set a value in the tree
; tree-get <tree> path-list: get a value from the tree or #f if not present
(define-class <tree> ()
  (children #:accessor children #:init-thunk make-hash-table)
  (key #:accessor key #:init-keyword #:key #:init-value 'node)
  (value #:accessor value #:setter set-value! #:init-value #f)
  (has-value #:accessor has-value #:setter has-value! #:init-value #f)
  (type #:accessor type #:setter set-type! #:init-value #f)
  )

; set value at path
; if the node at path has a type first check against that
; if the path doesn't exist yet intermediate nodes are created implicitly
(define-method (tree-set! (tree <tree>) (path <list>) val)
  (tree-set! #t tree path val))

; set value at path
; if create is #t missing intermediate nodes are created implicitly
; if the node at path has a type first check against that
(define-method (tree-set! (create <boolean>) (tree <tree>) (path <list>) val)
  (if (= (length path) 0)
      ;; end of path reached: set value
      (let ((pred? (type tree)))
        (if pred?
            ;; if tree has a type defined check value against it before setting
            (if (pred? val)
                (begin
                 (set-value! tree val)
                 (has-value! tree #t))
                (begin
                 (ly:input-warning (*location*)
                   (format "TODO: Format warning about typecheck error in tree-set!
Expected ~a, got ~a" (procedure-name pred?) val))
                 (set! val #f)))
            ;; if no typecheck is set simply set the value
            (begin
             (set-value! tree val)
             (has-value! tree #t)
             )))
      ;; determine child
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey)))
        (if (not (tree? child))
            ;; create child node if option is set
            (if create
                (begin
                 (set! child (make <tree> #:key ckey))
                 (hash-set! (children tree) ckey child))))
        (if (tree? child)
            ;; recursively walk path
            (tree-set! create child cpath val)
            (ly:input-warning (*location*)
              (format "TODO: Format missing path warning in tree-set!
Path: ~a" path)))))
  val)

; unset value at path
; set value = #f and has-value = #f at path
; if the path doesn't exist, the tree is left unchanged
(define-method (tree-unset! (tree <tree>) (path <list>))
  (let ((val #f))
    (if (= (length path) 0)
        ;; end of path reached: set value
        (begin
         (if (has-value tree) (set! val (value tree)))
         (set-value! tree #f)
         (has-value! tree #f)
         )
        ;; determine child
        (let* ((ckey (car path))
               (cpath (cdr path))
               (child (hash-ref (children tree) ckey)))
          (if (tree? child)
              ;; recursively walk path
              (tree-unset! child cpath))
          ))
    val))

(define-method (tree-set-type! (tree <tree>) (path <list>)(predicate <procedure>))
  (if (= (length path) 0)
      ;; end of path reached: register type
      (begin
       (set-type! tree predicate)
       ; TODO: What to do if there already is a value?
       ; probably: check type and issue an oll-warning
       )
      ;; determine child
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey)))
        (if (not (tree? child))
            ;; create child node if not present
            (begin (set! child (make <tree> #:key ckey))
              (hash-set! (children tree) ckey child)))
        ;; recursively walk path
        (tree-set-type! child cpath predicate))
      ))

; merge value at path into tree
(define-method (tree-merge! (tree <tree>) (path <list>) (proc <procedure>) val)
  (let ((ctree (tree-get-tree tree path)))
    (if (tree? ctree)
        (set! (value ctree)
              (if (has-value ctree) (proc (value ctree) val) val))
        (tree-set! tree path (proc #f val)))
    ))

; merge values of tree2 into tree1
(define-method (tree-merge! (tree1 <tree>) (proc <procedure>) (tree2 <tree>))
  (tree-walk tree2 '()
    (lambda (path nkey value)
      (tree-merge! tree1 path proc value)
      )))

; get value at path
; returns #f if path is not present or if its value is #f
; to discern use tree-get-node
(define-method (tree-get (tree <tree>) (path <list>))
  (let ((ctree (tree-get-tree tree path)))
    (if (tree? ctree) (value ctree) #f)))

; get the node at path
; returns '(key . value) pair - or #f if path is not present
; to be used if #f values are to be expected.
(define-method (tree-get-node (tree <tree>) (path <list>))
  (let ((ctree (tree-get-tree tree path)))
    (if (and (tree? ctree) (has-value ctree))
        (cons (last path) (value ctree)) #f)))

; return the sub-tree with path as its root
; returns #f if path is not in the tree
(define-method (tree-get-tree (tree <tree>) (path <list>))
  (if (= (length path) 0)
      ;; end of path reached: return sub-tree
      tree
      ;; determine child node
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey)))
        (if (tree? child)
            ;; recurse through path
            (tree-get-tree child cpath)
            ;; return #f immediately if node is not present
            #f))))

; get value with key <skey> from path
; if skey=global and path=music.momnt.brass.trumpet
; it looks for global, music.global, music.momnt.global, music.momnt.brass.global
; and music.momnt.brass.trumpet.global and returns the last value found
(define-method (tree-get-from-path (tree <tree>) (path <list>) skey)
  (tree-get-from-path tree path skey #f))
(define-method (tree-get-from-path (tree <tree>) (path <list>) skey val)
  (if (equal? skey (key tree))(set! val (value tree)))
  (let ((child (hash-ref (children tree) skey)))
    (if (tree? child)(set! val (value child))))
  (if (= (length path) 0)
      val
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (tree? child)
            (tree-get-from-path child cpath skey val)
            val)
        )))
; get key-value-pair with key <skey> from path
; if skey=global and path=music.momnt.brass.trumpet
; it looks for global, music.global, music.momnt.global, music.momnt.brass.global
; and music.momnt.brass.trumpet.global and returns the last value found
; TODO predicate?
(define-method (tree-get-node-from-path (tree <tree>) (path <list>) skey)
  (tree-get-node-from-path tree path skey #f))
(define-method (tree-get-node-from-path (tree <tree>) (path <list>) skey val)
  (if (and (equal? skey (key tree))(has-value tree))
      (set! val (cons skey (value tree))))
  (let ((child (hash-ref (children tree) skey)))
    (if (and (tree? child)(has-value child))
        (set! val (cons skey (value child)))))
  (if (= (length path) 0)
      val
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (tree? child)
            (tree-get-node-from-path child cpath skey val)
            val)
        )))

; return all sub-keys/nodes at path
(define-method (tree-get-keys (tree <tree>) (path <list>))
  (if (= (length path) 0)
      (hash-map->list (lambda (key value) key) (children tree))
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (tree? child)
            (tree-get-keys child cpath)
            #f)
        )))

; return pair with relative path to value
; if X is stored at 'a/b/c'
; (tree-dispatch tree '(a b c d e))
; returns: '((d e) . X)
(define-method (tree-dispatch (tree <tree>) (path <list>))
  (tree-dispatch tree path '() #f))
; def = default value
(define-method (tree-dispatch (tree <tree>) (path <list>) def)
  (tree-dispatch tree path '() def))
; relative = relative path to tree
(define-method (tree-dispatch (tree <tree>) (path <list>) (relative <list>) def)
  (let ((val (value tree)))
    (if (= (length path) 0)
        (if (has-value tree) (cons '() val)(cons relative def)) ; return last element
        (let* ((ckey (car path)) ; look deeper
               (cpath (cdr path))
               (child (hash-ref (children tree) ckey))
               )
          (if (or (has-value tree) (not (list? relative))) (set! relative '()))
          (if (has-value tree) (set! def (value tree)))
          (if (tree? child)
              (tree-dispatch child cpath `(,@relative ,ckey) def)
              `((,@relative ,@path) . ,def))
          ))))

; collect all values on path with optional predicate
(define-method (tree-collect (tree <tree>) (path <list>))
  (tree-collect tree path (stack-create) (lambda (v) #t)))
(define-method (tree-collect (tree <tree>) (path <list>) (pred? <procedure>))
  (tree-collect tree path (stack-create) pred?))
(define-method (tree-collect (tree <tree>) (path <list>) (vals (@@ (oll-core stack) <stack>))) ; there is also a <stack> class in (oop goops)
  (tree-collect tree path vals (lambda (v) #t)))
(define-method (tree-collect (tree <tree>) (path <list>) (vals (@@ (oll-core stack) <stack>)) (pred? <procedure>))
  (let ((val (value tree)))
    (if (> (length path) 0)
        (let* ((ckey (car path))
               (cpath (cdr path))
               (child (hash-ref (children tree) ckey))
               )
          (if (tree? child) (tree-collect child cpath vals pred?))
          ))
    (if (and (has-value tree)(pred? val)) (push vals val))
    (reverse (store vals))
    ))

; standard sort-function
(define (stdsort p1 p2)
  (let ((v1 (car p1))
        (v2 (car p2)))
    (cond
     ((and (number? v1) (number? v2)) (< v1 v2))
     ((and (ly:moment? v1) (ly:moment? v2)) (ly:moment<? v1 v2))
     (else (string-ci<? (format "~A" v1) (format "~A" v2)))
     )))

; walk the tree and call callback for every node
(define-method (tree-walk (tree <tree>) (path <list>) (callback <procedure>) . opts)
  (let ((dosort (assoc-get 'sort opts #f))
        (sortby (assoc-get 'sortby opts stdsort))
        (doempty (assoc-get 'empty opts #f)))
    (if (or doempty (has-value tree))
        (callback path (key tree) (value tree)))
    (for-each (lambda (p)
                (tree-walk (cdr p) `(,@path ,(car p)) callback `(sort . ,dosort) `(sortby . ,sortby) `(empty . ,doempty)))
      (if dosort (sort (hash-table->alist (children tree)) sortby)
          (hash-table->alist (children tree)) ))
    ))

; walk the tree and call callback for every node in sub-tree at path
(define-method (tree-walk-branch (tree <tree>) (path <list>) (callback <procedure>) . opts)
  (let ((dosort (assoc-get 'sort opts))
        (sortby (assoc-get 'sortby opts stdsort))
        (doempty (assoc-get 'empty opts))
        (ctree (tree-get-tree tree path)))
    (if (tree? ctree)
        (tree-walk ctree path callback `(sort . ,dosort) `(sortby . ,sortby) `(empty . ,doempty)))
    ))

; display tree
(define-public (tree-display tree . opt)
  (let ((path (ly:assoc-get 'path opt '() #f)) ; path to display
         (dosort (ly:assoc-get 'sort opt #t #f)) ; wether to sort by key
         (sortby (assoc-get 'sortby opt stdsort)) ; sort-function
         (empty (ly:assoc-get 'empty opt #f #f)) ; display empty nodes
         (dval (ly:assoc-get 'value opt #t #f)) ; display value
         (vformat (ly:assoc-get 'vformat opt (lambda (v)(format "~A" v)) #f)) ; format value
         (pformat (ly:assoc-get 'pformat opt (lambda (v)(format "~A" v)) #f)) ; format path
         (pathsep (ly:assoc-get 'pathsep opt "/" #f)) ; separator for path
         (port (ly:assoc-get 'port opt (current-output-port)))) ; output-port
    (tree-walk-branch tree path
      (lambda (path k val)
        (format port "[~A] ~A" (key tree) (string-join (map pformat path) pathsep 'infix))
        (if dval
            (begin
             (display ": " port)
             (display (vformat val) port)
             ))
        (newline port)
        ) `(sort . ,dosort) `(sortby . ,sortby) `(empty . ,empty) )
    ))

; display tree to string
(define-public (tree->string tree . opt)
  (call-with-output-string
   (lambda (port)
     (apply tree-display tree (assoc-set! opt 'port port))
     )))

; display tree
(define-method (display (tree <tree>) port)
  (let ((tkey (key tree)))
    (tree-display tree `(port . ,port))))

; tree predicate
(define-public (tree? tree)(is-a? tree <tree>))
; create tree
(define-public (tree-create . key)
  (let ((k (if (> (length key) 0)(car key) 'node)))
    (make <tree> #:key k)
    ))

; export methods
(export tree-set!)
(export tree-unset!)
(export tree-set-type!)
(export tree-merge!)
(export tree-get-tree)
(export tree-get)
(export tree-get-node)
(export tree-get-from-path)
(export tree-get-node-from-path)
(export tree-get-keys)
(export tree-dispatch)
(export tree-collect)
(export tree-walk)
(export tree-walk-branch)
