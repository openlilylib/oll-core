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

(define-module (oll-core scheme tree))

(use-modules (oop goops)(lily))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stack

; a stack implementation with methods push, pop and get
(define-class <stack> ()
  (name #:accessor name #:setter set-name! #:init-value "stack")
  (store #:accessor store #:setter set-store! #:init-value '())
  )

(define-method (push (stack <stack>) val)
  (set! (store stack) (cons val (store stack))))
(define-method (get (stack <stack>))
  (let ((st (store stack)))
    (if (> (length st) 0)
        (car st)
        #f)))
(define-method (pop (stack <stack>))
  (let ((st (store stack)))
    (if (> (length st) 0)
        (let ((ret (car st)))
          (set! (store stack) (cdr st))
          ret)
        #f)))
(define-method (display (stack <stack>) port)
  (for-each (lambda (e)
              (format #t "~A> " (name stack))(display e)(newline)) (store stack)))

(define-public (stack-create)(make <stack>))
(export push)
(export get)
(export pop)
(export store)
(export name)

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
  )

(define-method (tree-set! (tree <tree>) (path <list>) val)
  (if (= (length path) 0)
      (set! (value tree) val)
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (not (is-a? child <tree>))
            (begin (set! child (make <tree> #:key ckey))
              (hash-set! (children tree) ckey child)
              ))
        (tree-set! child cpath val)
        ))
  val)

(define-method (tree-merge! (tree <tree>) (path <list>) (proc <procedure>) val)
  (let ((ctree (tree-get-tree tree path)))
    (if (is-a? ctree <tree>)
        (set! (value ctree) (proc (value ctree) val))
        (tree-set! tree path (proc #f val)))
    ))
(define-method (tree-get-tree (tree <tree>) (path <list>))
  (if (= (length path) 0)
      tree
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (is-a? child <tree>)
            (tree-get-tree child cpath)
            #f)
        )))
(define-method (tree-get (tree <tree>) (path <list>))
  (let ((ctree (tree-get-tree tree path)))
    (if (is-a? ctree <tree>) (value ctree) #f)))
(define-method (tree-get-from-path (tree <tree>) (path <list>) skey val)
  (if (equal? skey (key tree))(set! val (value tree)))
  (let ((child (hash-ref (children tree) skey)))
    (if (is-a? child <tree>)(set! val (value child))))
  (if (= (length path) 0)
      val
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (is-a? child <tree>)
            (tree-get-from-path child cpath skey val)
            val)
        )))
(define-method (tree-get-keys (tree <tree>) (path <list>))
  (if (= (length path) 0)
      (hash-map->list (lambda (key value) key) (children tree))
      (let* ((ckey (car path))
             (cpath (cdr path))
             (child (hash-ref (children tree) ckey))
             )
        (if (is-a? child <tree>)
            (tree-get-keys child cpath)
            #f)
        )))

(define-method (tree-dispatch (tree <tree>) (path <list>) (relative <list>) def)
  (let ((val (value tree)))
    (if (= (length path) 0)
        (if val (cons '() val)(cons relative def))
        (let* ((ckey (car path))
               (cpath (cdr path))
               (child (hash-ref (children tree) ckey))
               )
          (if (or val (not (list? relative))) (set! relative '()))
          (if val (set! def (value tree)))
          (if (is-a? child <tree>)
              (tree-dispatch child cpath `(,@relative ,ckey) def)
              `((,@relative ,@path) . ,def))
          ))))

(define-method (tree-collect (tree <tree>) (path <list>) (vals <stack>))
  (let ((val (value tree)))
    (if (> (length path) 0)
        (let* ((ckey (car path))
               (cpath (cdr path))
               (child (hash-ref (children tree) ckey))
               )
          (if (is-a? child <tree>) (tree-collect child cpath vals))
          ))
    (if val (push vals val))
    (reverse (store vals))
    ))

(define (stdsort p1 p2)
  (let ((v1 (car p1))
        (v2 (car p2)))
    (cond
     ((and (number? v1) (number? v2)) (< v1 v2))
     ((and (ly:moment? v1) (ly:moment? v2)) (ly:moment<? v1 v2))
     (else (string-ci<? (format "~A" v1) (format "~A" v2)))
     )))
(define-method (tree-walk (tree <tree>) (path <list>) (callback <procedure>) . opts)
  (let ((dosort (assoc-get 'sort opts))
        (sortby (assoc-get 'sortby opts stdsort))
        (doempty (assoc-get 'empty opts)))
    (if (or doempty (value tree))
        (callback path (key tree) (value tree)))
    (for-each (lambda (p)
                (tree-walk (cdr p) `(,@path ,(car p)) callback `(sort . ,dosort) `(sortby . ,sortby) `(empty . ,doempty)))
      (if dosort (sort (hash-table->alist (children tree)) sortby)
          (hash-table->alist (children tree)) ))
    ))
(define-method (tree-walk-branch (tree <tree>) (path <list>) (callback <procedure>) . opts)
  (let ((dosort (assoc-get 'sort opts))
        (sortby (assoc-get 'sortby opts stdsort))
        (doempty (assoc-get 'empty opts))
        (ctree (tree-get-tree tree path)))
    (if (is-a? ctree <tree>)
        (tree-walk ctree path callback `(sort . ,dosort) `(sortby . ,sortby) `(empty . ,doempty)))
    ))
(define-public (tree-display tree . opt)
  (let ((path (ly:assoc-get 'path opt '() #f))
        (dosort (ly:assoc-get 'sort opt #t #f))
        (sortby (assoc-get 'sortby opt stdsort))
        (empty (ly:assoc-get 'empty opt #f #f))
        (dval (ly:assoc-get 'value opt #t #f))
        (vformat (ly:assoc-get 'vformat opt (lambda (v)(format "~A" v)) #f))
        (pformat (ly:assoc-get 'pformat opt (lambda (v)(format "~A" v)) #f))
        (pathsep (ly:assoc-get 'pathsep opt "/" #f))
        (port (ly:assoc-get 'port opt (current-output-port))))
    (tree-walk-branch tree path
      (lambda (path k val)
        (format #t "[~A] ~A" (key tree) (string-join (map pformat path) pathsep 'infix) port)
        (if (and dval val) (begin
                            (display ": " port)
                            (display (vformat val) port)
                            ))
        (newline port)
        ) `(sort . ,dosort) `(sortby . ,sortby) `(empty . ,empty) )
    ))
(define-public (tree->string tree . opt)
  (call-with-output-string
   (lambda (port)
     (apply tree-display tree (assoc-set! opt 'port port))
     )))


(define-method (display (tree <tree>) port)
  (let ((tkey (key tree)))
    (tree-display tree)))

(define-public (tree? tree)(is-a? tree <tree>))
(define-public (tree-create . key)
  (let ((k (if (> (length key) 0)(car key) 'node)))
    (make <tree> #:key k)
    ))

(export tree-set!)
(export tree-merge!)
(export tree-get-tree)
(export tree-get)
(export tree-get-from-path)
(export tree-get-keys)
(export tree-dispatch)
(export tree-collect)
(export tree-walk)
(export tree-walk-branch)
