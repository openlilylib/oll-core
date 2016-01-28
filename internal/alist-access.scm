;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%                                                                             %
;% This file is part of openLilyLib,                                           %
;%                      ===========                                            %
;% the community library project for GNU LilyPond                              %
;% (https://github.com/openlilylib/openlilylib                                 %
;%              -----------                                                    %
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
;%       Copyright Urs Liska, 2015                                             %
;%                                                                             %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define-module (oll-core internal alist-access))
(use-modules
 (lily))


;;;;;;;;;;;;;;;;;;;
;; Helper functions

;; Check if a given alist is already defined.
;; This is necessary as (ly:parser-lookup alst) will implicitly
;; create an empty list, which will usually result in strange
;; error conditions when a list name is misspelled.
(define (check-alst funcname alst key-name val)
  (if (not (defined? alst))
      ; TODO: Change this to oll-warning (when this is transfered to oll-core)
      (ly:input-warning (*location*) "
Trying to access non-present alist '~a' with function '~a',
using key '~a' and ~a.  This will create a new alist instead,
which is probably not intended."
      alst funcname key-name
      (if val
          (format "value '~a'" val)
          "no value"))))

;; Remove node <key-name> from a-list <alst> when it is present.
 ; QUESTION: This takes some extra work to preserve nodes
 ; which are not pairs? Is that appropriate?
(define (rem-from-alist alst key-name)
  (filter
   (lambda (node)
     (or (and (pair? node)
              (not (equal? (car node) key-name)))))
   alst))

;; Recursively walk the nested alist <tree> over the symbol-list <path>
;; and return the value for the last leaf in <path> or #f if the chain
;; is broken at any point.
(define (get-from-subtree tree path)
  (let ((key-name (car path)))
    (cond
     ((> (length path) 1)
      (let ((subtree (assoc-get key-name tree #f)))
        (if (list? subtree)
            (get-from-subtree subtree (cdr path))
            #f)))
     ((= (length path) 1)
      (assoc-get (car path) tree #f))
     (else #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processing regular alists

;; Create a new empty list with the name <name>.
;; After this call we can rely on the existence
;; and emptyness of this.
(define-public newAlist
   (define-void-function (name)(symbol?)
     "Creates or resets <name> as an empty list."
         (ly:parser-define! name (list))))

;; Set the node <key-name> to the value <val>.
;; If <key-name> is present it is replaced in-place,
;; otherwise it is appended at the end of the alist.
(define-public setAlist
  (define-void-function (alst key-name val)(symbol? symbol? scheme?)
    (check-alst 'setAlist alst key-name val)
    (ly:parser-define! alst
      (let ((result-list (ly:parser-lookup alst)))
        (if (assq-ref result-list key-name)
            ;; node is present, replace in-place
            (map (lambda (node)
                   (if (and (pair? node) (equal? (car node) key-name))
                       (cons name val)
                       node))
              result-list)
            ;; else simply append to the end
            (append result-list (list (cons key-name val))))))))

;; Set the node <key-name> to the value <val>.
;; If <key-name> is present it is moved to the end
;; otherwise it is appended to the alist.
(define-public addToAlist
  (define-void-function (alst key-name val) (symbol? symbol? scheme?)
    (check-alst 'addToAlist alst key-name val)
    (ly:parser-define! alst
      (append
       (rem-from-alist (ly:parser-lookup alst) key-name)
       (list (cons key-name val))))))

;% removes one entry from association list
(define-public removeFromAlist
  (define-void-function (alst key-name)(symbol? symbol?)
    (check-alst 'removeFromAlist alst key-name #f)
    (ly:parser-define! alst
      (rem-from-alist (ly:parser-lookup alst) key-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processing nested alists,
;; called a-trees herein

;; Create a new empty a-tree with the name <name>.
;; This is merely a renamed copy of newAlist as
;; an empty a-tree is actually an empty list.
(define-public newAtree newAlist)

;; Retrieve a value from path <path> in an a-tree <atree>.
;; If the key (last element) or any element in <path> is not present
;; #f is returned.
;; NOTE: There's an ambiguity between a non-present key and a key
;; with the explicit value #f
(define-public getAtree
  (define-scheme-function (atree path)(symbol? symbol-list-or-symbol?)
   (let ((tree (ly:parser-lookup atree)))
     (check-alst 'getAtree atree path #f)
     (if (list? tree)
         (get-from-subtree tree path)
         (begin
          (ly:input-warning (*location*) "~A is not list (~A)" atree tree)
          #f)))))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;% Old functions, to be reviewed
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;% add an entry to a nested a-list
(define (add-a-tree name sympath val assoc-set-append)
   (if (not (symbol? name)) (set! name (string->symbol (object->string name))))
   (let ((opts (ly:parser-lookup name)))
    (define (setval ol op)
      (let ((sym (car op))
            (ol (if (list? ol) ol (begin
                                   (oll:warn location "deleting '~A'" ol)
                                   '()))))
        (if (> (length op) 1)
            (let ((al (assoc-get sym ol '())))
              (if (not (list? al))
                  (begin
                   (oll:warn location "deleting '~A' = '~A'" sym al)
                   (set! al '())
                   ))
              (assoc-set-append ol sym (setval al (cdr op)))
              )
            (let ((ov (assoc-get sym ol #f)))
              ;(if ov (oll:warn location "deleting '~A'" ov))
              (assoc-set-append ol sym val)
              )
            )))
    (set! opts (setval opts sympath)) (ly:parser-define! name opts)
     ))

;% remove an entry from a nested a-list
(define (rem-a-tree name sympath)
  (if (not (symbol? name)) (set! name (string->symbol (object->string name))))
  (let ((opts (ly:parser-lookup name)))
    (define (remval ol op)
      (let ((sym (car op)))
        (if (> (length op) 1)
            (let ((al (assoc-get sym ol '())))
              (set! al (remval al (cdr op)))
              (if (> (length al) 0)
                  (map (lambda (p) (if (and (pair? p)(equal? (car p) sym)) (cons sym al) p)) ol)
                  (filter (lambda (p) (not (and (pair? p)(equal? (car p) sym)))) ol))
              )
            (filter (lambda (p) (not (and (pair? p)(equal? (car p) sym)))) ol)
            )
        ))
    (set! opts (remval opts sympath))
    (ly:parser-define! name opts)
    ))

;% add an entry to nested a-list at the end
(define-public addatree
  (define-void-function (name sympath val)(symbol? list? scheme?)
    (add-a-tree name sympath val
      (lambda (l sym val)
        (append (filter (lambda (p) (not (and (pair? p)(equal? (car p) sym)))) l)
          (list (cons sym val)))))))
;% set an entry in nested a-list in place
(define-public setatree
  (define-void-function (name sympath val)(symbol? list? scheme?)
    (add-a-tree name sympath val
      (lambda (l sym val) (assoc-set! l sym val)))))
;% remove an entry from nested a-list
(define-public rematree
  (define-void-function (name sympath)(symbol? list?)
    (rem-a-tree name sympath)))
