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

(define-module (oll-core internal named-alists))
(use-modules
 (lily)
 (ice-9 common-list)
 (oll-core internal alist-access)
 )

;;
;; Functions for accessing named (nested) association lists
;; typically used for option trees
;; or other global data in tree structure
;;


;; Two functions for accessing named alists and trees,
;; initially defined as module-local variables/names
(define save-list #f)
(define retrieve-list #f)

;; Closure storing an association list holding
;; an alist mapping names to lists/trees.
;; Set accessor functions to work on the closure
(let
 ((alists (list)))
 ;; Retrieve a named list/tree
 ;; return an empty list if the name isn't present
 ;; (like ly:parser-lookup)
 (set! retrieve-list
       (lambda (name)
         (or (assq-ref alists name) (list))))
 (set! save-list
       (lambda (name alst)
         (set!
          alists
          (assq-set! alists name alst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check if a given alist is already defined.
;; This is necessary as (ly:parser-lookup alst) will implicitly
;; create an empty list, which will usually result in strange
;; error conditions when a list name is misspelled.
(define (check-alst funcname alst-name key-name val)
  (if (not (retrieve-list alst-name))
      ; TODO: Change this to oll-warning (when this is transfered to oll-core)
      (ly:input-warning (*location*) "
Trying to access non-present alist '~a' with function '~a',
using key '~a' and ~a.  This will create a new alist instead,
which is probably not intended."
        alst-name funcname key-name
        (if val
            (format "value '~a'" val)
            "no value"))))

;; Wrapper function around set-in-alist
;; Is used by \setAlist and \addToAlist
(define (set-a-list funcname alst-name key-name val in-place)
  (check-alst funcname alst key-name val)
  (save-list alst-name
    (set-in-alist (retrieve-list alst-name) key-name val in-place)))

;; Wrapper function around set-in-atree,
;; to be used by \setAtree and \addAtree
(define (set-a-tree atree-name path val in-place)
  (save-list atree-name
    (set-in-atree (retrieve-list atree-name) path val in-place)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processing regular alists

;; Create a new empty list with the name <name>.
;; After this call we can rely on the existence
;; and emptyness of this.
(define-public newAlist
  (define-void-function (name)(symbol?)
    "Creates or resets <name> as an empty list."
    (save-list name (list))))

(define-public getAlist
  (define-scheme-function (name)(symbol?)
    (retrieve-list name)))

;; Set the node <key-name> to the value <val>.
;; If <key-name> is present it is replaced in-place,
;; otherwise it is appended at the end of the alist.
(define-public setAlist
  (define-void-function (alst-name key-name val)(symbol? symbol? scheme?)
    (set-a-list 'setAlist alst-name key-name val #t)))

;; Set the node <key-name> to the value <val>.
;; If <key-name> is present it is moved to the end
;; otherwise it is appended to the alist.
(define-public addToAlist
  (define-void-function (alst-name key-name val) (symbol? symbol? scheme?)
    (set-a-list 'addToAlist alst-name key-name val #f)))

;% removes one entry from association list
(define-public removeFromAlist
  (define-void-function (alst-name key-name)(symbol? symbol?)
    (check-alst 'removeFromAlist alst-name key-name #f)
    (save-list alst-name
      (assoc-remove! (retrieve-list alst-name) key-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processing nested alists,
;; called a-trees herein

;; Create a new empty a-tree with the name <name>.
;; This is merely a renamed copy of newAlist as
;; an empty a-tree is actually an empty list.
(define-public newAtree newAlist)

;; Set node <path> in a-tree <atree> to value <val>.
;; If <path> is present modify in-place, otherwise append the node.
;; Intermediate nodes are created if necessary.
(define-public setAtree
  (define-void-function (atree-name path val)(symbol? list? scheme?)
    (set-a-tree atree-name path val #t)))

;; Set node <path> in a-tree <atree> to value <val>.
;; If <path> is present it is moved to the end, otherwise appended
;; Intermediate nodes are created if necessary.
(define-public addAtree
  (define-void-function (atree-name path val)(symbol? list? scheme?)
    (set-a-tree atree-name path val #f)))

;; Retrieve a value from or a node from <path> in an a-tree <atree>.
;; The optional first argument <return-pair> controls the behaviour:
;; if #t the function returns the key-value pair, otherwise the value.
;; If <path> isn't present in the alist #f is returned.
;; However, if <return-pair> is #f there is no way to discern between
;; a literal value #f and a missing key.
(define-public getAtree
  (define-scheme-function (return-pair atree-name path)
    ((boolean?) symbol? symbol-list-or-symbol?)
    (check-alst 'getAtree atree-name path #f)
    (get-from-tree (retrieve-list atree-name) path return-pair)))

;; Remove node <path> from a-tree <atree>.
;; If <path> isn't present in <atree> it is not modified.
(define-public remAtree
  (define-void-function (atree-name path)(symbol? list?)
    (check-alst 'remAtree atree path #f)
    (save-list atree-name
      (remove-value (retrieve-list atree-name) path))))


;; This is somewhat special and doesn't really fit in that module,
;; but as it *is* dealing with alists and wouldn't warrant its own module
;; it is for now hosted here.
;; <ctx-mods> is a \with { } clause that is not used to pass along
;; *context* properties but rather general key-value pairs.
;; It returns an alist with these key-value pairs, dropping the
;; first element of each context property.
;; Returns an empty list if noe
(define-public extract-options
  (define-scheme-function (ctx-mods)((ly:context-mod?))
    (ly:warning "\"extract-options\" from module alist-access is deprecated.
Please use the equivalent context-mod->props instead.")
    (map (lambda (o)
           (cons (cadr o) (caddr o)))
      (ly:get-context-mods ctx-mods))))

