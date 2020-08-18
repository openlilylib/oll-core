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
 (lily)
 (ice-9 common-list))

;;
;; Functions for easier and robust access to nested association lists.
;; Typically useful for implementing option trees.
;;


;; Set key <key-name> in alist <alst> to value <val>
;; If <in-place> is #t the key is replaced in-place if already present.
;; Otherwise (<in-place> is #f and/or key is not set) it is appended.
(define-public (set-in-alist alst key-name val in-place)
  (let* ((process-alist
          (if in-place
              alst
              (assoc-remove! alst key-name)))
         (where (assoc key-name process-alist)))
    (if where
        (begin (set-cdr! where val) alst)
        (append alst (list (cons key-name val))))))

;; Retrieve entry <key-name> from alist <alst>.
;; If <return-pair> is #t then the function behaves like 'assoc',
;; that is it returns the key-value pair or #f.
;; Otherwise it returns only the value or #f without the chance of
;; discerning between a non-present key or a literal value #f.
(define-public (get-from-alist alst key-name return-pair)
  (let ((intermediate (assoc key-name alst)))
    (if return-pair
        intermediate
        (if (pair? intermediate)
            (cdr intermediate)
            #f))))

;; Set <path> in alist <tree> to value <val>.
;; <path> is a symbol list, with the last element being the actual key.
;; If any node or the final key is not present it is created implicitly.
;; If <in-place> is #t the key is modified in place if already present,
;; otherwise it will be appended.
;; Intermediate nodes are always updated in place if the already exist.
(define-public (set-in-atree tree path val in-place)
  (let ((key-name (car path)))
    (if (not (list? tree))
        (begin
         (ly:input-warning (*location*) "Not a list. Deleting '~A'" tree)
         (set! tree '())))
    (cond ((> (length path) 1)
           (let ((subtree (assoc-get key-name tree '())))
             (set-in-alist
              tree
              key-name
              ;; Intermediate nodes are always updated in-place
              (set-in-atree subtree (cdr path) val #t)
              in-place)))
      (else
       (set-in-alist tree key-name val in-place)))))

;; Recursively walk the nested alist <tree> over the symbol-list <path>
;; and return the value for the last leaf in <path> or #f if the chain
;; is broken at any point.
(define-public (get-from-tree tree path return-pair)
  (let ((key-name (car path)))
    (if (> (length path) 1)
        (let ((subtree (assoc-get key-name tree #f)))
          (if (list? subtree)
              (get-from-tree subtree (cdr path) return-pair)
              #f))
        (get-from-alist tree (car path) return-pair))))

;; Takes the alist <tree> and removes the node <path>,
;; returns a new list.
(define-public (remove-value tree path)
  (let* ((key-name (car path))
         (subpath (cdr path))
         (subtree (assoc-get key-name tree '())))
    (cond
     ((> (length subpath) 1)
      (set-in-alist tree key-name (remove-value subtree (cdr path)) #t))
     (else
      (set-in-alist tree key-name (assoc-remove! subtree (car subpath)) #t)))))


