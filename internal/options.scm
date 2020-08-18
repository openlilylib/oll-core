;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;                                                                             %
; This file is part of openLilyLib,                                           %
;                      ===========                                            %
; the community library project for GNU LilyPond                              %
; (https://github.com/openlilylib/openlilylib                                 %
;              -----------                                                    %
;                                                                             %
; openLilyLib is free software: you can redistribute it and/or modify         %
; it under the terms of the GNU General Public License as published by        %
; the Free Software Foundation, either version 3 of the License, or           %
; (at your option) any later version.                                         %
;                                                                             %
; openLilyLib is distributed in the hope that it will be useful,              %
; but WITHOUT ANY WARRANTY; without even the implied warranty of              %
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
; GNU General Public License for more details.                                %
;                                                                             %
; You should have received a copy of the GNU General Public License           %
; along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
;                                                                             %
; openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
; and others.                                                                 %
;       Copyright Urs Liska, 2016                                             %
;                                                                             %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; Global option handling for openLilyLib
;
; Options are stored in one nested alist
; Managment of that alist is realized through the alist-access module.

(define-module (oll-core internal options))

(use-modules
  (ice-9 pretty-print)
  (srfi srfi-1)
  (lily)
  (oll-core internal predicates)
  (oll-core internal logging)
  (oll-core internal named-alists)
  (oll-core internal os-path)
  )

;; Create the data structure to store all the options
(newAtree 'oll-options)

(define (option-registered? path)
  "Convenience function to determine if an option is set.
   can be used to avoid warnings when trying to access unregistered options."
  (pair? (getAtree #t 'oll-options path)))


;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Interface to store and retrieve options from one global option alist.
; The following functions rely on functionality in the
; (oll-core internal alist-access) module

; A library can register options (best to be done in the __init__.ily file).
; Later end users can only set registered options, so this is kind of a syntax check.
;
; #1: option path in list or dot notation.
;     The first item should be the library name
; #2: initial value
;     If the user doesn't set the option explicitly this value is assumed
(define-public registerOption
(define-void-function (opt-path init)(symbol-list? scheme?)
   (setAtree 'oll-options opt-path init)))


; Set an option.
; Only registered options can be set this way.
; #1: Optional argument <force-set>
;     If set and the option is not registered it is initialized
;     instead of being rejected.
; #2: Provide a tree path in dotted or list notation
;     the first item of the path is the library name,
;     followed by an arbitrary path at the library's discretion
; #3: Any Scheme value
(define-public setOption
(define-void-function (force-set path val) ((boolean?) symbol-list? scheme?)
  "Set an option to a given value.
   Only registered options can be set.
   If force-set it #t an option will be registered if necessary."
   (let ((is-set (option-registered? path)))
     (if (and (not is-set) force-set)
         (begin
          (registerOption path '())
          (set! is-set #t)))
     (if is-set
         (begin
          (setAtree 'oll-options path val)
          (oll:log "Option set: ~a"
            (format "~a: ~a"
              (os-path-join-dots path) val))
          )
         ;; reject setting unknown options and report that
         (oll:warn "Not a valid option path: ~a"  (os-path-join-dots path))
         ))))


; Append a value to a list option
; #1: Optional boolean <create>
;     If set this will implicitly create an empty list to append to
; #2: <path>
;     A path within the option tree.
;     If <create> is not #t and <path> doesn't exist a warning is issued
; #3: <val>
;     Any Scheme value to be appended to the list option
(define-public appendToOption
(define-void-function (create path val)
   ((boolean?) symbol-list? scheme?)
  "Append a value to a list option.
   If force-set is #t an empty list is created if necessary.
   Otherwise the option must exist and be a list."
   (let
    ((opt
      ;; Handle non-existing option, either by creating an empty list
      ;; or by triggering the warning
      (if force-set
          (getOptionWithFallback path '())
          (getOptionWithFallback path #f))))
    (cond
     ((not opt)
      (oll:warn
       "Trying to append to non-existent option: ~a"
       (os-path-join-dots path)))
     ((not (list? opt))
      (oll:warn
       "Trying to append to non-list option: ~a"
       (os-path-join-dots path)))
     (else
      (setOption #f path (append opt (list val))))))))


; Set a child option below a given option path.
; #1: Optional boolean <force-set>
;     If set this will implicitly create a missing 'parent' node
; #2: <parent-path>
;     A path within the a-tree. Child options will be set/created below
; #3: <option>
;     The name of the option
; #4: <value>
;     The actual option value
(define-public setChildOption
(define-void-function (force-set parent-path option val)
   ((boolean?) symbol-list? symbol? scheme?)
  "Set a child option (useful for setting options in a loop).
   Only registered options can be set.
   If force-set it #t an option will be registered if necessary."
   (let ((is-set (option-registered? parent-path)))
     (if (and (not is-set) force-set)
         ;; register missing parent option
         (begin
          (registerOption parent-path '())
          (set! is-set #t)))
     (if is-set
         (setOption #t (append parent-path (list option)) val)
         (oll:warn
          "Trying to add child to non-existent option: ~a"
          (os-path-join-dots parent-path))))))

; Set multiple child options below a given option path.
; #1: Optional boolean <force-set>
;     If set this will implicitly create a missing 'parent' node
; #2: <parent-path>
;     A path within the a-tree. Child options will be set/created below
; #3: <children>
;     an alist with the children
(define-public setChildOptions
(define-void-function (force-set parent-path children)
   ((boolean?) symbol-list? alist?)
  "Set multiple child options at once, given as a flat alist.
   Only registered options can be set.
   If force-set it #t an option will be registered if necessary."
   (let ((is-set (option-registered? parent-path)))
     (if (and (not is-set) force-set)
         ;; register missing parent option
         (begin
          (registerOption parent-path '())
          (set! is-set #t)))
     (if is-set
         (for-each
          (lambda (opt)
            (setChildOption force-set parent-path (car opt) (cdr opt)))
          children)
         (oll:warn
          "Trying to add children to non-existent option: ~a"
          (os-path-join-dots parent-path))))))


;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Retrieve options from registered option tree.

; Retrieve an option
; Provide a tree path in dotted or list notation
; Retrieving a non-existing option path issues a warning and returns #f
(define-public getOption
(define-scheme-function (path) (symbol-list?)
  "Retrieve value of an existing option."
   (let ((option (getAtree #t 'oll-options path)))
     (if option
         ;; getAtree has returned a pair => option is set
         (cdr option)
         ;; getAtree has returned #f
         (begin
          (oll:warn
           "Trying to access non-existent option: ~a" (os-path-join-dots path))
          #f)))))

; Same as \getOption, but retrieving non-existing options returns
; the fallback argument and does not raise a warning.
(define-public getOptionWithFallback
(define-scheme-function (path fallback)
   (list? scheme?)
  "Retrive value of an option.
   Use fallback if option doesn't exist."
   (let ((option (getAtree #t 'oll-options path)))
     (if option
         (cdr option)
         fallback))))

; Retrieve a child option from option <path>.
; If either the 'parent' path or the child option are not present
; a warning is issued and #f returned
(define-public getChildOption
(define-scheme-function (path child)
   (symbol-list? symbol?)
  "Retrieve a child value of an existing option."
   (symbol-list? symbol?)
   (getOption (append path (list child)))))

; Same as \getChildOption, but retrieving non-existing options
; returns the fallback argument and doesn't issue a warning.
; This is useful for dynamic options where the user should be
; allowed to provide arbitrary values.
; An example is the setting of arbitrary annotation properties.
(define-public getChildOptionWithFallback
(define-scheme-function (path child fallback)
   (symbol-list? symbol? scheme?)
  "Retrive value of a child option.
   Use fallback if option doesn't exist."
   (getOptionWithFallback (append path (list child)) fallback)))




;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Macro support to define LilyPond-style music/void/scheme-functions
; with validated options.
; A set of option rules can define known options with types and defaults

(define (validate-props rules props)
  "Check a list of properties and return a possibly updated list.
    - Handle unknown options (remove or not depending on 'strict' or 'flexible' ruleset)
    - type check
    - Handle missing properties. If a default is available use that.
    It is *assumed* without checking that rules satisfies the prop-rules? predicate,
    which can be justified because the function should not be called from documents."
  (let*
   ((strict (eq? (car rules) 'strict))
    (rules (cdr rules))
    (rules
     (map (lambda (rule)
            (let*
             ((rule (if (symbol? rule) (list rule) rule))
              (optional (eq? (first rule) '?))
              (rule (if optional (cdr rule) rule))
              (k (first rule))
              (pred
               (if (= (length rule) 1)
                   scheme?
                   (second rule)))
              (default
               (if (= 3 (length rule))
                   (third rule)
                   '())))
             (list k pred default optional)))
       rules))
    (missing
     (delete '()
       (map (lambda (r)
              (let*
               ((k (car r))
                (default (third r))
                (optional (fourth r))
                (prop (assoc-get k props)))
               (cond
                (prop '())
                ((not (null? default)) (cons k default))
                (optional '())
                (else
                 (begin
                  (ly:input-warning (*location*)
                    "Missing mandatory property \"~a\"." k)
                  '())))))
         rules)))
    (props
     (delete '()
       (map (lambda (p)
              (let*
               ((k (car p)) (v (cdr p)) (rule (assoc-ref rules k)))
               (cond
                ;; unknown option
                ((not rule)
                 (if strict
                     (begin
                      (ly:input-warning (*location*)
                        "Unknown property \"~a\"." k)
                      '())
                     p))
                ;; type check successful
                (((car rule) v) p)
                (else
                 (begin
                  (ly:input-warning (*location*)
                    "Type check failed for property \"~a\".\nExpected: ~a, given: ~a"
                    k (car rule) v)
                  '())))))
         props)))
    )
   (append props missing)))


; Convert a ly:context-mod? argument to a properties alist
; Arguments:
; - rules (optional): a prop-rules? property definition list
; - mod: the context-mod
(define context-mod->props
  (lambda (req . rest)
    ;unpack mod and rules from the arguments
    (let ((mod
           (cond
            ((ly:context-mod? req) req)
            ((and (= 1 (length rest)) (ly:context-mod? (first rest))) (first rest))
            (else
             (begin
              (ly:error "context-mod->props didn't receive a context-mod")
              (ly:make-context-mod)))))
          (rules (if (prop-rules? req)
                     req
                     '(flexible))))
      (let
       ((props
         (map
          (lambda (prop)
            (cons (cadr prop) (caddr prop)))
          (ly:get-context-mods mod))))
       (if rules
           (validate-props rules props)
           props)))))


(define (make-opts-function-declaration proc vars preds rules optional . body)
  "Return the declaration of a function with the given arguments."
  (let* ((vars (append '(opts) vars))
         (preds
          (if optional
              (begin
               (cond
                ((every list? preds)
                 (ly:warning "defining a with-options function without mandatory arguments."))
                ((list? (first preds))
                 (ly:warning "defining a with-options function where the first argument is optional.")))
               (append '((ly:context-mod? (ly:make-context-mod))) preds))
              (append '(ly:context-mod?) preds)))
         (rules
          (if (empty-parens? rules)
              (quote '(flexible))
              rules)))
    `(,proc ,vars ,preds
       (let* ((rules ,rules)
              (props (context-mod->props rules opts))
              (property (lambda (name) (assq-ref props name)))
              )
         . ,body))))

; Macro to facilitate definition of functions with options.
; Begin the function definition with 'with-options and give the ruleset
; before the body of the function.
; Example:
; (with-options define-void-function () ()
;   `(strict
;      (msg ,string?)
;      (? author ,string? "Anonymous"))
;   (pretty-print props))
; Warning: The body of the function can't be empty.
(define-macro (with-options proc vars preds rules . body)
   (let ((optional #t))
     (apply make-opts-function-declaration `(,proc ,vars ,preds ,rules ,optional . ,body))))

(define-macro (with-opts . rest)
   `(with-options . ,rest))

(define-macro (with-required-options proc vars preds rules . body)
   (let ((optional #f))
     (apply make-opts-function-declaration `(,proc ,vars ,preds ,rules ,optional . ,body))))

(define-macro (with-req-opts . rest)
   `(with-required-options . ,rest))





;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%% Display all currently registered options

(define-public displayOptions
(define-void-function (root)((symbol-list? '()))
   (display "\n\nopenLilyLib: Currently registered options:\n=====\n")
   (let ((use-root (if (null? root)
                       (getAlist 'oll-options)
                       (getOption root))))
     (pretty-print
      ;oll-options
      use-root #:display? #t))))

; Display the metadata of a package
(define-public describePackage
(define-void-function (name)(symbol?)
   (if (member name (getOption '(loaded-packages)))
       (begin
        (format #t "\n\nopenLilyLib: Metadata of package '~a':\n=====\n" name)
        (pretty-print (getOptionWithFallback `(,name meta) "None available")))
       (format #t "\n\nopenLilyLib: Package '~a' is not loaded.\n\n" name))))


; Display the options of a package or module (if available)
; Package options will also include options of loaded modules
(define-public displayModuleOptions
(define-void-function (path)(symbol-list?)
   (let*
    ((package (car path))
     (module (cdr path)))
    (if (null? module)
        ;; display *package* options
        (if (member package (getOption '(loaded-packages)))
            ;; package is loaded
            (begin
             (format #t "\n\nopenLilyLib: Options of package '~a':\n=====\n" package)
             (let
              ((options (filter
                         (lambda (o)
                           (not (member (car o) '(root meta))))
                         (getOptionWithFallback (list package) '()))))
              (if (not (null? options))
                  ;; there are package options
                  (pretty-print options)
                  ;; no package options available
                  (format #t "None available.\n\n"))))
            ;; package is not loaded
            (format #t "\n\nopenLilyLib: Can't show options, package '~a' is not loaded.\n\n" package))
        ;; display *module* options
        (if (member module (getOptionWithFallback `(loaded-modules ,package) '()))
            ;; module is loaded
            (begin
             (format #t "\n\nopenLilyLib: Options of module '~a':\n=====\n" (os-path-join-dots path))
             (pretty-print (getOptionWithFallback path "None available")))
            ;; module is not loaded
            (format #t "Can't show options, module '~a' is not loaded.\n\n" path))))))


; Convenience functions to add some type checking
; to the bundling of package option templates.
; To be used by package creators only
; TODO: Remove, this really seems not to have been used at all
; (at least it can't be found in any OLL package)
(define (make-mandatory-props props)
  (if (oll-mand-props? props)
      props
      (begin
       (oll:warn "Wrong argument type: oll-mand-props? expected")
       '())))

(define (make-accepted-props props)
  (if (oll-accepted-props? props)
      props
      (begin
       (oll:warn "Wrong argument type: oll-accepted-props? expected")
       '())))


(export
 context-mod->props
 option-registered?
 validate-props
 with-options
 with-opts ; deprecated
 with-required-options
 with-req-opts ; deprecated
 )
