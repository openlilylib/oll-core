%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of openLilyLib,                                           %
%                      ===========                                            %
% the community library project for GNU LilyPond                              %
% (https://github.com/openlilylib/openlilylib                                 %
%              -----------                                                    %
%                                                                             %
% openLilyLib is free software: you can redistribute it and/or modify         %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% openLilyLib is distributed in the hope that it will be useful,              %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU General Public License for more details.                                %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
%                                                                             %
% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
% and others.                                                                 %
%       Copyright Urs Liska, 2016                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Global option handling for openLilyLib
%
% Options are stored in one nested alist
% Managment of that alist is realized through the a-list-access.ily files.

#(use-modules (ice-9 pretty-print))


% predicate for check-props
% which accepts an a-list or a context-mod
#(define (al-or-props? obj)
   (if (or (ly:context-mod? obj)
           (and (list? obj)
                (every pair? obj)))
       #t #f))

% temporary predicate, as this seems just too general ...
#(define (alist? obj)
   (if (and (list? obj)
            (every pair? obj))
       #t #f))

% Predicate for a mandatory option:
% a three-element list consisting of
% - name (symbol?)
% - element predicate (procedure?)
% - default value (tested against predicate)
#(define (oll-mand-prop? obj)
   (if (and (list? obj)
            (= (length obj) 3)
            (symbol? (first obj))
            (procedure? (second obj))
            ((second obj) (third obj)))
       #t #f))

% Predicate for mandatory options:
% list of oll-mand-prop? items
#(define (oll-mand-props? obj)
   (if (and (list? obj)
            (every oll-mand-prop? obj))
       #t #f))

% Precidate for an accepted options:
% pair of property name and predicate
#(define (oll-accepted-prop? obj)
   (if (and (pair? obj)
            (symbol? (car obj))
            (procedure? (cdr obj)))
       #t #f))

% Predicate for accepted options:
% list of oll-accepted-prop? items
#(define (oll-accepted-props? obj)
   (if (and (list? obj)
            (every oll-accepted-prop? obj))
       #t #f))

% Convenience functions to add some type checking
% to the bundling of package option templates.
% To be used by package creators only
#(define (make-mandatory-props props)
   (if (oll-mand-props? props)
       props
       (begin
        (oll:warn "Wrong argument type: oll-mand-props? expected")
        '())))

#(define (make-accepted-props props)
   (if (oll-accepted-props? props)
       props
       (begin
        (oll:warn "Wrong argument type: oll-accepted-props? expected")
        '())))


% Convenience function, retrieves a property alist from a context-mod object.
% mod has to satisfy the ly:context-mod? predicate,
% returns an alist with all key-value pairs set.
#(define-public (context-mod->props mod)
   (map
    (lambda (prop)
      (cons (cadr prop) (caddr prop)))
    (ly:get-context-mods mod)))


% check a property alist (or a ly:context-mod?) for adherence to
% a given set of rules/templates
% Returns a possibly modified version of the given property list
% - force-mand (optional)
%   if #t mandatory options are added with default values
% - mand
%   list of mandatory options with name, predicate and default
% - accepted
%   list of accepted/known options with name and predicate
#(define check-props
   (define-scheme-function (force-mand mand accepted props)
     ((boolean?) oll-mand-props? oll-accepted-props? al-or-props?)
     (let*
      ((props (if (ly:context-mod? props) (context-mod->props props) props))

       ;; determine missing properties
       ;; if we're not interested in them simply create an empty list
       (missing-props
        (if force-mand
            (filter
             (lambda (m)
               (not (assq-ref props m)))
             (map car mand))
            '())))

      ;; check all given properties
      (for-each
       (lambda (prop)
         (let ((accepted-prop (assq (car prop) accepted))
               (mand-prop (assq (car prop) mand)))
           (cond
            (accepted-prop
             ;; passed option is in list of known options
             (if (not ((cdr accepted-prop) (cdr prop)))
                 (begin
                  ;; warn about wrong type for known option, discard
                  (oll:warn "Wrong type for known option ~a. Discarded\n~a" (car prop) (cdr prop))
                  (set! props (assq-remove! props (car prop)))
                  )))
            (mand-prop
             ;; check type of present mandatory option
             (if (not ((second mand-prop) (cdr prop)))
                 (begin
                  (oll:warn "Wrong type for mandatory option ~a:\n~a. Use default ~a"
                    (first mand-prop) (cdr prop) (third mand-prop))
                  (set! props (assq-set! props (first prop) (third mand-prop)))))
             )
            (else
             (begin
              ;; warn about unknown option, discard
              (oll:warn "Unknown option ~a: ~a. Discarded" (car prop) (cdr prop))
              (set! props (assq-remove! props (car prop)))
              )))))
       props)

      ;; return props, appended with missing mandatory options
      ;; (if force-mand is #f then missing-props will be empty)
      (append props
        (map
         (lambda (p)
           (cons p (cdr (assq-ref mand p))))
         missing-props))
      )))



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface to store and retrieve options from one global option alist.
% The following functions rely on functionality in the
% (oll-core internal alist-access) module

% Global object holding all configuration data of all loaded openLilyLib modules
\newAtree oll-options

% A library can register options (best to be done in the __init__.ily file).
% Later end users can only set registered options, so this is kind of a syntax check.
%
% #1: option path in list or dot notation.
%     The first item should be the library name
% #2 (optional): type predicate
%     If a predicate is given the given initial value must match.
% #3: initial value
%     If the user doesn't set the option explicitly this value is assumed
registerOption =
#(define-scheme-function (opt-path pred init)(symbol-list? (procedure?) scheme?)
   (let
    ((init-val
      (cond
       ((not pred) init)
       ((pred init) init)
       (else
        (begin
         (oll:warn "Type mismatch for option ~a.
  Expected predicate: ~a
  Given: ~a
  Using #f instead"
           (string-join (map symbol->string opt-path) ".") pred init)
         #f)))))
    (setAtree 'oll-options opt-path init-val)
    )
   )

% Convenience function to determine if an option is set.
% can be used to avoid warnings when trying to access unregistered options.
% Returns #t or #f
#(define option-registered?
   (define-scheme-function (path) (symbol-list?)
     (pair? (getAtree #t 'oll-options path))))


% Set an option.
% Only registered options can be set this way.
% #1: Optional argument <force-set>
%     If set and the option is not registered it initialized
%     instead of being rejected.
% #2: Provide a tree path in dotted or list notation
%     the first item of the path is the library name,
%     followed by an arbitrary path at the library's discretion
% #3: Any Scheme value
setOption =
#(define-void-function (force-set path val) ((boolean?) symbol-list? scheme?)
   (let ((is-set (option-registered? path)))
     (if (and (not is-set) force-set)
         (begin
          (registerOption path '())
          (set! is-set #t)))
     (if is-set
         (begin
          (setAtree 'oll-options path val)
          ; TODO: change to oll-log
          (oll:log "Option set: ~a"
            (format "~a: ~a"
              (os-path-join-dots path) val)))
         ;; reject setting unknown options and report that
         ; TODO: change to oll-warning
         (oll:warn "Not a valid option path: ~a" (os-path-join-dots path)))))

% Set a child option below a given option path.
% #1: Optional boolean <force-set>
%     If set this will implicitly create a missing 'parent' node
% #2: <parent-path>
%     A path within the a-tree. Child options will be set/created below
% #3: <option>
%     The name of the option
% #4: <value>
%     The actual option value
setChildOption =
#(define-void-function (force-set parent-path option val)
   ((boolean?) symbol-list? symbol? scheme?)
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
          (os-path-join-dots parent-path)))))

% Retrieve an option
% Provide a tree path in dotted or list notation
% Retrieving a non-existing option path issues a warning and returns #f
getOption =
#(define-scheme-function (path) (symbol-list?)
   (let ((option (getAtree #t 'oll-options path)))
     (if option
         ;; getAtree has returned a pair => option is set
         (cdr option)
         ;; getAtree has returned #f
         (begin
          (oll:warn
           "Trying to access non-existent option: ~a" (os-path-join-dots path))
          #f))))

% Same as \getOption, but retrieving non-existing options returns
% the fallback argument and does not raise a warning.
getOptionWithFallback =
#(define-scheme-function (path fallback)
   (list? scheme?)
   (let ((option (getAtree #t 'oll-options path)))
     (if option
         (cdr option)
         fallback)))

% Retrieve a child option from option <path>.
% If either the 'parent' path or the child option are not present
% a warning is issued and #f returned
getChildOption =
#(define-scheme-function (path child)
   (symbol-list? symbol?)
   (getOption (append path (list child))))

% Same as \getChildOption, but retrieving non-existing options
% returns the fallback argument and doesn't issue a warning.
% This is useful for dynamic options where the user should be
% allowed to provide arbitrary values.
% An example is the setting of arbitrary annotation properties.
getChildOptionWithFallback =
#(define-scheme-function (path child fallback)
   (symbol-list? symbol? scheme?)
   (getOptionWithFallback (append path (list child)) fallback))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Display all currently registered options

displayOptions =
#(define-void-function ()()
   (display "\n\nopenLilyLib: Currently registered options:\n=====\n")
   (pretty-print
    oll-options #:display? #t))

% Display the metadata of a package
describePackage =
#(define-void-function (name)(symbol?)
   (if (member name (getOption '(loaded-packages)))
       (begin
        (format #t "\n\nopenLilyLib: Metadata of package '~a':\n=====\n" name)
        (pretty-print (getOptionWithFallback `(,name meta) "None available")))
       (format #t "\n\nopenLilyLib: Package '~a' is not loaded.\n\n" name)))

% Display the options of a package or module (if available)
% Package options will also include options of loaded modules
displayModuleOptions =
#(define-void-function (path)(symbol-list?)
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
            (format #t "Can't show options, module '~a' is not loaded.\n\n" path)))))


% TODO:
% Provide commands to bulk-process this.
% Maybe also make it possible to load options froma  JSON file
% (although I'm not completely sure if that JSON file would be
% actually easier to maintain).
