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

% Provides functions for loading/handling submodules of a package

#(use-modules
  (oll-core internal vbcl)
  (oll-core internal file-handling))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for type-checking of package options

% Simple regex check for Name plus email address in angled brackets:
% "Ben Maintainer <ben@maintainer.org>"
%
% TODO: This seems not correct as in Scheme the dot isn't treated as a special character
%
#(define oll-maintainer-regex
   (make-regexp "^([[:graph:]]+[[:space:]]*)*<([[:graph:]]+)(\\.[[:graph:]]*)*@([[:graph:]]+)(\\.[[:graph:]]+)*>"))
#(define (oll-maintainer? obj)
   (let ((pat oll-maintainer-regex))
     (if (and (string? obj)
              (regexp-exec pat obj))
         #t #f)))

% Returns true for one maintainer or a list of them
#(define (oll-maintainers? obj)
   (or (oll-maintainer? obj)
       (and (list? obj)
            (every oll-maintainer? obj))))

% Simple URL regex.
% Matches HTTP(S) or git@ URLs
#(define repo-url-regex
   (make-regexp "^((https?://)([[:graph:]-]+\\.)+([[:graph:]-]+/)|git@([[:graph:]-]+\\.)+([[:graph:]-]+:))([[:graph:]-]+/)*([[:graph:]]+(\\.[[:graph:]]+)?|/)?$"))
% URL predicate
#(define (repo-url? obj)
   (if (and (string? obj)
            (regexp-exec repo-url-regex obj))
       #t #f))

% Version string regex
% Matches strings with at least two integers separated by dot(s)
#(define version-string-regex
   (make-regexp "^([[:digit:]]+\\.)+([[:digit:]]+)$"))
#(define (oll-version-string? obj)
   (if (and (string? obj)
            (regexp-exec version-string-regex obj))
       #t #f))

%Allowed arguments for \loadModules:
% - symbol-list (single submodule)
% - list with each entry being:
%   - symbol (single module)
%   - symbol-list (single submodule)"
#(define (oll-module-list? obj)
   (if (or (symbol-list? obj)
           (stringlist? obj)
           (and (list? obj)
                (any symbol-list? obj)
                (every (lambda (entry)
                         (or (symbol? entry)
                             (symbol-list? entry))) obj)))
       #t #f))

% Define mandatory and optional properties accepted in package declarations
#(define oll-package-props
   `(strict
     (? name                 ,string? "No package name specified")
     (? display-name         ,string? "No package display name specified")
     (? short-description    ,string? "No short description available")
     (? description          ,string? "No description available")
     (? maintainers          ,oll-maintainers? "No <maintainer.s@available>")
     (? version              ,oll-version-string? "0.0.0")
     (? oll-core             ,oll-version-string? "0.0.0")
     (? license              ,string? "No license specified")
     (? website              ,repo-url? "http://no.website.specified/")
     (? repository           ,repo-url? "http://no.repository.specified/")
  ;; accepted/optional properties
     (? lilypond-min-version ,oll-version-string?)
     (? lilypond-max-version ,oll-version-string?)
     (? dependencies         ,list?)
     (? contributors         ,oll-maintainers?)
     (? modules              ,oll-module-list?)))

#(define (parse-meta lines)
   "Parse the VBCL string list and perform type checking and defaulting.
    Returns an alist (with given and default values) or #f in case of any
    failure."
   (let*
    ((orig-meta (parse-vbcl-config lines))
     (meta (if orig-meta
               (validate-props oll-package-props orig-meta)
               #f)))
    meta))


#(define (register-package name root meta)
   "Create a package's entries in the global option tree"
   (let*
    ((meta-path `(loaded-packages ,name meta)))
    ;; add the bare name to the list of loaded packages
    (setOption '(loaded-packages)
      (append (getOption '(loaded-packages)) (list name)))
    ;; create node loaded-modules-><package-name> to store loaded modules
    (registerOption `(loaded-modules ,name) '())
    ;; create node <package-name>->root and store the package root
    (registerOption `(,name root) root)
    ;; create node <package-name>->meta and store the parsed metadata
    (registerOption `(,name meta) meta)
    #t))

% Test if a given package is already loaded
ollPackageLoaded =
#(define-scheme-function (package)(symbol?)
   (if (memq package (getOption '(loaded-packages))) #t #f))

% Test if a given module in a package is already loaded
ollModuleLoaded =
#(define-scheme-function (package module)(symbol? symbol-list-or-symbol?)
   (let ((module (if (symbol? module) (list module) module)))
     (if
      (and (ollPackageLoaded package)
           (member module (getOptionWithFallback (list 'loaded-modules package) '())))
      #t #f)))

%{
  Load an openLilyLib package.
  Mandatory argument is the package name, given as a (case insensitive) symbol.
  Package options can be given as an optional \with {} clause.
  Options that are not registered in the package will be discarded
    (along with a warning message).
  With the special option 'modules' a symbol-list of (top-level) modules
  can be loaded. Submodules are not supported in this invocation, and
  module names are interpreted case insensitively.
%}
loadPackage =
#(define-void-function (options name)
   ((ly:context-mod?) symbol?)
   "Load an openLilyLib package"
   (let ((name (symbol-downcase name)))
     (if (not (ollPackageLoaded name))
         ;; load the package because it's new
         (let*
          ((package-root (append openlilylib-root (list name)))
           (package-file (os-path-join (append package-root '("package.ily"))))
           (exists (file-exists? package-file))
           (loaded (immediate-include package-file)))
          (if (not loaded)
              (if exists
                  (oll:warn
                   "Error loading package file ~a. Please contact maintainer.\n\n"
                   package-file)
                  (oll:warn
                   "No entry file found for package '~a'. Please check spelling and/or package installation." name))

              ;; loading of the package file has completed successfully
              ;; read metadata and register package
              (let*
               ((meta-file (os-path-join (append package-root '("package.cnf"))))
                (meta-lines (read-lines-from-file meta-file))
                (meta (if meta-lines (parse-meta meta-lines) #f))
                (registered (if meta (register-package name package-root meta) #f)))

               ;; log (un)successful loading of package and metadata
               (oll:log "Package ~a @~a loaded successfully,"
                 name (getOption `(,name meta version)))
               (if registered
                   (oll:log "package metadata successfully read.\n\n")
                   (oll:warn "Dubious or missing metadata for package ~a ignored.
Package not registered. Please contact maintainer!\n\n" name))

               ;; process optional arguments (package options)
               (if options
                   (let*
                    ((options (context-mod->props options))
                     (modules (assq-ref options 'modules)))
                    (for-each
                     (lambda (opt)
                       (let*
                        ((option-name (car opt))
                         (option-path (list name option-name)))
                        (if (not (eq? option-name 'modules))
                            ; TODO:
                            ; can this be simplified once #17 is closed?
                            (if (option-registered? option-path)
                                (setOption option-path (cdr opt))
                                (oll:warn "Unknown option '~a = ~a' for package '~a'"
                                  (car opt) (cdr opt) name)))))
                     options)

                    ;; load modules if given as option
                    ;; A single module can be given or a list of modules.
                    ;; In this each module can be given as symbol or as a symbol list
                    (if modules
                        (if (string? modules)
                            (loadModule (list name (string->symbol modules)))
                            (for-each
                             (lambda (module)
                               (loadModule (list name module)))
                             modules)))
                    )) ;; end (if options)
               ) ;; end (if loaded)

              )) ;; end loading package
         (oll:log "Package '~a' already loaded. Skipping\n\n" name))))

%{
  Load a single package module.
  Mandatory argument is a symbol-list, consisting of the package name and
  the relative module path. Elements are case insensitive.
  The module path can either point to a directory containing a module.ily
  file or directly to an .ily file, given without the extension.
  If the package isn't loaded yet it will implicitly be attempted.
  If loading of the package fails the module isn't loaded either.
  The optional first argument can specify module options in a \with {} clause.
  Allowed options are specified by the module.
%}
loadModule =
#(define-void-function (options module-path)
   ((ly:context-mod?) symbol-list?)
   (let*
    (;(module-path (map symbol-downcase module-path))
      (package (car module-path))
      (module (cdr module-path)))

    ;; implicitly load package when not already loaded
    (if (not (ollPackageLoaded package))
        (loadPackage package))

    (let
     ((loaded (ollModuleLoaded package module)))
     ;; check if module (and package) has already been loaded and warn appropriately
     ;; (as this may indicate erroneous input files)
     (if loaded
         (if options
             (oll:warn "Trying to reload module \"~a\". Skipping. Options will be set anyway."
               (os-path-join-dots (append (list package) module))))
         ;; else load module and register
         (let*
          ((module-base
            (os-path-join
             (append
              openlilylib-root
              module-path)))
           ;; try either a 'module.ily' within a directory ...
           (module-file (string-append module-base "/module.ily"))
           ;; ... or a .ily file
           (module-component (string-append module-base ".ily"))
           (exists
            (cond
             ((file-exists? module-file) module-file)
             ((file-exists? module-component) module-component)
             (else #f)))
           )

          ;; try loading module file and (re)set flag
          (set! loaded (immediate-include exists))
          (if loaded
              ;; register in the option tree
              (setOption `(loaded-modules ,package)
                (append (getOption `(loaded-modules ,package)) (list module)))
              ;; loading failed
              (if exists
                  (oll:warn
                   "Error loading module file ~a. Please contact maintainer.\n\n"
                   module-file)
                  (oll:warn
                   "No entry file found for module '~a'. Please check spelling and/or package installation."
                   (os-path-join-dots module-path))
                  ))))

     ;; if module is (now) loaded process options if present
     (if loaded
         (let ((opts (if options (context-mod->props options) '())))
           (for-each
            (lambda (opt)
              (let*
               ((name (car opt))
                (value (cdr opt))
                (path (append module-path (list name))))
               (if (option-registered? path)
                   (setOption path value)
                   (oll:warn "Unknown module option '~a'" (os-path-join-dots path)))))
            opts))))))


%{
  Load multiple modules from a given package.
  This command does *not* allow module options.
  The first argument is the package name, passed as a case insensitive symbol.
  The second argument is a list of modules, passed in one of the following ways:
  - a symbol list, with a number of top-level modules
    e.g. \loadModules analysis frames.arrows
  - a list with symbols and at least one symbol-list, specifying
    top-level *and* submodules
    e.g. \loadModules package-foo
         #'(module-one
            module-two
            (a nested submodule)
            module-three)
    NOTE: it is crucial that this list contains *at least* one nested list
    because otherwise the whole expression would be interpreted as a
    plain symbol-list!
%}
loadModules =
#(define-void-function (package modules)(symbol? oll-module-list?)
   (let
    ((modules
      (if (stringlist? modules) ;; incoming ffrom VBCL
          (stringlist->symbol-list modules)
          modules)))
    (if (symbol-list? modules)
        ;; list of top-level modules
        (for-each
         (lambda (m)
           (loadModule (list package m)))
         modules)
        ;; a list of multiple modules and submodules
        (for-each
         (lambda (m)
           (let ((module (if (list? m) m (list m))))
             (loadModule (append (list package) module))))
         modules))))
