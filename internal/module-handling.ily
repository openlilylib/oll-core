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
  (oll-core scheme vbcl)
  (oll-core scheme file-handling))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for type-checking of package options

% Simple regex check for Name plus email address in angled brackets:
% "Ben Maintainer <ben@maintainer.org>"
%
% TODO: This seems not correct as in Scheme the dot isn't treated as a special character
%
#(define oll-maintainer-regex
   (make-regexp "^([[:alnum:]]+)[[:space:]]*<([[:alnum:]]+)(\\.[[:alnum:]]*)*@([[:alnum:]]+)(\\.[[:alnum:]]+)*>"))
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
   (make-regexp "^((https?://)([[:alnum:]]+\\.)+([[:alnum:]]+/)|git@([[:alnum:]]+\\.)+([[:alnum:]]+:))([[:alnum:]]+/)*([[:alnum:]]+(\\.[[:alnum:]]+)?|/)?$"))
% URL predicate
#(define (repo-url obj)
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
% - single string (single module)
% - single symbol (single module)
% - symbol-list (single submodule)
% - list with each entry being:
%   - symbol (single module)
%   - symbol-list (single submodule)"
#(define (oll-module-list? obj)
   (if (or (string? obj)
           (symbol? obj)
           (symbol-list? obj)
           (and (list? obj)
                (every (lambda (entry)
                         (or (string? entry)
                             (symbol? entry)
                             (symbol-list? entry))) obj)))
       #t #f))


% Alist with mandatory options for library declarations
% Each entry is a list with
% - option name
% - type predicate
% - Default value
#(define oll-mandatory-props
   (make-mandatory-props
    `((name ,string? "No package name specified")
      (display-name ,string? "No package display name specified")
      (short-description ,string? "No short description available")
      (description ,string? "No description available")
      (maintainers ,oll-maintainers? "No <maintainer(s)@available>")
      (version ,oll-version-string? "0.0.0")
      (oll-core ,oll-version-string? "0.0.0")
      (license ,string? "No license specified")
      (repository ,repo-url "http://no.repository.specified/")
      )))

% Alist with recognized options for library declarations
% If an option is in this list it is type-checked against the given predicate.
#(define oll-accepted-props
   (make-accepted-props
    `((lilypond-min-version . ,oll-version-string?)
      (lilypond-max-version . ,oll-version-string?)
      (dependencies . ,list?)
      (modules . ,oll-module-list?)
      )))


#(define (parse-meta lines)
   "Parse the VBCL string list and perform type checking and defaulting.
    Returns an alist (with given and default values) or #f in case of any
    failure."
   (let*
    ((orig-meta (parse-vbcl-config lines))
     (meta (if orig-meta
               (check-props oll-mandatory-props oll-accepted-props orig-meta)
               #f)))
    meta))


#(define (register-package name root meta)
   "Create a package's entries in the global option tree"
   (let*
    ((meta-path `(loaded-packages ,name meta)))
    ;; add the bare name to the list of loaded packages
    (setOption '(loaded-packages)
      (append (getOption '(loaded-packages)) (list name)))
    ;; create node <package-name>->root and store the package root
    (registerOption `(,name root) root)
    ;; create node <package-name>->meta and store the parsed metadata
    (registerOption `(,name meta) meta)
    ;; create node <package-name>->modules to store module options later
    (registerOption `(,name modules) '())
    #t))


%{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module loading code (take care of that later)

#(define (module-list? obj)
   (if (and
        (symbol-list? obj)
        (= (length obj) 2))
       #t #f))

% Load a module from within a package.
% Module locations are looked up from the package's 'modules' options,
% and trying to load a non-existent module will cause a warning.
%
% An optional \with {} clause can contain options that will be set
% after the module has been loaded. Such options must have been registered
% in the module definition file.
loadModule =
#(define-void-function (opts path)
   ((ly:context-mod?) module-list?)
   (let*
    ((package (car path))
     (module (cdr path)))
   (if
    (member module (getOption (list 'loaded-modules package)))
    (oll:warn "Trying to reload module \"~a\". Skipping. Options will be set anyway."
      (os-path-join-dots path))
    (let ((module-file (module-entry path)))
      (if (not module-file)
          (oll:warn "Trying to load unregistered module '~a'"
            (os-path-join-dots path))
          (begin
           (if (not (immediate-include (os-path-join-unix module-file)))
               (oll:warn "No file found for module \"~a\"" path))
           (setOption '(loaded-modules)
             (append (getOption '(loaded-modules)) (list path)))))))
   (if opts
       (for-each
        (lambda (opt)
          (let* ((opt-path
                  (append path (list (car opt))))
                 (is-registered (option-registered? opt-path)))
            (if is-registered
                (setOption opt-path (cdr opt))
                (oll:warn "Trying to set unregistered option '~a'"
                  (os-path-join-dots opt-path)))))
        (context-mod->props opts))))
   )

% Module loading code (take care of that later)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%}


loadModule =
#(define-void-function (options module)
   ((ly:context-mod?) symbol-list?)
   (let*
    ((package (car module))
     (opts (if options (context-mod->props options) #f))
     )
    (registerOption (append (list (car module) 'modules) (cdr module)) '())

   ;
   ; TODO:
   ; Check if package has already been loaded.
   ; If not load it implicitly, without options and then load the modules
   ;
   (display "")

   ))

loadModules =
#(define-void-function (package modules)(symbol? oll-module-list?)
   "Load one or more package modules.
    This function dispatches the given module(s) argument to one
    or multiple calls to (loadModule).
    A package name has to be provided as a symbol.
    If the package isn't loaded yet it will be implicitly loaded.
    For the type of the module(s list) refer to oll-module-list? above)."
   (cond
    ((string? modules)
     ;; single module
     (loadModule (list package (string->symbol modules))))
    ((symbol? modules)
     ;; single module
     (loadModule (list package modules)))
    ((symbol-list? modules)
     ;; single submodule
     (loadModule (append (list package) modules)))
    ((list? modules)
     ;; a list of multiple modules is provided
     (for-each
      (lambda (module)
        (cond
         ((symbol? module)
          ;; simple module, package has to be provided separately
          (loadModule (list package module)))
         ((symbol-list? module)
          ;; submodule
          (loadModule (append (list package) module)))))
      modules))))

loadPackage =
#(define-void-function (options name)
   ((ly:context-mod?) symbol?)
   "Load an openLilyLib package"
   (if (not (member name (getOption '(loaded-packages))))
       ;; load the package because it's new
       (let*
        ((package-root (append openlilylib-root (list name)))
         (package-file (os-path-join-unix (append package-root '("package.ily"))))
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
             ((meta-file (os-path-join-unix (append package-root '("package.cnf"))))
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
       (oll:log "Package '~a' already loaded. Skipping\n\n" name)))
