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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions

% Return the lowercase version of a symbol
#(define (symbol->lowercase sym)
   (string->symbol
    (string-downcase
     (symbol->string sym))))

% Immediate inclusion of files
% Returns #t if file is found and #f if it is missing.
% If the file is considered to have a language different from nederlands
% it must be given at the beginning of the file
#(define (immediate-include file)
   (if (file-exists? file)
       (let ((parser (ly:parser-clone)))
         (ly:parser-parse-string parser "\\language \"nederlands\"")
         (ly:parser-parse-string parser
           (format "\\include \"~a\"" file))
         #t)
       #f))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for type-checking of library options

% Simple regex check for Name plus email address in angled brackets:
% "Ben Maintainer <ben@maintainer.org>"
#(define (oll-maintainer? obj)
   (let ((pat (make-regexp ".*<.*@.*>")))
     (if (and (string? obj)
              (regexp-exec pat obj))
         #t #f)))

% Returns true for one maintainer or a list of them
#(define (oll-maintainers? obj)
   (or (oll-maintainer? obj)
       (and (list? obj)
            (every oll-maintainer? obj))))

% Returns true if obj is a string representation of an integer
#(define (integer-string? obj)
   (integer? (string->number obj)))

% Returns true if a string is a three-element dot-joined list of integers
#(define (oll-version-string? obj)
   (and (string? obj)
        (let ((lst (string-split obj #\.)))
          (and (= 3 (length lst))
               (every integer-string? lst)))))

% Alist with mandatory options for library declarations
% Each entry is a pair of option name symbol and type predicate
#(define oll-lib-mandatory-options
   `((maintainers . ,oll-maintainers?)
     (version . ,oll-version-string?)
     (short-description . ,string?)
     (description . ,string?)
     ))

% Alist with recognized options for library declarations
% If an option is in this list it is type-checked against the given predicate.
#(define oll-lib-known-options
   `((lilypond-min-version . ,oll-version-string?)
     (lilypond-max-version . ,oll-version-string?)
     ))




% Each package that uses oll-core is encouraged to register itself upon loading.
% This will ensure there's an option tree and some metadata about the package
% available.  Additionally this may be used to ensure a package is loaded only
% once. At least \usePackage will check for this and implicitly register the
% package if it doesn't use \registerPackage in the initialization
%
% A root directory will be inferred from the location of the caller, and there
% are mandatory and optional (known) arguments that are checked against type
% predicates.
registerPackage =
#(define-void-function (package-name properties)(symbol? ly:context-mod?)
   (let ((name (symbol->lowercase package-name)))
     (if (option-registered? `(,package-name root))
         (oll:warn "Package ~a already registered." package-name))
     (let*
      ((props (context-mod->props properties))
       (meta-path `(,name meta)))

      ;; check if all required options are present
      ;; and satisfy the given predicates
      (if (not
           (require-props
            (format "Register package ~a" package-name)
            oll-lib-mandatory-options
            properties))
          (oll:error "Error registering package ~a. Please contact maintainer."
            package-name))

      ;; determine package root directory
      (setOption #t
        `(,name root)
        (os-path-dirname (location->normalized-path (*location*))))


      (registerOption meta-path '())

      ;; process and store all options
      ;; stop with error when facing an unknown option
      ;; or a type check fails.
      ;; while this is technically unimportant we want
      ;; to "encourage" package maintainers to be correct
      ;; about this.
      (let
       ((mandatory (map car oll-lib-mandatory-options))
        (known (map car oll-lib-known-options)))
       (for-each
        (lambda (prop)
          (let ((prop-key (car prop))
                (prop-value (cdr prop)))
            (if
             (or (member prop-key mandatory)
                 (and (member prop-key known)
                      ((cdr (assq prop-key oll-lib-known-options)) prop-value)))
             (setChildOption meta-path (car prop)(cdr prop))
             (ly:error "Error in registration of package \"~a\".
Unknown option \"~a\" or type mismatch: ~a.
Please contact package maintainer(s)\n - ~a"
               name prop-key prop-value
               (let ((maintainers (assq-ref props 'maintainers)))
                 (if (string? maintainers)
                     maintainers
                     (string-join maintainers "\n - "))
                 )))))
        props))

      ;; print a confirmation of successful registration
      (ly:message "\nPackage ~a @~a registered successfully.\n\n"
        package-name (assq-ref props 'version)))))




% Packages can register 'modules' that are not implicitly loaded
% together with the package itself. Modules can then be loaded
% upon request.
%
% #1: The package name
% #2: A list of modules, formatted as a symbol list, either way of
%    #'(mod-a mod-b mod-c)
%    mod-a.mod-b.mod-c
% Each module lives within a subdirectory of the package, named
% exactly like the symbol passed to \registerModules, so the
% directory naming is limited to LilyPond's symbol? parsing.
% The module must then contain the file module.ily, which will
% then be loaded by \useModule.
registerModule =
#(define-void-function (path)(symbol-list?)
   (let* ((path (map symbol->lowercase path))
          (package (symbol->lowercase (car path)))
         (module-path
          (append
           `(,package modules)
           (cdr path)
           '(root))))
     (registerOption module-path
       (append (getOption `(,package root)) (cdr path)))))

% Check if a module is registered.
% Return the absolute path to the module's entry file
% or #f.
#(define module-entry
   (define-scheme-function (path)(symbol-list?)
     (let* ((package (car path))
            (module (cdr path))
            (module-path
             (append
              `(,package modules)
              module
              '(root)))
            (module-dir (getOptionWithFallback module-path #f)))
       (if module-dir
           (append module-dir (list "module.ily"))
           #f))))

% Load a module from within a package.
% Module locations are looked up from the package's 'modules' options,
% and trying to load a non-existent module will cause a warning.
%
% An optional \with {} clause can contain options that will be set
% after the module has been loaded. Such options must have been registered
% in the module definition file.
loadModule =
#(define-void-function (opts path)
   ((ly:context-mod?) symbol-list?)
   (let ((module-file (module-entry path)))
     (if (not module-file)
         (oll:warn "Trying to load unregistered module '~a'"
           (os-path-join-dots path))
         (begin
          (ly:parser-parse-string (ly:parser-clone)
            ;
            ; TODO: Check how this is to be done on Windows"
            (format "\\include \"~a\"" (os-path-join module-file)))
          (if opts
              (for-each
               (lambda (opt)
                 (let* ((path `(,package ,module ,(car opt)))
                        (is-registered (option-registered? path)))
                   (if is-registered
                       (setOption path (cdr opt))
                       (oll:warn "Trying to set unregistered option '~a'"
                         (os-path-join-dots path)))))
               (extract-options opts)))))))
