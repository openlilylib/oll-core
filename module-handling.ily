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

% Each package that uses oll-core is encouraged to register itself, ensuring
% there is an option tree available. After this there will be a top-level
% branch for the package, with a first entry for the package's root directory
% The root directory will be inferred from the directory the calling file
% is in, so \registerPackage should be called from the package's root directory.
%
% #1: symbol name for the package.
%     Will be used to navigate the top level of the option tree.
registerPackage =
#(define-void-function (package-name)
   (symbol?)
   (setPackageOption #t
    `(,package-name root)
    (os-path-dirname (location->normalized-path (*location*)))))

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
registerModules =
#(define-void-function (package modules)(symbol? symbol-list?)
   (for-each
    (lambda (mod)
      (setModuleOption #t `(,package ,mod root)
        (append (getPackageOption `(,package root)) (list mod))))
    modules))

% The options for packages and modules are maintained using extra
% 'packages', 'modules' and 'options' nodes in the options tree.
% The following functions are used to let the user (or other packages)
% transparently access the options.
% Packages and modules are limited to a flat structure, while options
% may be arbitrarily nested trees. So the following options are valid:
%
% - some-package.an-option
% - some-package.option-group.an-option
% - some-package.some-module.an-option
% - some-package.some-module.another-option
% - some-package.some-module.option-subgroup.another-option
% but not:
% - some-package.a-sub-package.an-option
% - some-package.some-module.a-sub-module.an-option
% - etc.

% Two functions to transparently wrap the option paths for packages and modules
#(define (package-option-path path)
   (append `(packages ,(car path) options) (cdr path)))

#(define (module-option-path path)
   (append `(packages ,(car path) modules ,(second path) options) (cddr path)))

% Wrapper functions to set and retrieve options in packages and modules.
% These make use of the above path wrappers.
%
% TODO: either add the missing functions or find a generic way
% to wrap the normal getOption etc.

% Set an option in a package. Wrapper to \setOption.
% <path> consists of
% - package name
% - option (optionally nested)
% ex.: \setPackageOption #t some-package.colors.highlight #red
%      \setPackageOption some-package.use-colors ##t
setPackageOption =
#(define-void-function (force-set path val)((boolean?) symbol-list? scheme?)
   (setOption force-set (package-option-path path) val))

% Retrieve a package option. Wrapper to \getOption
% See setPackageOption
getPackageOption =
#(define-scheme-function (path)(symbol-list?)
   (getOption (package-option-path path)))

% Set an option in a module. Wrapper to \setOption.
% <path> consists of
% - package name
% - module name
% - option (optionally nested)
% ex.: \setModuleOption #t some-package.some-module.colors.highlight #red
%      \setModuleOption some-package.some-module.use-colors ##t
setModuleOption =
#(define-void-function (force-set path val) ((boolean?) symbol-list? scheme?)
   (setOption force-set (module-option-path path) val))

% Retrieve a module option. Wrapper to \getOption
% See setModuleOption
getModuleOption =
#(define-scheme-function (path) (symbol-list?)
   (getOption (module-option-path path)))

% TODO: The following functions seem to be missing, both for packages and modules:
% - setChildOption
% - getChildOption
% - ...WithFallback

%{

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper tools

% Extract an options alist from a context-mods argument
% Return an empty list if no mods are passed.
#(define (extract-options ctx-mods)
   (if ctx-mods
       (map (lambda (o)
              (cons (cadr o) (caddr o)))
         (ly:get-context-mods ctx-mods))
       '()))

% Initialize a library before first use.
% This also serves as a kind of declaration of the intent of using it.
% If options are passed in a \with {} clause they are set after in
% initialization file has been loaded. If the initializiation did not
% register the options (in the form LIBRARY.OPTION) this will cause
% warnings about trying to set unregistered options.
useLibrary =
#(define-void-function (parser location options name)
   ((ly:context-mod?) symbol? )
   (let*
    ;; ensure the library name is lowercase
    ((display-name name)
     (name (string->symbol (string-downcase (symbol->string name)))))
   "Load an openLilyLib library and initialize it"
   (if (not (member name oll-loaded-libraries))
       ;; Determine paths to init and main files
       (let* ((lib-dir
               (string-append
                #{ \getOption global.root-path #}
                "/" (symbol->string name) "/"))
              (init-file
               (string-append lib-dir "__init__.ily"))
              (main-file
               (string-append lib-dir "__main__.ily")))

         ;; Create a root option for the library
         #{ \registerOption #(list name) #'() #}

         ;; Load initialization file if it exists
         (if (file-exists? init-file)
             (begin
              (oll:log location "Initialize library \"~a\" ..." display-name)
              (let ((arg (format "\\include \"~a\"" init-file)))
                (if (lilypond-greater-than? "2.19.21")
                    (ly:parser-include-string arg)
                    (ly:parser-include-string parser arg)))))

         ;; If a \with clause has been given pass the options to the library.
         ;; If the options have not been registered in the __init__ file this
         ;; will trigger oll:warn messages but don't abort the job.
         (if options
             (for-each
              (lambda (o)
                (let ((opt-path (list name (cadr o)))
                      (opt-val (caddr o)))
                  #{ \setOption #opt-path #opt-val #}))
              (ly:get-context-mods options)))

         ;; load the main file of the library or issue a warning if that isn't found.
         (if (file-exists? main-file)
             (begin
              ;              (ly:parser-include-string parser (ly:gulp-file main-file))
              (let ((arg (format "\\include \"~a\"" main-file)))
                (if (lilypond-greater-than? "2.19.21")
                    (ly:parser-include-string arg)
                    (ly:parser-include-string parser arg))

              (set! oll-loaded-libraries
                    (append oll-loaded-libraries
                      `(,name)))
              (oll:log "... completed." "")))
             (oll:warn location (format "Library main file \"~a\" not found" main-file)))))))


% Load a module from within a library.
% A module is either a single .ily file or a __main__.ily file in a folder.
% It is adressed as a dotted path representing the directory structure
% leading to the file. The first element of the path is the library, the last one
% is the name of the module.
% It is looked for files path/to/NAME.ily or path/to/NAME/__main__.ily
%
% An optional \with {} clause can contain options that will be set
% after the module has been loaded. Such options must have been registered
% in the module definition file.

useModule =
#(define-void-function (parser location options sym-path)
   ((ly:context-mod?) list?)
   (let ((dot-path (join-dot-path sym-path)))
     ;; only do any work if the module isn't already loaded
     (if (member dot-path oll-loaded-modules )
         (oll:warn location
           (format "Module already loaded. Skipping \"~a\"" dot-path))
         (let*
          ((library (car sym-path))
           (mod-path (cdr sym-path))
           ;; unix file path to the module (base)
           (module-basename
            (string-append
             #{ \getOption global.root-path #}
             "/" (join-unix-path sym-path)))
           ;; Check if a valid file can be found for the module path
           ;; #f if no file is found
           (ext
            (or (if (file-exists? (string-append module-basename ".ily"))
                    ".ily" #f)
                (if (file-exists? (string-append module-basename "/__main__.ily"))
                    "/__main__.ily" #f)))
           (filename
            (if ext
                (string-append module-basename ext) #f))
           (init-file
            (and ext
                 (string=? "/__main__.ily" ext)
                 (let ((fname (string-append module-basename "/__init__.ily")))
                   (if (file-exists? fname)
                       fname #f))))
           (opts (extract-options options)))

          ;; Load module if present
          (if filename
              ;; but only if the library is already loaded
              (if (not (member library oll-loaded-libraries))
                  (oll:warn location
                    (format "Library \"~a\" must be loaded before module \"~a\""
                      library (join-dot-path mod-path)))
                  (begin

                   ;; include init-file if present
                   (if init-file
                       (if (lilypond-greater-than? "2.19.21")
                       (ly:parser-include-string
                         (format "\\include \"~a\"" init-file))
                       (ly:parser-include-string parser
                         (format "\\include \"~a\"" init-file))))


                   ;; include module file
                   (if (lilypond-greater-than? "2.19.21")
                       (ly:parser-include-string
                        (format "\\include \"~a\"" filename))
                       (ly:parser-include-string parser
                         (format "\\include \"~a\"" filename)))

                   ;; register module
                   (set! oll-loaded-modules
                         (append oll-loaded-modules (list dot-path)))

                   ;; pass along options
                   (for-each
                    (lambda (o)
                      #{ \setChildOption #sym-path #(car o) #(cdr o) #})
                    opts)

                   ;; TODO: COntinue with setting options
                   ))
              (oll:warn location
                (format "No file found for module ~a"
                  (join-dot-path (append (list library) mod-path)))))))))



% Conditionally register and load a library when
% for the first time a module from that library is requested.

%%% DEPRECATED !!!
%%% This is deprecated together with \loadModule
%%%
registerLibrary =
#(define-void-function (parser location lib)
   (string?)
   "Register a library with the configuration system
    if it hasn't been already loaded.
    If the library has an __init__.ily file
    this is loaded (library initialized) too."
   (if (not (member lib oll-loaded-libraries))
       (begin
        (set! oll-loaded-libraries
              (append oll-loaded-libraries
                `(,lib)))
        (let* ((root #{ \getOption global.root-path #})
               (lib-init-file (string-join
                               `(,root ,lib "__init__.ily") "/")))
          (if (file-exists? lib-init-file)
              (begin
               (oll:log "initialize library \"~a\"" lib)
               (if (lilypond-greater-than? "2.19.21")
                   (ly:parser-include-string
                     (format "\\include \"~a\"" lib-init-file))
                   (ly:parser-include-string parser
                     (format "\\include \"~a\"" lib-init-file))
                   )))))))

% Load module from an openLilyLib library
% A module may be an individual file or a whole library, this can also be
% designed by the individual library.
% The string argument to be given is the path to the module, starting from
% the root directory of openLilyLib. It can be either an actual file or a
% directory name indicating the module (the check is whether the last item
% contains a dot in its name). If there's no dot in the last element of the
% path we assume it is a directory and try to load a file "__main__.ily"
% inside that directory.
loadModule =
#(define-void-function (parser location path)(string?)
   "Load an openLilyLib module if it has not been already loaded."

   ;; DEPRECATION !!!
   (oll:warn location
     "\n    \\loadModule
    is deprecated and will eventually be removed.
    Please use the more idiomatic and powerful
      \\useLibrary and
      \\useModule now.")

   (let*
    ((path-list (string-split path #\/))
     (lib (first path-list))
     (last-elt
      (if (string-index (last path-list) #\.)
          ;; if the last element is a file (with extension)
          ;; we don't do anything, otherwise we append the
          ;; default "module name"
          '()
          '("__main__.ily")))
     (append-path (string-join
                   (append path-list last-elt) "/"))
     (load-path (string-append
                 #{ \getOption global.root-path #}
                 "/"
                 append-path)))

    ;; try to load the file if it isn't already present
    (if (member load-path oll-loaded-modules)
        (oll:log "module ~a already loaded. Skipping." load-path)
        (if (file-exists? load-path)
            (begin
             (oll:log "Registering library ~a" (first path-list))
             ;; first register/load the library
             #{ \registerLibrary #(first path-list) #}
             ;; then load the requested module
             (oll:log "load module ~a" load-path)
             (if (lilypond-greater-than? "2.19.21")
                 (ly:parser-include-string
                  (format "\\include \"~a\"" load-path))
                 (ly:parser-include-string parser
                   (format "\\include \"~a\"" load-path)))
             (set! oll-loaded-modules
                   (append! oll-loaded-modules `(,load-path))))
            (oll:warn "module not found: ~a" load-path)))))
%}
