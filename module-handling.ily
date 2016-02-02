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
#(define-void-function (package-name)(symbol?)
   (if (not (option-registered `(,package-name root)))
       (setOption #t
         `(,package-name root)
         (os-path-dirname (location->normalized-path (*location*))))
       (oll:warn "Package ~a already registered." package-name)))

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
      (setOption #t `(,package modules ,mod)
        (append (getOption `(,package root)) (list mod))))
    modules))

% Check if a module is registered.
% Return the absolute path to the module's entry file
% or #f.
#(define module-entry
   (define-scheme-function (package module)(symbol? symbol?)
     (let ((module-dir (getOptionWithFallback `(,package modules ,module) #f)))
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
#(define-void-function (opts package module)
   ((ly:context-mod?) symbol? symbol?)
   (let ((module-file (module-entry package module)))
     (if (not module-file)
         (oll:warn "Trying to load unregistered module '~a'"
           (os-path-join-dots `(,package ,module)))
         (begin
          (ly:parser-parse-string (ly:parser-clone)
            ;
            ; TODO: Check how this is to be done on Windows"
            (format "\\include \"~a\"" (os-path-join module-file)))
          (if opts
              (for-each
               (lambda (opt)
                 (let* ((path `(,package ,module ,(car opt)))
                        (is-registered (option-registered path)))
                   (if is-registered
                       (setOption path (cdr opt))
                       (oll:warn "Trying to set unregistered option '~a'"
                         (os-path-join-dots path)))))
               (extract-options opts)))))))
