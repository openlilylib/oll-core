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
% #2: initial value
%     If the user doesn't set the option explicitly this value is assumed
registerOption =
#(define-void-function (opt-path init)(symbol-list? scheme?)
   (setAtree 'oll-options opt-path init ))

% Each package that uses oll-core is encouraged to register itself, ensuring
% there is an option tree available. After this there will be a top-level
% branch for the package, with a first entry for the package's root directory
%
% #1: symbol name for the package.
%     Will be used to navigate the top level of the option tree.
% #2: root path of the package
%     An absolute path list is expected, typically generated
%     using (this-dir) or (this-parent) from within the package.
registerPackage =
#(define-void-function (package-name root-path)
   (symbol? list?)
   (registerOption (list package-name) `((root . ,root-path))))

% Convenience function to determine if an option is set.
% can be used to avoid warnings when trying to access unregistered options.
% Returns the option's value or #f
#(define option-registered
   (define-scheme-function (path) (symbol-list?)
     (pair? (getAtree #t 'oll-options path))))


% Set an option.
% Only registered options can be set this way.
% #1: Provide a tree path in dotted or list notation
%     the first item of the path is the library name,
%     followed by an arbitrary path at the library's discretion
% #2: Any Scheme value
setOption =
#(define-void-function (path val) (symbol-list? scheme?)
   (if (option-registered path)
       (begin
        (setAtree 'oll-options path val)
        ; TODO: change to oll-log
        (ly:input-message (*location*) "Option set: ~a"
          (format "~a: ~a"
            (os-path-join-dots path) val)))
       ;; reject setting unknown options and report that
       ; TODO: change to oll-warning
       (ly:input-warning (*location*) "Not a valid option path: ~a" (os-path-join-dots path))))

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
          (ly:input-warning (*location*)
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

%{

% Set a child option to a given option path.
% This is practical to dynamically add sub-options.
% If the given option path isn't present already it is registered.
% If the child path is present it is modified, otherwise it is added.
setChildOption =
#(define-void-function (parser location opt-path child value)
   (list? symbol? scheme?)
   (if (not #{ \optionRegistered #opt-path #})
       #{ \registerOption #opt-path #'() #})
   (let ((option (append opt-path (list child))))
     (if #{ \optionRegistered #option #}
         #{ \setOption #option #value #}
         #{ \registerOption #option #value #})))

% Retrieve a suboption from an option that stores an alist
% This actually is just another subtree but that function can
% significantly ease the use when sub-options have to be handled dynamically.
% \getOption scholarly.annotate.colors.critical-remark
% is equivalent to
% \getChildOption scholarly.annotate.colors #'critical-remark
% but we can simply pass the sub-option name as a symbol
getChildOption =
#(define-scheme-function (parser location opt-path child)
   (list? symbol?)
   #{ \getOption #(append opt-path (list child)) #})

% Same as \getChildOption, but retrieving non-existing options
% returns the #fallback argument and doesn't issue a warning.
% This is useful for dynamic options where the user should be
% allowed to provide arbitrary values.
% An example is the setting of arbitrary annotation properties.
getChildOptionWithFallback =
#(define-scheme-function (parser location opt-path child fallback)
   (list? symbol? scheme?)
   #{ \getOptionWithFallback #(append opt-path (list child)) #fallback #})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Display all currently registered options

displayOptions =
#(define-void-function (parser location)()
   (display "\n\nopenlilyLib: Currently registered options:\n=====\n")
   (pretty-print
    openlilylib-options #:display? #t))

% TODO:
% Provide commands to bulk-process this.
% Maybe also make it possible to load options froma  JSON file
% (although I'm not completely sure if that JSON file would be
% actually easier to maintain).

%}