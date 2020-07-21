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

#(use-modules
  (ice-9 pretty-print)
  (oll-core internal predicates)
  (oll-core internal options))


% Macro to facilitate definition of functions with options.
% Begin the function definition with 'with-options and give the ruleset
% before the body of the function.
% Example:
% (with-options define-void-function () ()
%   `(strict
%      (msg ,string?)
%      (? author ,string? "Anonymous"))
%   (pretty-print props))
% Warning: The body of the function can't be empty.
#(define-macro (with-options proc vars preds rules . body)
   (let ((optional #t))
     (apply make-opts-function-declaration `(,proc ,vars ,preds ,rules ,optional . ,body))))

#(define-macro (with-opts . rest)
   `(with-options . ,rest))

#(define-macro (with-required-options proc vars preds rules . body)
   (let ((optional #f))
     (apply make-opts-function-declaration `(,proc ,vars ,preds ,rules ,optional . ,body))))

#(define-macro (with-req-opts . rest)
   `(with-required-options . ,rest))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface to store and retrieve options from one global option alist.
% The following functions rely on functionality in the
% (oll-core internal alist-access) module

% Global object holding all configuration data of all loaded openLilyLib modules
%\newAtree oll-options

% A library can register options (best to be done in the __init__.ily file).
% Later end users can only set registered options, so this is kind of a syntax check.
%
% #1: option path in list or dot notation.
%     The first item should be the library name
% #2: initial value
%     If the user doesn't set the option explicitly this value is assumed
registerOption =
#(define-void-function (opt-path init)(symbol-list? scheme?)
   (register-option opt-path init))


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
   (set-option force-set path val))

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
   (set-child-option force-set parent-path option val))

% Set multiple child options below a given option path.
% #1: Optional boolean <force-set>
%     If set this will implicitly create a missing 'parent' node
% #2: <parent-path>
%     A path within the a-tree. Child options will be set/created below
% #3: <children>
%     an alist with the children
setChildOptions =
#(define-void-function (force-set parent-path children)
   ((boolean?) symbol-list? alist?)
   (set-child-options force-set parent-path children))

% Append a value to a list option
% #1: Optional boolean <create>
%     If set this will implicitly create an empty list to append to
% #2: <path>
%     A path within the option tree.
%     If <create> is not #t and <path> doesn't exist a warning is issued
% #3: <val>
%     Any Scheme value to be appended to the list option
appendToOption =
#(define-void-function (create path val)
   ((boolean?) symbol-list? scheme?)
   (append-to-option create path val))

% Retrieve an option
% Provide a tree path in dotted or list notation
% Retrieving a non-existing option path issues a warning and returns #f
getOption =
#(define-scheme-function (path) (symbol-list?)
   (get-oll-option path))

% Same as \getOption, but retrieving non-existing options returns
% the fallback argument and does not raise a warning.
getOptionWithFallback =
#(define-scheme-function (path fallback)
   (list? scheme?)
   (get-option-with-fallback path fallback))

% Retrieve a child option from option <path>.
% If either the 'parent' path or the child option are not present
% a warning is issued and #f returned
getChildOption =
#(define-scheme-function (path child)
   (symbol-list? symbol?)
   (get-child-option path child))

% Same as \getChildOption, but retrieving non-existing options
% returns the fallback argument and doesn't issue a warning.
% This is useful for dynamic options where the user should be
% allowed to provide arbitrary values.
% An example is the setting of arbitrary annotation properties.
getChildOptionWithFallback =
#(define-scheme-function (path child fallback)
   (symbol-list? symbol? scheme?)
   (get-child-option-with-fallback path child fallback))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Display all currently registered options

displayOptions =
#(define-void-function (root)((symbol-list? '()))
   (display "\n\nopenLilyLib: Currently registered options:\n=====\n")
   (let ((use-root (if (null? root)
                       oll-options
                       (getOption root))))
     (pretty-print
      ;oll-options
      use-root #:display? #t)))

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
