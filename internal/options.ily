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

% TODO:
% Provide commands to bulk-process this.
% Maybe also make it possible to load options froma  JSON file
% (although I'm not completely sure if that JSON file would be
% actually easier to maintain).
