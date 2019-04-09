%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of openLilyLib,                                           %
%                      ===========                                            %
% the community library project for GNU LilyPond                              %
% (https://github.com/openlilylib)                                            %
%              -----------                                                    %
%                                                                             %
% Library: oll-core                                                           %
%          ========                                                           %
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
%       Copyright Urs Liska, 2019                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{
  This files contains utility routines to load Score templates from a given directory
%}

\version "2.19.82"

% Root directory from where Tools can be loaded
\registerOption oll-core.load.templates.directory #f

% Include a "template" (file with specific settings or functionality)
% for an example. Available tools are all .ily files within the given directory,
% an option that has to be explicitely set.
% The mandatory argument is the tool-namename of the file without path and file extension.
% If an optional \with {} block is given its entries are temporarily stored in a
% parser variable and can be retrieved *while* loading the tool through:
% \toolOption <option-name> <default-value>
%
% NOTE: The variable is always 'tool-options, and it is only valid during
% *loading* of the tool. If the tool has to make use of the value at a later
% point, e.g. a function call, the value has to be stored securely within the tool.
loadTemplate =
#(define-void-function (options template-name)((ly:context-mod? #f) string?)
   (let ((directory (getOption '(oll-core load templates directory))))
     (if (not directory)
         (oll:warn "Trying to load Template, but oll-core.templates.directory not set")
         (begin
          ;; set 'template-options to the given options or an empty list.
          ;; This can be accessed while *loading* the tool
          (ly:parser-define! 'template-options
            ; TODO: Change to with-options
            (if options (context-mod->props options) '()))
          (let*
           (;TODO: replace dots with slashes (to load tools from subdirectories)
             (template-path #f)
             (template-file (format "~a/~a.ily" directory template-name))
             (exists (file-exists? template-file))
             (loaded (immediate-include template-file)))
           (if (not loaded)
               (if (file-exists? template-file)
                   (oll:warn "Error loading Template ~a" template-name)
                   (oll:warn "Could not load Template '~a': No template file found" template-name))))
          ;; Reset tool options so they can't be accidentally accessed outside loading routine
          (ly:parser-define! 'template-options '())))))

% Retrieve a given value for an option that is optionally passed to a tool.
% If the option has explicitly been given in the \with {} block the value
% is returned, otherwise the default.
% NOTE: this function can only be used upon *loading* the tool, not within
% later function calls from within the tool.
templateOption =
#(define-scheme-function (option default)(symbol? scheme?)
   (let ((option (assq option (ly:parser-lookup 'template-options))))
     (if option (cdr option) default)))

% Precidate for music-name argument
#(define (variable-pair? obj)
   (and (pair? obj)
        (symbol? (car obj))))

% Retrieve the music from a given variable if that is defined,
% otherwise return an empty music expression.
% This can be used in templates that expect one or more music variables,
% e.g. to support one or more voices in a staff.
% Variable names can be either symbols or symbol-anything pairs,
% e.g. \musicOrEmpty one.3 (presumably the third voice in the first staff
% or the third segment in the first part)
musicOrEmpty =
#(define-music-function (music-name)(variable-pair?)
   (let*
    ((name (car music-name))
     (variable (ly:parser-lookup name))
     (content
      (if (= 1 (length music-name))
          variable
          (assq-ref variable (cadr music-name)))))
    (if (ly:music? content)
        content
        #{ #})))
