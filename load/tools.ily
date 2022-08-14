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
  This files contains utility routines to load "Tools" from a given directory
%}

\version "2.19.82"

% Root directory from where Tools can be loaded
\registerOption oll-core.load.tools.directory #f

% Include a "tool" (file with specific settings or functionality)
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
loadTool =
#(define-void-function (options tool-name)((ly:context-mod? #f) string?)
   (let ((directory (getOption '(oll-core load tools directory))))
     (if (not directory)
         (oll:warn "Trying to load Tool, but oll-core.tools.directory not set")
         (begin
          ;; set 'tool-options to the given options or an empty list.
          ;; This can be accessed while *loading* the tool
          (ly:parser-define! 'tool-options
            ; TODO: Change to with-options
            (if options (context-mod->props options) '()))
          (let*
           (;TODO: replace dots with slashes (to load tools from subdirectories)
             (tool-path #f)
            (tool-file (format #f "~a/~a.ily" directory tool-name))
            (exists (file-exists? tool-file))
            (loaded (immediate-include tool-file)))
           (if (not loaded)
               (if (file-exists? tool-file)
                   (oll:warn "Error loading tool ~a" tool-name)
                   (oll:warn "Could not load Tool '~a': No tool file found" tool-name))))
          ;; Reset tool options so they can't be accidentally accessed outside loading routine
          (ly:parser-define! 'tool-options '())))))

% Retrieve a given value for an option that is optionally passed to a tool.
% If the option has explicitly been given in the \with {} block the value
% is returned, otherwise the default.
% NOTE: this function can only be used upon *loading* the tool, not within
% later function calls from within the tool.
toolOption =
#(define-scheme-function (option default)(symbol? scheme?)
   (let ((option (assq option (ly:parser-lookup 'tool-options))))
     (if option (cdr option) default)))
