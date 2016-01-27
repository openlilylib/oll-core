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
%       Copyright Urs Liska, 2016                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is the main entry file for openLilyLib.
% To use openLilyLib this file has to be in LilyPond's include path.
% including this file with
%     \include "openlilylib.ily"
% will initialize openLilyLib and make the library management available
% as well as significant utility functionality.
%
% This does several things:
% - defines a global variable 'openlilylib-root
%   which is the absolute path to the root of openLilyLib
%   (the parent of the folder this file is located in)
% - adds openlilylib-root to Scheme's module path
% - adds library/module handling support
% - adds option handling
% - adds logging tools
% - adds miscellaneous helper functionality (e.g. version predicates)

% We won't support 2.18 anymore as there are simply too many
% substantial improvements in the 2.19 branch.
% While development versions are usually more or less up to date,
% 2.19.22 marks an important step regarding access to LilyPond's parser.
\version "2.19.22"

#(ly:set-option 'relative-includes #t)

% The os-path and the 'this' modules have to be loaded in order to
% calculate openlilylib-root. However, they are not available yet
% *inside* this function, so we have to split the loading into two
% stages.
#(if (not (defined? 'openlilylib-root))
     (ly:parser-include-string "\\include \"internal/os-path.ily\""))

% Determine (once) global root directory of openLilyLib libraries
openlilylib-root =
#(if (defined? 'openlilylib-root)
     openlilylib-root
     (this-parent))

% Add openLilyLib root directory to Guile's module load path
% After this Scheme modules can be addressed starting from openLilyLib's
% root directory (the parent of oll-core)
\include "internal/add-guile-path.ily"
\addGuilePath #(os-path-join-unix openlilylib-root)


%{
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Common functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Make common functionality available to all openLilyLib "users"
\include "utilities/__main__.ily"

% Logging capabilities with different log levels
\include "logging.ily"

% Common option handling
\include "options.ily"

% Set default loglevel to 'warning'
% (can only be done after options have been included)
\registerOption global.loglevel #oll-loglevel-warning

% Utility to include multiple files at once
% Depends on "options.ily"
\include "utilities/include-pattern.ily"

% Set the root path of openLilyLib
% - for oll module inclusion
% - for Scheme module inclusion
setRootPath =
#(define-void-function (parser location)()
   (let* ((path
           (normalize-path
            (string-append
             (location-extract-path location)
             "/.."))))
     #{ \registerOption global.root-path #path #}))
\setRootPath

% Functionality to load and manage modules
\include "module-handling.ily"

% Welcome message.
% This is a default ly:message because otherwise we'd have to mess around with
% loglevels. This shouldn't be logged anyway.

#(ly:message "\nopenLilyLib: library infrastructure successfully loaded.\n\n")

%}