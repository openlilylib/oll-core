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

#(ly:input-warning (*location*)
   "\n\nYou have loaded \"oll-core/oll-core.ily\" which is deprecated
and will be removed at some point.
Please load \"oll-core/package.ily\" instead.\n\n")

% Initialize oll-core *once*
#(if (not (defined? 'openlilylib-root))
     (begin
      (ly:parser-parse-string (ly:parser-clone) "\\include \"oll-core/internal/os-path.ily\"")
      (define-public openlilylib-root (this-parent))
      (ly:parser-include-string "\\include \"oll-core/internal/init.ily\"")))
