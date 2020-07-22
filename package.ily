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
% substantial improvements in the 2.19 branch starting from 2.19.22.
\version "2.20.0"

#(ly:set-option 'relative-includes #t)

% Initialize oll-core *once*
#(if (null? (ly:parser-lookup 'openlilylib-root))
     (let*
      ((this (car (ly:input-file-line-char-column (*location*))))
       (path (string-split this #\/))
       (oll-root (list-head path (- (length path) 2)))
       (scheme-path (append oll-root '("oll-core" "scheme")))
       )
      ;; Add openLilyLib root to Guile path
      ;; (enable packages to load Scheme modules through <package-name>)
      (set! %load-path `(,(string-join oll-root "/") ,@%load-path))
      ;; Add internal oll-core Scheme path
      (set! %load-path `(,(string-join scheme-path "/") ,@%load-path))
      (ly:parser-define! 'openlilylib-root oll-root)
      (ly:parser-include-string "\\include \"oll-core/internal/init.ily\"")))
