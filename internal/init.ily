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

% Initializes oll-core and loads secondary internal functionality

\version "2.20.0"

\include "os-path.ily"

% Add openLilyLib root directory to Guile's module load path
% After this Scheme modules can be addressed starting from openLilyLib's
% root directory (the parent of oll-core)
\include "add-guile-path.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Common functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%

#(use-modules
  (oll-core internal tools)
  (oll-core internal grob-tools)
  (oll-core internal control)
  (oll-core internal predicates)
  (oll-core internal lilypond-version-predicates)
  (oll-core internal named-alists)
  (oll-core internal logging)
  (oll-core internal options)
  (oll-core internal properties)
  )

% Storage for all property sets
\registerOption #'(_propsets) #'()
\definePropertySet #'(OLL global) #'()

% Initialize option branch for oll-core
\registerOption oll-core.root #(this-parent)

% Create these nodes as oll-core is not loaded through \loadPackage
\registerOption loaded-packages #'(oll-core)
\registerOption loaded-modules.oll-core #'()

% Functionality to load and manage modules
\include "module-handling.ily"

% Functionality to load additional files
% (submodules load.tools and load.templates have to be loaded explicitly)
\loadModule oll-core.load

% Welcome message.
% First set log level to 'log so it will be displayed,
% then set the default log level to 'warning.
\setLogLevel log
#(oll:log "oll-core: library infrastructure successfully loaded.")
\setLogLevel warning
