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

\version "2.19.22"

% Add openLilyLib root directory to Guile's module load path
% After this Scheme modules can be addressed starting from openLilyLib's
% root directory (the parent of oll-core)
\include "add-guile-path.ily"
\addGuilePath #(os-path-join openlilylib-root)
\addGuilePath #(os-path-join (append openlilylib-root '(oll-core scheme)))

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Common functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%

% A collection of general-purpose predicates
#(use-modules (oll-core internal tools))
#(use-modules (oll-core internal grob-tools))
#(use-modules (oll-core internal control))

% Version predicates to execute code for specific LilyPond versions
#(use-modules (oll-core internal lilypond-version-predicates))

% Helpers for handling Scheme association lists
#(use-modules (oll-core internal alist-access))

% Logging capabilities with different log levels
\include "logging.ily"

% Option handling,
% for oll-core, other openLilyLib packages or arbitrary end-user code
\include "options.ily"
% Initialize option branch for oll-core
\registerOption oll-core.root #(this-parent)

% Create these nodes as oll-core is not loaded through \loadPackage
\registerOption loaded-packages #'(oll-core)
\registerOption loaded-modules.oll-core #'()

% Functionality to load and manage modules
\include "module-handling.ily"

% Welcome message.
% First set log level to 'log so it will be displayed,
% then set the default log level to 'warning.
#(oll:log "oll-core: library infrastructure successfully loaded.")
\setLogLevel #'warning
