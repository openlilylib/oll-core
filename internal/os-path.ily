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

% Provide tools to OS-independently work with file paths.
% Additionally retrieve current file, current and parent dir of the file
% where a function is called from.
%
% Compiled and refactored by Urs Liska, based heavily on work by Jan-Peter Voigt

\version "2.19.22"

#(use-modules
  (lily)
  (ice-9 regex)
  (oll-core internal os-path)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions handling the low-level differences between OSes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define a global variable containing the OS-dependent path separator character
%#(define os-path-separator-char os-path-separator-char)

%#(define os-path-separator-string os-path-separator-string)

%%%%%%%%%%%%%%%%%
% Path operations
%%%%%%%%%%%%%%%%%

% The core of OS-independent path handling:
% force an arbitrary path to be a list of strings.
% From there we can reconstruct paths in arbitrary ways.

%#(define os-path-split os-path-split)
%#(define os-path-split-os os-path-split-os)

% Output paths in different forms
% First force the input to be a list, then convert it to the desired format
% All the functions take a 'path' argument as processed by os-path-split.

#(define os-path-join-os os-path-join-os)
#(define os-path-join os-path-join)
#(define os-path-join-dots os-path-join-dots)


%%%%%%%%%%%%%%%%%%%%%
% Path manipulations
%
% The following functions all take a path argument
% that can be passed to os-path-split, i.e. a
% OS-specific string or list of strings or symbols.
% They always return the resulting path as a list of strings

% Handling absolute and relative paths

#(define os-path-absolute? os-path-absolute?)
#(define os-path-absolute os-path-absolute)
#(define os-path-normalize os-path-normalize)
#(define os-path-cwd-list os-path-cwd-list)
#(define os-path-dirname os-path-dirname)

% processing "location" arguments
%#(define location->normalized-path location->normalized-path)
%#(define location-extract-path location-extract-path)

%%%%%%%%%%%%%%%%%%
% "this" functions
%
% These functions operate on the file where they are used
% (i.e. *not* necessarily the file that is currently being compiled)

#(define this-file this-file)
#(define this-dir this-dir)
#(define this-parent this-parent)
#(define this-file-compiled? this-file-compiled?)

%%%%%%%%%%%%%%%%%%%%%%
% Directory operations
%%%%%%%%%%%%%%%%%%%%%%

% Return all files from the given dir
% as a string list
#(define scandir scandir)
% Return all subdirectories from the given dir
% as a string list
#(define get-subdirectories get-subdirectories)

%%%%%%%%%%%%%%%%%%%%%%%
% Input file operations
%
% Retrieve information and produce variants of the input file name
%%%%%%%%%%%%%%%%%%%%%%

% Returns a list with the absolute path to the compiled input file
#(define os-path-input-file os-path-input-file)
% Returns a string with the absolute path to the compiled input file
#(define os-path-input-filename os-path-input-filename)
% Returns a list with the absolute path of the directory containing the input file
#(define os-path-input-dir os-path-input-dir)
% Returns a string with the absolute path of the directory containing the input file
#(define os-path-input-dirname os-path-input-dirname)
% Returns a string wtih the absolute path to the input file, without file extension
#(define os-path-input-basename os-path-input-basename)
