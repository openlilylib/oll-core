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

% Provide the command
%    \addGuilePath
% to add a folder to Guile's module load path
%
% Originally provided by Jan-Peter Voigt
% and simplified by Urs Liska

\version "2.19.22"

% add a directory to Guile's %load-path (Scheme module search path)
% If path is an absolute path it is simply normalized,
% but if it's relative it is appended to the directory
% of the file the command is used in.
#(define-public addGuilePath
   (define-void-function (path)(string?)
     (let* ((path-arg (os-path-split path))
            (joined-path
             (if (os-path-absolute? path-arg)
                 (os-path-normalize path-arg)
                 (os-path-normalize
                  (append
                   (os-path-split (this-dir))
                   path-arg))))
            (new-path (os-path-join joined-path)))
       (if (not (member new-path %load-path))
           (set! %load-path `(,new-path ,@%load-path))))))
