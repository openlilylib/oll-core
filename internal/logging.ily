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
%       Copyright Urs Liska, 2015                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Implements logging facilities (to console and/or files)

#(use-modules
  (oll-core internal logging))

% Set the log level. oll-core's oll: logging functions won't do anything
% if their log level is lower than the currently set level.
% <level> has to be one of the symbols used in 'oll-loglevels'
setLogLevel =
#(define-void-function (level)(symbol?)
   (set-log-level level))

% Critical error
% Aborts the compilation of the input file
% so use with care!
#(define (oll:error fmt . vals)
   (oll-error fmt vals))

% Warning
#(define (oll:warn fmt . vals)
   (oll-warn fmt vals))

% General logging
#(define (oll:log fmt . vals)
   (oll-log fmt vals))

% Debug output
#(define (oll:debug fmt . vals)
   (oll-debug fmt vals))
