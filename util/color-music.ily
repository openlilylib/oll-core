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

\version "2.19.80"


% Color or uncolor the given grob types by creating a list of \overrides
% - grob-names: a list of grob names
% - col: the color to be used
% - on: a boolean, if ##f switch coloring *off* again
%   note that col has to be supplied even for *un*coloring.
colorGrobs =
#(define-music-function (grob-names col on)(symbol-list? color? boolean?)
   (make-sequential-music
    (map
     (lambda (gn)
       (if on
           #{ \temporary \override #gn #'color = #col #}
           #{ \revert #gn #'color #}))
     grob-names)))

% Color all grobs in the given music expression
% This is not the most efficient function since it creates overrides
% for *all* registered grob types. But the list of grob names is only
% generated once upon loading and then cached in the closure.
colorMusic =
#(let ((grob-names (map car all-grob-descriptions)))
   (define-music-function (my-color music)
     (color? ly:music?)
     (make-sequential-music
      (list
       (colorGrobs grob-names my-color #t)
       music
       (colorGrobs grob-names my-color #f)))))


