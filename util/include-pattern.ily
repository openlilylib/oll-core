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

\version "2.19.22"

% Should includes be logged or not?
\registerOption oll-core.include-pattern.log-includes ##f

% Include not only a single file but all files in a directory
% that match a given pattern.
% #1: <in-dir>
%     A string indicating the parent directory from which to include
%     If it is an absolute path this is taken literally,
%     a relative path is interpreted relative to the file
%     in which \includePattern is used.
%     If the parent directory does not exist a simple warning is issued
% #2: <pattern>
%     A regex pattern to match files in <in-dir>
%     Files are included in alphabetical order.
%     NOTE: files included later do *not* have access to definitions
%     from earlier files in the same invocation of \includePattern.
includePattern =
#(define-void-function (in-dir pattern)
   (string? string?)
   (let* ((base-dir (os-path-split in-dir))
          (parent
           (os-path-join
            (if (os-path-absolute? base-dir)
                in-dir
                (append (this-dir) base-dir))))
          (includefiles '())
          (pattern-regexp (make-regexp pattern)))

     ;; Only process include pattern when parent directory exists
     (if (and (file-exists? parent)
              (eq? 'directory (stat:type (stat parent))))

         ;; generate list of files in parent that match the pattern
         (let ((dir (opendir parent)))
           (do ((entry (readdir dir) (readdir dir)))
             ((eof-object? entry))
             (if (regexp-exec pattern-regexp entry)
                 (set! includefiles
                       (merge includefiles
                         (list (string-append parent "/" entry))
                         string>?))))
           (closedir dir)

           ;; Open the collected files.
           ;; Note that files do not have access to definitions that are
           ;; in files opened previously in the same execution of \includePattern
           (for-each
            (lambda (file)
              (let ((include-string (format "\\include \"~A\"\n" file)))
                (ly:parser-include-string include-string)))
            includefiles)

           ;; log loading if option is set
           (if (getOption '(oll-core include-pattern log-includes))
               (oll:log "Included through pattern:\n~a" (string-join includefiles "\n"))))

         ;; parent directory doesn't exist
         (oll:warn "Including pattern from dir '~a'. Directory not found" in-dir))))
