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
% Compiled and refactored by Urs Liska, based heavily on work by Jan-Peter Voigt

\version "2.19.22"

#(use-modules
  (lily)
  (ice-9 regex))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions handling the low-level differences between OSes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define a global variable is-windows
% that returns #t when run on a Windows OS
#(define-public is-windows
   (let ((os (getenv "OS")))
     (if (and (string? os)
              (regexp-exec (make-regexp ".*Windows.*" regexp/icase) os))
         #t #f)))

% Define a global variable containing the OS-dependent path separator character
#(define-public os-path-separator-char
   (if is-windows #\\ #\/ ))

#(define-public os-path-separator-string (format "~a" os-path-separator-char))

%%%%%%%%%%%%%%%%%
% Path operations
%%%%%%%%%%%%%%%%%

% The core of OS-independent path handling:
% force an arbitrary path to be a list of strings.
% From there we can reconstruct paths in arbitrary ways.

#(define-public (os-path-split path)
   "Returns a string list with path elements.
    Takes either a path string or a list.
    If 'path' is a string it is split
    respecting the OS dependent path separator,
    if it is a list then the list is returned,
    while elements are converted from symbol to string if necessary."
   (if (string? path)
       (string-split path os-path-separator-char)
       (map
        (lambda (element)
          (if (string? element)
              element
              (symbol->string element)))
        path)))

% Output paths in different forms
% First force the input to be a list, then convert it to the desired format
% All the functions take a 'path' argument as processed by os-path-split.

#(define-public (os-path-join path)
   "Converts a given path to a path corresponding to the OS convention"
   (string-join (os-path-split path) os-path-separator-string))

#(define-public (os-path-join-unix path)
   "Converts a given path to a unix-like path"
   (string-join (os-path-split path) "/"))

#(define-public (os-path-join-dots path)
   "Returns a string in LilyPondish dot-notation (for display)"
   (string-join (os-path-split path) "."))


%%%%%%%%%%%%%%%%%%%%%
% Path manipulations
%
% The following functions all take a path argument
% that can be passed to os-path-split, i.e. a
% OS-specific string or list of strings or symbols.
% They always return the resulting path as a list of strings

% Handling absolute and relative paths

#(define-public (os-path-absolute? path)
   "Test if the given path is absolute"
   (let ((path-list (os-path-split path)))
     (if (and (> (length path-list) 0)
              ;; consider the path absolute if either the regex for windows volumes is matched
              ;; or the first list element is empty (indicating a "/" unix root)
              (or (regexp-exec (make-regexp "^[a-z]:$" regexp/icase) (car path-list))
                  (= 0 (string-length (car path-list)))))
         #t #f)))

#(define-public (os-path-absolute path)
   "Return absolute and normalized path of given 'path'.
    If 'path' is already an absolute path it is simply normalized,
    if it is a relative path it is interpreted as relative 
    to the current working directory."
   (let* ((path-list (os-path-normalize path)))
     (if (os-path-absolute? path-list)
         path-list
         (append
          (os-path-cwd-list)
          path-list))))

#(define-public (os-path-normalize path)
   "Return a normalized path by resolving '.' and '..' elements."
   (let ((result '()))
     (for-each
      (lambda (e)
        (set! result 
              (cond 
               ((equal? e "..")
                ;; go up one directory except if  ".." is the first element
                (if (> (length result) 0) 
                    (cdr result) 
                    `(,e ,@result)))
               ;; strip "." element
               ((equal? e ".") 
                result)
               (else `(,e ,@result)))))
      (os-path-split path))
     (reverse result)))


#(define-public (os-path-cwd-list)
   "Return the current working directory as a list of strings."
   (os-path-split (getcwd)))


