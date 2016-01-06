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
#(define-public os-path-separator
   (if is-windows #\\ #\/ ))

%%%%%%%%%%%%%%%%%
% Path operations
%%%%%%%%%%%%%%%%%

% The core of OS-independent path handling:
% force an arbitrary path to be a list of strings.
% From there we can reconstruct paths in arbitrary ways.

#(define-public (split-path path)
   "Returns a string list with path elements.
    Takes either a path string or a list.
    If 'path' is a string it is split
    respecting the OS dependent path separator,
    if it is a list then the list is returned,
    while elements are converted from symbol to string if necessary."
   (if (string? path)
       (string-split path os-path-separator)
       (map
        (lambda (element)
          (if (string? element)
              element
              (symbol->string element)))
        path)))

#(define-public (join-unix-path path)
   "Returns a Unix formatted path string from a (symbol?/string?) list."
   (string-join (split-path path) "/"))

#(define-public (join-dot-path path)
   "Returns a string in dot-notation (to be displayed).
   Takes a list with symbol?/string? elements or an
   OS independent path string."
   (let ((path-list (split-path path)))
     (string-join path-list ".")))

#(define-public (get-cwd-list)
   "Return the current working directory as a list of strings."
   (split-path (getcwd)))

#(define-public (absolute-path? path)
   "Test if the given path is absolute.
    Process either a string or a symbol?/string? list."
   (let ((path-list (split-path path)))
     (if (and (> (length path-list) 0)
              ;; consider the path absolute if either the regex for windows volumes is matched
              ;; or the first list element is empty (indicating a "/" unix root)
              (or (regexp-exec (make-regexp "^[a-z]:$" regexp/icase) (car path-list))
                  (= 0 (string-length (car path-list)))))
         #t #f)))


#(define-public (normalize-path path)
   "Return a normalized path by removing '.' and '..' elements.
    If 'path' is a string a normalized string is returned,
    if it is a list a list is returned.
    The input string is OS independent (takes os-dependent path separator)
    but the resulting string is Unix-like (because this is nearly always what we need."
   (let* ((path-list (split-path path))
          (normalized
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
              path-list)
             (reverse result))))
     (if (string? path)
         (string-join normalized "/" 'infix)
         normalized)))

#(define-public (absolute-path path)
   "Return absolute path of given 'path'.
    Path can be given as string or string list.
    If 'path' is an absolute path it is simply normalized,
    if it is a relative path it is interpreted as relative 
    to the current working directory.
    Input is OS independent, output is Unix style."
   (let* ((is-string (string? path))
          (path-list (split-path path))
          (abs-path
           (if (absolute-path? path-list)
               path-list
               (append
                (get-cwd-list)
                (normalize-path path-list)))))
     (if is-string
         (string-join abs-path "/" 'infix)
         abs-path)))

#(define-public (normalize-location location)
   "Returns a normalized path to the given location object"
   (normalize-path (car (ly:input-file-line-char-column location))))

#(define-public (location-extract-path location)
   "Returns the normalized path from a LilyPond location
    or './' if 'location' is in the same directory."
   (let* ((loc (normalize-location location))
          (dirmatch (string-match "(.*/).*" loc))
          (dirname (if (regexp-match? dirmatch)
                       (let ((full-string (match:substring dirmatch 1)))
                         (substring full-string
                           0
                           (- (string-length full-string) 1)))
                       ".")))
     (normalize-path dirname)))

% Return the normalized absolute path and file name of the
% file where this function is called from (not the one that
% is compiled by LilyPond).
#(define-public thisFile
   (define-scheme-function ()()
     (normalize-location (*location*))))

#(define-public thisDir
   (define-scheme-function ()()
     (dirname (thisFile))))

#(define-public (this-file-compiled? parser location)
   "Return #t if the file where this function is called
    is the one that is currently compiled by LilyPond."
   (let ((outname (ly:parser-output-name parser))
         (locname (normalize-location location)))
     (regexp-match? (string-match (format "^(.*/)?~A\\.i?ly$" outname) locname))))

% LilyPond format wrapper for this-file-compiled?
#(define-public thisFileCompiled
   (define-scheme-function (parser location)()
     (this-file-compiled? parser location)))
