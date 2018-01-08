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

% Constant symbols representing the different log levels.
#(define oll-loglevels
   '((nolog . 0)
     (critical . 1)
     (warning . 2)
     (log . 3)
     (debug . 4)))

% Define one single public variable.
% We can't use oll-core's options for this because they are not loaded yet -
% and the option handline needs the logging code ...
% Initialize to 'log, will later be set to 'warning
#(define oll-loglevel 2)

% Set the log level. oll-core's oll: logging functions won't do anything
% if their log level is lower than the currently set level.
% <level> has to be one of the symbols used in 'oll-loglevels'
setLogLevel =
#(define-void-function (level)(symbol?)
   (let ((new-level (getAtree #t 'oll-loglevels (list level))))
     (if new-level
         (set! oll-loglevel (cdr new-level))
         (oll:warn
          (*location*) "Not a valid openLilyLib log level: ~a. Ignoring" level))))

% Open log file
#(define oll-logfile
   (open-output-file
    (format "~a.oll.log" (ly:parser-output-name (*parser*)))))

% Check if a logging function should be executed
% by comparing the value passed in <loglevel> to the
% currently active log level
#(define (oll:do-log loglevel)
   (>= oll-loglevel (getAtree 'oll-loglevels `(,loglevel))))

% Generic function to consistently write to log file.
% <title> is a sectioning header in the log file
% <fmt> and <vals> are simply passed along.
#(define (oll:log-to-file title fmt vals)
   (format oll-logfile
     (string-append
      "\n"
      (os-path-join-os (location->normalized-path (*location*)))
      "\nLine: "
      (number->string (cadr (ly:input-file-line-char-column (*location*))))

      "\n~a:\n"
      (apply format fmt vals)
      "\n\n")
      title))

% Generic function to consistently format the output for the logging functions
#(define (oll-format-log fmt vals)
   (apply format (format "\n\n~a\n" fmt) vals))

% Critical error
% Aborts the compilation of the input file
% so use with care!
#(define (oll:error fmt . vals)
   (if (oll:do-log 'critical)
       (begin
        (oll:log-to-file "Error" fmt vals)
        (ly:input-message (*location*)
         (format "Error:~a" (oll-format-log fmt vals)))
        (ly:error ""))))

% Warning
#(define (oll:warn fmt . vals)
   (if (oll:do-log 'warning)
       (begin
        (oll:log-to-file "Warning" fmt vals)
        (ly:input-warning (*location*)
           (oll-format-log fmt vals)))))

% General logging
#(define (oll:log fmt . vals)
   (if (oll:do-log 'log)
       (begin
        (oll:log-to-file "Event" fmt vals)
        (ly:input-message (*location*)
           (oll-format-log fmt vals)))))

% Debug output
#(define (oll:debug fmt . vals)
   (if (oll:do-log 'debug)
       (begin
        (oll:log-to-file "Debug info" fmt vals)
        (ly:input-message (*location*)
          (oll-format-log fmt vals)))))
