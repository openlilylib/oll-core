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
#(define oll-loglevel 0)

% Set the log level. oll-core's oll: logging functions won't do anything
% if their log level is lower than the currently set level.
% <level> has to be one of the symbols used in 'oll-loglevels'
setLoglevel =
#(define-void-function (level)(symbol?)
   (let ((new-level (getAtree #t 'oll-loglevels (list level))))
     (if new-level
         (set! oll-loglevel (cdr new-level))
         ;
         ; TODO:
         ; Change to oll:warn
         ;
         (ly:input-warning
          (*location*) "Not a valid openLilyLib log level: ~a. Ignoring" level))))

% Open log file
#(define oll-logfile
   (open-output-file
    (format "~a.oll.log" (ly:parser-output-name (*parser*)))))


%{

% Different logging levels can be output.
% Can be used with or without location argument

% Critical error
#(define (oll:error location fmt . vals)
   (if (>= #{ \getOption global.loglevel #} oll-loglevel-critical)
       (begin
        ;; open logfile upon first request
        #{ \openLogfile #}
        (if (ly:input-location? location)
            (begin
             ;; console output
             (ly:error location
               (format
                (string-append "openLilyLib: " fmt) vals))
             ;; logfile output
             (format oll-logfile fmt vals))
            (begin
             ;; this is an "abuse" of the parameters,
             ;; "location" is actually the "fmt" argument
             (ly:error
              (format
               (string-append "openLilyLib: " location) fmt))
             (format oll-logfile
               (format "error: ~a\n" location) fmt))))))

% Warning
#(define (oll:warn location fmt . vals)
   (if (>= #{ \getOption global.loglevel #}  oll-loglevel-warning)
       (begin
        #{ \openLogfile #}
        (if (ly:input-location? location)
            (begin
             (ly:input-warning location
              (format
               (string-append "openLilyLib: " fmt) vals))
             (format oll-logfile fmt vals))
            (begin
             (ly:warning
              (format
               (string-append "openLilyLib: " location) fmt))
             (format oll-logfile
               (format "warning: ~a\n" location) fmt))))))

% Logging
#(define (oll:log location fmt . vals)
   (if (>= #{ \getOption global.loglevel #}  oll-loglevel-log)
       (begin
        #{ \openLogfile #}
        (if (ly:input-location? location)
            (begin
             (ly:input-message location
               (format
                (string-append "openLilyLib: " fmt) vals))
             (format oll-logfile fmt vals))
            (begin
             (ly:message
              (format
               (string-append "openLilyLib: " location) fmt))
             (format oll-logfile
               (format "log: ~a\n" location) fmt))))))

% Debug output
#(define (oll:debug location fmt . vals)
   (if (>= #{ \getOption global.loglevel #}  oll-loglevel-debug)
       (begin
        #{ \openLogfile #}
        (if (ly:input-location? location)
            (begin
             (ly:input-message location
               (format
                (string-append "openLilyLib: " fmt) vals))
             (format oll-logfile fmt vals))
            (begin
             (ly:message
              (format
               (string-append "openLilyLib: " location) fmt))
             (format oll-logfile
               (format "log: ~a\n" location) fmt))))))

%}