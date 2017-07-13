\version "2.19.22"

\include "oll-core/package.ily"
\loadModule oll-core.util.include-pattern

% show all includes
\setLogLevel #'log
\setOption oll-core.include-pattern.log-includes ##t

% Comment out *one* of the following definitions
pattern = ".*\\.i?ly"
%pattern = "f[13]\\.i?ly"

% include all files with pattern (regular expression)
% the file f1.ly to f3.ly simply display a string
% Arguments:
% #1: directory,
%     - absolute or
%     - relative to the location of the file where it is used
% #2: pattern matching files in that directory
\includePattern "include-pattern-examples" \pattern
