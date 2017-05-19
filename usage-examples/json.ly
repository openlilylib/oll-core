\version "2.19.60"

\include "oll-core/package.ily"

% Try loading the json module.
% The include itself does work (as can be seen from
% #(use-modules (oll-core scheme tree))
% ), so any errors are Guile incompatibilities with the json module.

#(use-modules (oll-core scheme json))