\version "2.19.22"

% The following is temporary, and the oll-core directory must be in the include path.
\include "oll-core.ily"

% Little helper function
show =
#(define-void-function (fstring obj)(string? scheme?)
   (ly:message (format "\n~a:\n~a\n" fstring obj)))

%%%%%%%%%%%%%%%%
% A) flat a-lists

% Create an empty alist with the given name
\newAlist tags
\show "Initialized alist" #tags

% Set two key-value pairs
\setAlist tags instrumentName "Trumpet (b)"
\setAlist tags midiInstrument "trumpet"
\show "Two initial entries" #tags

% Replace entry
\setAlist tags instrumentName "Flute"
\show "Replaced in-place" #tags

% If key isn't present the entry is appended
\setAlist tags shortInstrumentName "Fl"
\show "Implicitly appended" #tags

% Append new entry
\addToAlist tags performer "John Doe"
\show "Appended entry" #tags

% If entry is already present it is moved to the end
\addToAlist tags midiInstrument "trombone"
\show "Implicitly moved to end" #tags

% Remove an entry. If it isn't present nothing happens
\removeFromAlist tags midiInstrument
\show "Removed entry midiInstrument" #tags

% Add to non-present alist causes a warning and a new
% alist to be created implicitly (usually not intended)
% (same with setAlist)
\addToAlist taggy count "2 players"
\show "Implicitly created alist" #taggy

% Reset alist
\newAlist tags
\show "Reset alist" #tags

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B) Nested a-lists or a-trees

#(ly:message "\n\nWorking with nested alists\n\n")


% Create a new a-tree (equivalent to \newAlist)
\newAtree opts

% set entries at arbitrary positions.
% Intermediate nodes are created as necessary
\setAtree opts staffs.trumpet.clef "treble"
\setAtree opts staffs.trumpet.name "Trompete"
\setAtree opts staffs.piano.upper.clef "treble"
\setAtree opts staffs.piano.lower.clef "bass"
\setAtree opts staffs.trumpet.key "B flat"

\show "Atree" #opts

% Implicitly move entry to end of list
% TODO: This doesn't seem to work ...
\addAtree opts staffs.trumpet.clef "bass"
\show "Move entry" #opts

% Remove an entry
\remAtree opts staffs.trumpet.clef
\show "Removed entry 'trumpet.clef'" #opts
