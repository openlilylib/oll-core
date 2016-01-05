% This is the main entry file for openLilyLib.
% When this is included different libraries and their modules can be loaded using
% the \import command defined here.

% Including this file also makes available some general helper material that
% is therefore automatically available for any contained library, for example the
% LilyPond version predicates that can be used to create code depending on the
% LilyPond version currently executed

% Include (once) the basic infrastructure of openLilyLib
%
% This does several things:
% - define a global variable 'openlilylib-root
%   which is the absolute path to the root of openLilyLib
%   (the folder this file is located in)
%   This can be used to construct paths to locations in the
%   libraries that are relative to openlilylib-root
% - add this directory to Scheme's module path. Scheme modules
%   can now be created and references from this root too.
% - Add module handling support (\loadModule and friends)
% - Add general tools that are available for all libraries.
%   - lilypond-version-predicates
%   - logging commands
%


% Make general openLilyLib utilities available to any library.
% See TODO: DOC for more information
% This file is part of the openLilyLib library infrastructure
% ... TOBEDONE ...
%
% This file initializes openLilyLib

#(ly:set-option 'relative-includes #t)

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Common functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Add directoires to Guile's module loading path.
\include "general-tools/scheme-wrapper/add-guile-path/definitions.ily"
% the ly directory is now an included path from which modules can be addressed
% Add openLilyLib root directory to Guile's module load path
\addGuilePath ".."
% TODO: Check this when the Scheme lib has moved
% This is only needed to possibly access modules in the "old" places
\addGuilePath "../.."

% Make common functionality available to all openLilyLib "users"
\include "utilities/__main__.ily"

% Logging capabilities with different log levels
\include "logging.ily"

% Common option handling
\include "options.ily"

% Set default loglevel to 'warning'
% (can only be done after options have been included)
\registerOption global.loglevel #oll-loglevel-warning

% Utility to include multiple files at once
% Depends on "options.ily"
\include "utilities/include-pattern.ily"

% Set the root path of openLilyLib
% - for oll module inclusion
% - for Scheme module inclusion
setRootPath =
#(define-void-function (parser location)()
   (let* ((path
           (normalize-path
            (string-append
             (location-extract-path location)
             "/.."))))
     #{ \registerOption global.root-path #path #}))
\setRootPath

% Functionality to load and manage modules
\include "module-handling.ily"

% Welcome message.
% This is a default ly:message because otherwise we'd have to mess around with
% loglevels. This shouldn't be logged anyway.

#(ly:message "\nopenLilyLib: library infrastructure successfully loaded.\n\n")
