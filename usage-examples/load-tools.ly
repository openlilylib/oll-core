\version "2.24.0"

\include "oll-core/package.ily"
\loadModule oll-core.load.tools

% Set the directory in which the tools reside (in this case we need this to
% be relative to the openLilyLib root directory).
\setOption oll-core.load.tools.directory
#(os-path-join (append openlilylib-root '(oll-core usage-examples tools)))

% Load a tool from the tools directory
\loadTool custos

{
  % Use a function defined in the tool
  \custos a'
}
