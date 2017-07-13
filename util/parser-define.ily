\version "2.17.96"

\header {
  snippet-title = "Define variables anywhere"
  snippet-author = "Jan-Peter Voigt"
  snippet-description = \markup {
  }
  % add comma-separated tags to make searching more effective:
  tags = "Program flow, LilyPond variables"
  % is this snippet ready?  See meta/status-values.md
  status = "ready"
}

%%%%%%%%%%%%%%%%%%%%%%%%%%
% here goes the snippet: %
%%%%%%%%%%%%%%%%%%%%%%%%%%

parserDefine =
#(define-void-function (name val)(symbol? scheme?)
    (ly:parser-define! name val))
