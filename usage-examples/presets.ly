\version "2.20.0"

% Test file for configuration filters

\include "oll-core/package.ily"

\definePropertySet test.configurations
#`((color ,color? ,red)
   (direction ,ly:dir? ,UP))

test =
#(with-property-set define-music-function (mus)(ly:music?)
   `(test configurations)
   (let*
    ((text (or (property 'configuration) 'none))
     ;; use (use-configuration) to determine the "active" state of the function
     (color (if (use-configuration) (property 'color) black))
     (direction (property 'direction)))
   #{
     \once \override Stem.direction = #direction
     \once \override Stem.color = #color
     a'2
     \once \override Score.RehearsalMark.color = #color
     \mark \markup #(symbol->string text)
     #mus
   #}))

\definePropertySet test.colors
#`((color ,color? ,red))

testColor =
#(with-property-set define-music-function (mus)(ly:music?)
   `(test colors)
   (let*
    ((use (use-configuration))
     (color (if use (property 'color) black)))
    #{
      \temporary \override NoteHead.color = #color
      #mus
      \revert NoteHead.color
    #}))

% Presets for the text function

\definePropertyConfiguration \with {
  color = #green
  direction = #DOWN
} test.configurations one

\definePropertyConfiguration \with {
  color = #blue
  direction = #UP
} test.configurations two

\definePropertyConfiguration \with {
  color = #magenta
  direction = #DOWN
} test.configurations three

\definePropertyConfiguration \with {
  color = #yellow
  direction = #UP
} test.configurations four

\definePropertyConfiguration \with {
  color = #darkgreen
  direction = #DOWN
} test.configurations five

% Presets for the inner coloring function

\definePropertyConfiguration \with {
  color = #green
} test.colors one

\definePropertyConfiguration \with {
  color = #blue
} test.colors two

\definePropertyConfiguration \with {
  color = #magenta
} test.colors three

\definePropertyConfiguration \with {
  color = #yellow
} test.colors four

\definePropertyConfiguration \with {
  color = #darkgreen
} test.colors five


% Test different filter settings
% \setGlobalPresetFilters affects *all* functions with the named configuration
% \setPresetFilters affects only the specified property set.


% require-configuration
% if ##t only functions with a given configuration are used

%\setGlobalPresetFilters require-configuration ##t
%\setPresetFilters test.configurations require-configuration ##t
%\setPresetFilters test.colors require-configuration ##t


% use-only-configurations
% only configurations given in the lists are used.
% global and local filters add up the restrictions, possibly resulting in
% *no* configurations being used.
% If require-configuration = ##t this amounts to "only use functions
% where this configuration has been set".

% ignore-configurations
% Don't use configurations within the list. Functions without configuration
% are *not* affected by this.
% Global and local lists add up.


content = {
  \test \testColor b2
  \test \with { configuration = one   } \testColor \with { configuration = one   } c'2
  \test \with { configuration = two   } \testColor \with { configuration = two   } d'
  \test \with { configuration = three } \testColor \with { configuration = three } e'
  \test \with { configuration = four  } \testColor \with { configuration = four  } f'
  \test \with { configuration = five  } \testColor \with { configuration = five  } g'
}

\new Staff \content
