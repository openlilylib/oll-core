\version "2.20.0"

% Test file for preset settings

\include "oll-core/package.ily"

\definePropertySet test.presets
#`((color ,color? ,red)
   (direction ,ly:dir? ,UP))

test =
#(with-property-set define-music-function (mus)(ly:music?)
   `(test presets)
   (let*
    ((text (or (property 'preset) 'none))
     ;; use (use-preset) to determine the "active" state of the function
     (color (if (use-preset) (property 'color) black))
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
    ((use (use-preset))
     (color (if use (property 'color) black)))
    #{
      \temporary \override NoteHead.color = #color
      #mus
      \revert NoteHead.color
    #}))

% Presets for the text function

\definePreset \with {
  color = #green
  direction = #DOWN
} test.presets one

\definePreset \with {
  color = #blue
  direction = #UP
} test.presets two

\definePreset \with {
  color = #magenta
  direction = #DOWN
} test.presets three

\definePreset \with {
  color = #yellow
  direction = #UP
} test.presets four

\definePreset \with {
  color = #darkgreen
  direction = #DOWN
} test.presets five

% Presets for the inner coloring function

\definePreset \with {
  color = #green
} test.colors one

\definePreset \with {
  color = #blue
} test.colors two

\definePreset \with {
  color = #magenta
} test.colors three

\definePreset \with {
  color = #yellow
} test.colors four

\definePreset \with {
  color = #darkgreen
} test.colors five


% Test different filter settings
% \setGlobalPresetSettings affects *all* functions with the named preset
% \setPresetSettings affects only the specified property set.


% require-preset
% if ##t only functions with a given preset are used

%\setGlobalPresetSettings require-preset ##t
%\setPresetSettings test.presets require-preset ##t
%\setPresetSettings test.colors require-preset ##t


% use-only-presets
% only presets given in the lists are used.
% global and local filters add up the restrictions, possibly resulting in
% *no* presets being used.
% If require-preset = ##t this amounts to "only use functions
% where this preset has been set".

%\setGlobalPresetSettings use-only-presets one.two
%\setPresetSettings test.presets use-only-presets three.four
%\setPresetSettings test.colors use-only-presets two.five


% ignore-presets
% Don't use presets within the list. Functions without preset
% are *not* affected by this.
% Global and local lists add up.

%\setGlobalPresetSettings ignore-presets one.two
%\setPresetSettings test.presets ignore-presets three.four
%\setPresetSettings test.colors ignore-presets two.five


content = {
  \test \testColor b2
  \test \with { preset = one   } \testColor \with { preset = one   } c'2
  \test \with { preset = two   } \testColor \with { preset = two   } d'
  \test \with { preset = three } \testColor \with { preset = three } e'
  \test \with { preset = four  } \testColor \with { preset = four  } f'
  \test \with { preset = five  } \testColor \with { preset = five  } g'
}

\new Staff \content
