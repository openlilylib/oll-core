\version "2.24.0"

% Common overrides to define the appearance of a custos
custosOverrides = {
  \once \override NoteHead.stencil = #ly:text-interface::print
  \once \override NoteHead.text =
  #(markup #:fontsize 3.5 #:musicglyph "custodes.mensural.u0")
  \once \omit Stem
  \once \omit Flag
  \once \omit Dots

  % Make sure the note column has exactly the same width as the glyph
  % (the value '4' has been determined by trial and error)
  \once \override NoteColumn.before-line-breaking =
  #(lambda (grob) (ly:grob-set-property! grob 'X-extent '(0 . 4)))
}

% Create a manual pitched "custos" glyph
custos =
#(define-music-function (pitch)(ly:pitch?)
   #{
     \custosOverrides
     % return a "note"
     % In case the example continues after the custos it is relevant
     % that the "musical" duration is 1/32 - which is done in order to
     % have a very short duration because it seems only possible to
     % *widen* the note column to a given width
     #(make-music
       'NoteEvent
       'duration
       (ly:make-duration 5)
       'pitch pitch)
   #})

