# oll-core
Library Infrastructure for LilyPond add-ons

`oll-core` is the heart of [openLilyLib](https://openlilylib.org) and provides common
functionality that any ”openLilyLib” package uses.  However, it is also useful for
inclusion in arbitrary LilyPond files.

*Note: this code (and documentation) is currently in a conceptual state of pre-alpha
quality.*

### Overview for files using oll-core directly

This is only a short outline of the functionality that is automatically provided as
soon as `oll-core` is loaded (implicitly or explicitly).

#### Option handling

openLilyLib packages support configuration with the `\setOption` command. Users can
make use of that mechanism as well to make their files easily configurable.

#### Logging

A number of logging commands can be used in conjunction with a settable log level.

#### Miscellaneous internals

##### LilyPond version predicates

`oll-core` provides predicates to match against the currently running LilyPond version.
This makes it possible to write code that supports multiple LilyPond versions, which
can be particularly appropriate for libraries.

##### Simplified and extended access to association lists

`oll-core` provides functions to get and set values from association lists, with
specific concern of *nested* lists or “trees”.

##### OS independent path handling

The `os-path` module (naming inspired by Python) provides a number of commands that simplify
dealing with paths and file names.

