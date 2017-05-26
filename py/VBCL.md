

# VBCL - The Very Basic Configuration Language

VBCL has been created to use as a configuration file language with a high degree
of simplicity. It supports name value pairs at the same time as supporting list
and long text description types. By providing the ability to specify types, VBCL
is a step above INI files and other simple configuration file formats. But VBCL
is not intended to be as sophisticated as JSON or YAML or even TOML. The reason
for creating yet another configuration file format is to have something that can
be very easily parsed with Scheme as provided by Guile 1.8, for lilypond, and
equally simply parsed with Python 3, for Frescobaldi, for example. This file
format can be trivially parsed with simple regular expression parsers where more
advanced parsing machinery is lacking (in Guile 1.8 in particular). Apart from
the long text and list types, all items in the file are considered to be
strings, so numerical and other types are not supported. It is up to the program
parser to deal with such considerations.

### Format

#### Whitespace
Where whitespace is mentioned, it consists of any number of standard Unicode
whitespace characters, space, tab, and so on. Blank lines are ignored. Do not
forget there needs to be a newline at the end of the file.

#### Comments
Lines beginning with # are discarded. Whitespace can precede the comment.

Block comments are not supported.

> \# comment ...

#### Name Value Pairs
Name value pairs consist of a name followed by a ':' followed by whitespace
followed by a value. Both name and value may be arbitrary text. The pair may
only occupy a single line. There is no line continuation syntax for this type.

>name: value

#### Long Text Lines
Long text lines can be used. The concept is that paragraph style text can be
used and the newlines will be preserved in the parsed content. Such lines use a
start marker '<' after a name, and an end marker '>' preceded by optional
whitespace on a line by itself. Text lines may be indented with optional
whitespace if so desired.

>long-text: <  
Lorem ipsum dolor sit amet, consectetur adipiscing elit.  
Nulla interdum mattis tellus. Donec vehicula eros eget neque   
volutpat consequat. Vestibulum imperdiet non metus mattis  
auctor. Proin vitae neque purus.  
...  
\>

#### Lists
Arbitrary lists can be specified. Note that list items will be parsed as
strings. If numerical types are needed that is up to the program calling the
parser to do. List entries consists of a name followed by a ':' followed by
whitespace followed by the list start marker '['. There is one list item per
line, which may be indented with optional whitespace, which should be stripped
by the parser. The list end marker is ']' on a separate line, preceded by
optional whitespace. List items are text of arbitrary length, each on a single
line.

Nested lists are not supported at this time.

>name: [  
item1  
item2  
...  
]

### Author
Andrew Bernard  
andrew.bernard@gmail.com
