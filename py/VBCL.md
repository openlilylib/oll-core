

# VBCL - The Very Basic Configuration Language

VBCL has been created to use as a configuration file language with a high degree of simplicity. It supports name value pairs at the same time as supporting list and long text description types. By providing the ability to specify types, VBCL is a step above INI files and other simple configuration file formats. But VBCL is not intended to be as sophisticated as JSON or YAML. The reason for creating yet another configuration file format is to have something that can be very easily parsed with Scheme as provided by Guile 1.8, for lilypond, and equally simply parsed with Python 3, for Frescobaldi, for example. This file format can be trivially parsed with simple regular expression parsers where more advanced parsing machinery is lacking (in Guile 1.8 in particular).

### Format

#### Comments

#### Name Value Pairs

#### Lists

#### Long Text Lines

name value pairs, arbitrary text including whitespace, single line.

n: v

long text, text lines to be started with whitespace indent, e.g. two spaces.
terminate with '  >'

name: <
 long text over as many lines as needed
 ...
 >

comments.

lists: any number of lines, two spaces at start of each item line.
terminate with '  ]'.

name: [
 item1
 item2
 ]

lines starting with are ignored

blank lines are ignored

Do not forget there needs to be a newline at the end of the file.
