#!/usr/bin/env python3
#
# VBCL parser
#
# Andrew Bernard 2017

import re

# compile the match patterns
comment = re.compile(r"^#")
nv_pair = re.compile(r"^(.*): (.*)$")
long_text_start = re.compile(r"^(.*): <")
long_text_end = re.compile(r"^  >")
list_items_start = re.compile(r"^(.*): \[")
list_items_end = re.compile(r"^  \]")

defaults = {
    'name': 'NN',
    'display-name': 'NN',
    'short-description': 'No short description available',
    'description': 'No description available',
    'dependencies': [],
    'oll-core': '0.0.0',
    'maintainers': [],
    'version': '0.0.0',
    'license': 'None'
}

def set_defaults(d):
    """Ensure mandatory properties are set to 'empty' values."""
    for key in defaults:
        d[key] = d.get(key, defaults[key])
    return d

def parse(lines):
    """Returns a dictionary corresponding to a parsed VBCL string list."""
    d = {}
    i = -1
    line_count = len(lines)

    while i < line_count - 1:
        i += 1

        # comments - discard
        if comment.search(lines[i]):
            continue
        else:
            # long text
            m = long_text_start.search(lines[i])
            if m:
                text = []
                i += 1
                while not (i == line_count or  long_text_end.search(lines[i])):
                    text.append(lines[i].strip())
                    i += 1
                d[m.group(1)] = ' '.join(text)
                continue
            else:
                # list
                m = list_items_start.search(lines[i])
                if m:
                    items = []
                    i += 1
                    while not (i == line_count or list_items_end.search(lines[i])):
                        items.append(lines[i].strip())
                        i += 1
                    d[m.group(1)] = items
                    continue
                else:
                    # name value pair
                    m = nv_pair.search(lines[i])
                    if m:
                        d[m.group(1).strip()] = m.group(2).strip()
    cfg = set_defaults(d)
    return cfg
    
def parse_file(filename):
    """Returns a dictionary corresponding to a parsed VBCL config file."""

    with open(filename) as f:
        cfg_dict = parse(f.read().split('\n'))
        return cfg_dict
    f.close()
            
     
