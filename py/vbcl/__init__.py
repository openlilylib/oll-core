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


def parse(lines):
    """Returns a dictionary corresponding to a parsed VBCL string list."""
    d = {}
    i = -1
    k = -1
    
    for line in lines:
        i += 1
        if k > i:
            continue

        # comments - discard
        if comment.search(line):
            continue
        else:
            # long text
            m = long_text_start.search(line)
            if m:
                j = -1
                text = str()
                for line in lines[i+1:]:
                    j += 1
                    if long_text_end.search(line):
                        d[m.group(1)] = text.strip('\n')
                        k = i + j
                        break
                    text += line.strip(' ') + ' ' # add only a single space
                continue
            else:
                # list
                m = list_items_start.search(line)
                if m:
                    j = -1
                    items = list()
                    for line in lines[i+1:]:
                        j += 1
                        if list_items_end.search(line):
                            d[m.group(1)] = items
                            k = i + j
                            break
                        items.append(line.strip(' \n'))
                    continue
                else:
                    # name value pair
                    m = nv_pair.search(line)
                    if m:
                        d[m.group(1).strip()] = m.group(2).strip()
    return d
    
def parse_file(filename):
    """Returns a dictionary corresponding to a parsed VBCL config file."""

    with open(filename) as f:
        return parse(f.read().split('\n'))
    f.close()
            
     
