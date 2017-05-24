#!/usr/bin/env python3
#
# VBCL parser
#
# Andrew Bernard 2017

import re


def parse_file(file):
    """Returns a dictionary corresponding to a parsed VBCL config file."""

    d = dict()
    
    # compile the match patterns
    comment = re.compile(r"^#")
    nv_pair = re.compile(r"^(.*): (.*)$")
    long_text_start = re.compile(r"^(.*): <")
    long_text_end = re.compile(r"^  >")
    list_items_start = re.compile(r"^(.*): \[")
    list_items_end = re.compile(r"^  \]")
    
    with open(file) as f:
        for line in f:
            
            # comments - discard
            if comment.search(line):
                continue
            else:
                # long text
                m = long_text_start.search(line)
                if m:
                    text = str()
                    for line in f:
                        if long_text_end.search(line):
                            d[m.group(1)] = text.strip('\n')
                            break
                        text += line.strip(' ')
                    continue
                else:
                    # list
                    m = list_items_start.search(line)
                    if m:
                        items = list()
                        for line in f:
                            if list_items_end.search(line):
                                d[m.group(1)] = items
                                break
                            items.append(line.strip(' \n'))
                        continue
                    else:
                        # name value pair
                        m = nv_pair.search(line)
                        if m:
                            d[m.group(1).strip()] = m.group(2).strip()
            
        f.close()
        return d
     
