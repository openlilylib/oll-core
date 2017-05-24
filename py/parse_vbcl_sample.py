#!/usr/bin/env python3
#
# VBCL parser
#
# Andrew Bernard 2017

import re


def parse_vbcl_config(file):
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
        
def main():
    d = parse_vbcl_config("package.cnf")
    print('config dictionary')
    print('=========================================')
    print(d)
    print('\n')
    print('config:')
    print('=========================================')
    print('name:', d['name'])
    print('display name:', d['display-name'])
    print('short description:')
    print(d['short-description'], '\n')
    print('version:', d['version'])
    print('description:')
    print(d['description'], '\n')
    print('dependencies:')
    list_display(d['dependencies'])
    print('oll core:', d['oll-core'])
    print('maintainers:')
    (list_display(d['maintainers']))
    print('version:', d['version'])
    print('license:', d['license'])
    print('repository:', d['repository'])

def list_display(l):
    """DIsplay list items."""
    for item in l:
        print('- ', item)

if __name__ == "__main__":
    import sys
    main()
    
