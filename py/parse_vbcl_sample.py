#!/usr/bin/env python3
#
# VBCL parser demonstration file
#
# Andrew Bernard 2017

import vbcl

        
def main():
    d = vbcl.parse_file("package.cnf")
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
    
