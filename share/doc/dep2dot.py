#!/usr/bin/env python
# Creates dot from Makefile.depend files. So far only Fortran.
import sys

dot_head = '''
digraph G {
'''
dot_foot = ''' }
'''

try:
    dep_file = str(sys.argv[1])
except IndexError:
    print 'dep2dot [file]'
    sys.exit(2)


def transform(strg):
    strg = strg.replace('-', '_')
    strg = strg.replace('.lo:', ' ->')
    strg = strg.replace('.lo', '')
    strg = strg.replace('../', '')
    strg = strg.replace('/', '_')
    strg = dot_head + strg + dot_foot
    return strg


dot = open(dep_file, "r")
dep_strgs = []
for line in dot.readlines():
    if "_ut" not in line and "/" not in line:
        dep_strgs.append(line)
dep_strgs = list(set(dep_strgs))
if len(dep_strgs) == 0:
    dep_strgs.append('"No internal dependencies"')
dot_strg = transform(''.join(dep_strgs))
print dot_strg
