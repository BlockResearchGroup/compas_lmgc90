from __future__ import print_function

import os, sys
import subprocess

# open the file which will contain the documentation
doc = open('docs/chipy_swig_docstrings.i','w')

# for all files in doc/xml
files = os.listdir('./docs/xml')
for file in files:
  (base, ext) = os.path.splitext(file)
  # for all wrap*.xml files
  if ext=='.xml' and base[0:4]=='wrap':
    # convert it in a .i file using doxy2swig.py script
    retcode = subprocess.call([sys.executable, 'doxy2swig.py', './docs/xml/'+file, './docs/'+base+'.i'])
    # add the content of newly created .i file to chipy_swig_docstrings.i
    new = open('docs/'+base+'.i')
    for line in new:
      doc.write(line)
    new.close()
 
doc.close()

# ugly regex to remove some strange break line in arguments definition
#os.system("sed -i '' -e ':a' -e 'N' -e '$!ba' -e 's/\* \`\([[:print:]]*\)\` :[[:space:]]*/\\1 /g' docs/chipy_swig_docstrings.i")

import fileinput, re

fname = 'docs/chipy_swig_docstrings.i'

#  look for line which could look like
# * `toto (double*[4])` : [some spaces] *
# and replace it by only the text between the ` `
# remove the carriage return and the starting
# spaces of the next line

pattern = re.compile(r'\* `([(\w)\[\]\* ]*)` :\s*\n')
f = fileinput.input(fname,inplace=1)
for l in f:
  nl = pattern.sub(r'\g<1>', l)
  if nl != l:
    print(nl,end='')
    l = f.readline()
    nl = re.sub(r'^\s*', '', l)

  print(nl,end='')

