#!/usr/bin/env python
from __future__ import print_function
import sys, math

dfile  = open( './POSTPRO/Dep_0000001.DAT','r')
ffile = open( './POSTPRO/Fint_0000001.DAT','r')

ofile = open('f_u.txt','w')

t = []; fx = []
for k,line in enumerate(ffile):
    line = line.replace('D','E')
    pair = line.split()
    t.append(float(pair[0]))
    fx.append(float(pair[4]))

t = []; dx = []
for k,line in enumerate(dfile):
    line = line.replace('D','E')
    pair = line.split()
    t.append(float(pair[0]))
    dx.append(float(pair[1]))

for j in range(0, len(t)-1, 1):
    try:
      ofile.write('%12.5e %12.5e %12.5e \n' % (dx[j],fx[j],t[j]))
    except:
      pass

dfile.close();ofile.close();ffile.close()

