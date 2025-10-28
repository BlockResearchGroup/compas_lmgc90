#!/usr/bin/env python
import sys, math

ifile2 = open( './POSTPRO/BODY_0000002.DAT','r')
ifile1 = open( './POSTPRO/REAC_0000002.DAT','r')

ofile = open('xx.txt','w')

t = [] ; fx = []
for line in ifile1:
    line = line.replace('D','E')
    pair = line.split()
    t.append(float(pair[0]));fx.append(float(pair[1]))

t = [] ; ux = []
for line in ifile2:
    line = line.replace('D','E')
    pair = line.split()
    t.append(float(pair[0]));ux.append(float(pair[4]))

for i in range(0, len(t), 1):
   ofile.write('%12.5e %12.5e %12.5e\n' % (t[i],ux[i],fx[i]))

ifile1.close();ifile2.close();ofile.close()

