#!/usr/bin/env python
import sys, math

ifile2 = open( './POSTPRO/BODY_0000002.DAT','r')
ifile1 = open( './POSTPRO/REAC_0000002.DAT','r')

ofile = open('xx.txt','w')

t = [] ; fz = []
for line in ifile1:
    line = line.replace('D','E')
    pair = line.split()
    t.append(float(pair[0]));fz.append(float(pair[3]))

t = [] ; uz = []
for line in ifile2:
    line = line.replace('D','E')
    pair = line.split()
    t.append(float(pair[0]));uz.append(float(pair[6]))

for i in range(0, len(t), 1):
   ofile.write('%12.5e %12.5e %12.5e\n' % (t[i],uz[i],fz[i]))

ifile1.close();ifile2.close();ofile.close()

