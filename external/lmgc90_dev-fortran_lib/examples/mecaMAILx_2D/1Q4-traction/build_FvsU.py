#!/usr/bin/env python
import sys, math

ifile1 = open( 'POSTPRO/Dep_00001.DAT','r')
ifile2 = open( 'POSTPRO/Fint_00001.DAT','r')
ofilen = open('fy_uy.txt','w')


u1 = []
for line in ifile1:
    line = line.replace('D','E')
    pair = line.split()
    uy = float(pair[2])
    u1.append(float(uy))

f2 = []
for line in ifile2:
    line = line.replace('D','E')
    pair = line.split()
    fy = float(pair[4])
    f2.append(float(fy))

for i in range(0, len(u1), 1):
   ofilen.write('%12.5e %12.5e\n' % (u1[i],-f2[i]))

ifile1.close();ifile2.close(); ofilen.close()

