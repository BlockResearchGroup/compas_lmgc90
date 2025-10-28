#!/usr/bin/env python
import sys, math

ifile1 = open( 'POSTPRO/Dep_00002.DAT','r')
ifile2 = open( 'POSTPRO/Fint_00002.DAT','r')
ofilen = open('fn_un_M2.txt','w')


u2 = []
for line in ifile1:
    line = line.replace('D','E')
    pair = line.split()
    uz = float(pair[3])
    u2.append(float(uz))

f2 = []
for line in ifile2:
    line = line.replace('D','E')
    pair = line.split()
    fz = float(pair[6])
    f2.append(float(fz))


for i in range(0, len(u2)-1, 1):
   try:
     ofilen.write('%12.5e %12.5e\n' % (-(u2[i]*1.e6)/(2*0.87*math.cos((math.pi/2) - math.atan(0.83/0.87))),-(f2[i]*1.e-3)))

   except:
     pass
 
ifile1.close();ifile2.close();ofilen.close()

