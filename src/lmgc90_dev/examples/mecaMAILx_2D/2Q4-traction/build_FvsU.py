#!/usr/bin/env python
import sys, math

ifile1 = open( 'POSTPRO/Dep_00001.DAT','r')
ifile2 = open( 'POSTPRO/Dep_00002.DAT','r')
ifile3 = open( 'POSTPRO/Fint_00002.DAT','r')
ofilen = open('fn_un.txt','w')


u1 = []
for line in ifile1:
    line = line.replace('D','E')
    pair = line.split()
    uy = float(pair[2])
    u1.append(float(uy))

u2 = []
for line in ifile2:
    line = line.replace('D','E')
    pair = line.split()
    uy = float(pair[2])
    u2.append(float(uy))

f2 = []
for line in ifile3:
    line = line.replace('D','E')
    pair = line.split()
    fy = float(pair[2])
    f2.append(float(fy))



for i in range(0, len(u1), 1):
   #if math.fabs(f2[i]) < 4. and i > 100 :
   if math.fabs(u2[i]-u1[i]) > 1.e-6:
     break
   ofilen.write('%12.5e %12.5e\n' % (u2[i]-u1[i],-f2[i]))

ifile1.close();ifile2.close();ifile3.close(); ofilen.close()

