#!/usr/bin/env python
import sys, math

ifile1 = open('./POSTPRO/Dep_00001.DAT','r')
ifile2 = open('./POSTPRO/Fint_00001.DAT','r')
ofile = open('fz_uz.txt','w')

t1 = [] ;u = []
for line in ifile1:
    line = line.replace('D','E')
    pair = line.split()
    t = float(pair[0]); uz = float(pair[3])
    t1.append(t);u.append(uz)

t1 = []; f = []
for line in ifile2:
    line = line.replace('D','E')
    pair = line.split()
    t = float(pair[0]); fz = float(pair[6])
    t1.append(t);f.append(fz)


for i in range(0, len(t1), 1):
    ofile.write('%12.5e %12.5e\n' % (u[i],f[i]))

ifile1.close();ifile2.close();ofile.close()


try:
  import matplotlib.pyplot as pl
  
  pl.figure()
  pl.plot(u,f)
  pl.xlabel('U')
  pl.ylabel('F')
  pl.title('Force/Displacement')
  pl.plot()
  pl.show()
except:
  pass
