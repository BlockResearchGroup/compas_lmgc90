#!/usr/bin/env python
import sys, math

ifile1 = open( './POSTPRO/BODY_0000001.DAT','r')
ifile2 = open( './POSTPRO/BODY_0000005.DAT','r')
ifile3 = open( './POSTPRO/BODY_0000006.DAT','r')
ifile4 = open( './POSTPRO/DTE_0000001_0000006.DAT','r')

ofile = open('xx.txt','w')

t = [] ; u1y = []
for line in ifile1:
    line = line.replace('D','E')
    pair = line.split()
    t.append(float(pair[0]));u1y.append(float(pair[5]))

t = [] ; u5y = []
for line in ifile2:
    line = line.replace('D','E')
    pair = line.split()
    t.append(float(pair[0]));u5y.append(float(pair[5]))

t = [] ; u6y = []
for line in ifile3:
    line = line.replace('D','E')
    pair = line.split()
    t.append(float(pair[0]));u6y.append(float(pair[5]))

t = [] ; Rz = []; Mx = []
for line in ifile4:
    line = line.replace('D','E')
    pair = line.split()
    t.append(float(pair[0]));Rz.append(float(pair[3]));Mx.append(float(pair[4]))

for i in range(0, len(t), 1):
   ofile.write('%12.5e %12.5e %12.5e %12.5e %12.5e\n' % (t[i],u1y[i]-u6y[i],u5y[i]-u1y[i],Rz[i],Mx[i]))

ifile1.close();ifile2.close();ifile3.close();ifile4.close();ofile.close()

