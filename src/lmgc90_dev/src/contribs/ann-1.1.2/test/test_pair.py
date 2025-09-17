import sys, operator
import numpy
from pylmgc90.ann import ann_manh
from pylmgc90.ann import ann_eucl

#set of data points
datap = numpy.array([0., 0.,
                     1., 0.,
                     2., 0.,
                     0., 1.,
                     1., 1.,
                     2., 1.
                    ], dtype=float)
datap.shape=[6,2]

# the point to tests
testp = numpy.array([1.8, 0.8], dtype=float)

# defining a tolerance for tests comparison
tol = 1.e-12

print('set of data points')
print(datap)
print('the test point')
print(testp)

# creating the 2 trees
kdtree  = ann_eucl.kdtree(datap)
kdtree2 = ann_manh.kdtree(datap)

# sizing output array
n_nearest = 4
dd = numpy.zeros([n_nearest],dtype=numpy.int32)
dp = numpy.zeros([n_nearest],dtype=float)

# euclid nearest search test
kdtree.searchNearest(testp,dd,dp)
print('euclid '+str(n_nearest)+' nearest: ', dd, dp)

ref_nearest = { 5:0.08, 4:0.68, 2:0.68, 1:1.28 }
ok = True
for i in range(n_nearest):
  if abs( ref_nearest[dd[i]] - dp[i] ) > tol:
    ok = False
    break

if not ok:
  print('ERROR in testing euclid nearest search')
  sys.exit(1)


# manhattan nearest search test
kdtree2.searchNearest(testp,dd,dp)
print('manhat '+str(n_nearest)+' nearest: ', dd, dp)

ref_nearest = { 5:0.4, 4:1., 2:1., 1:1.6 }
ok = True
for i in range(n_nearest):
  if abs( ref_nearest[dd[i]] - dp[i] ) > tol:
    ok = False
    break

if not ok:
  print('ERROR in testing manhattan nearest search')
  sys.exit(1)


# euclid radius search test
radius = 0.8
nb = kdtree.countAround(testp,radius)
print('number withing circle: ', nb)
kdtree.searchAround(testp, radius, dd[:nb], dp[:nb])
print('which are: ', dd[:nb], dp[:nb])

if nb != 3:
  print('ERROR in testing euclid radius search (wrong number)')
  sys.exit(1)

ref_radius = { 5:0.08, 4:0.68, 2:0.68 }
ok = True
for i in range(nb):
  if abs( ref_radius[dd[i]] - dp[i] ) > tol:
    ok = False
    break
if not ok:
  print('ERROR in testing euclid radius search (wrong values)')
  sys.exit(2)


# manhattan radius search test
nb = kdtree2.countAround(testp,radius)
print('number withing square: ', nb)
kdtree2.searchAround(testp, radius, dd, dp, nb)
print('which are: ', dd, dp)

if nb != 1:
  print('ERROR in testing manhattan radius search (wrong number)')
  sys.exit(1)

ref_radius = { 5:0.4, 4:1., 2:1. }
ok = True
for i in range(nb):
  if abs( ref_radius[dd[i]] - dp[i] ) > tol:
    ok = False
    break
if not ok:
  print('ERROR in testing manhattan radius search (wrong value)')
  sys.exit(2)

kdtree2.searchAround(testp, radius, dd, dp)
print('which are: ', dd, dp)


