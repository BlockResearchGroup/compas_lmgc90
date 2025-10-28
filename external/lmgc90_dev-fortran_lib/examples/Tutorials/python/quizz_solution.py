
# This is A solution... not THE solution

# Basic

# generate inputs
n = 120
a = 1
b = 12

# random list, may also check random.sample function
import random
gl = [ random.randint(a,b) for i in range(n) ]

# extract the list of generated value (without repetition)
print( set(gl) )
# and count them
print( len(set(gl)) )

# generate the map associating for each unique random number, the number of occurences
from collections import defaultdict
d = defaultdict(lambda : 0)
for g in gl:
  d[g] += 1

# a rough reverse map, bewar if values are not unique
r = { v:k for k,v in d.items() }

# compute a medium value
s = ( max(d.values()) + min(d.values()) ) // 2

# count the number unique values with occurences inferior to medium value
c = 0
for v in d.values():
  c = c+1 if v < s else c


# Numpy

import numpy as np

n  = 12

ra = 1.
rb = 2.
assert ra < rb, f"ra ({ra}) must be inferio to rb ({rb})"

cmin = 0.
cmax = 5.
assert cmin < cmax, f"cmin ({cmin}) must be inferio to cmax ({cmax})"

radii = np.random.rand(12)   * ( rb - ra ) +  ra
coor  = np.random.rand(12,3) * (cmax-cmin) + cmin

bound_min = np.amin( coor-radii[:,np.newaxis], axis=0 )
bound_max = np.amax( coor+radii[:,np.newaxis], axis=0 )

def in_circle(center, radius, co):
  return np.linalg.norm( co-center ) < radius

circle_c = (bound_min+bound_max) / 2.
circle_r = np.linalg.norm( (bound_max-bound_min) / 5. )
print( 'checking circle : ', circle_c, circle_r )

for c in coor:
  print( in_circle(circle_c, circle_r, c) )


c_mask = np.linalg.norm( coor-circle_c, axis=1 ) < circle_r
print('mask : ', c_mask)
c_idx  = c_mask.nonzero()
print('indices : ', c_idx)

print('coordinate:')
print(coor[c_idx])

rot = np.zeros( [3,3] )
rot[0,0] = rot[1,2] = rot[2,1] = 1.

print('permutation:')
print( np.matmul(rot,coor.T).T )
# in this case no difference with
print( np.matmul(coor,rot) )
