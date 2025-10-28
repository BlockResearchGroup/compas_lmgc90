import math

from pylmgc90.pre import *

# first generate lots of random polyhedra:
# - generate the first half of them with radius 1., the second with radius rad
# - position of center is in a cubic box, and deposited on a cubic lattice

dim = 3
nb_each = 7
min_vert = 18
max_vert = 50

rad=1.5

bodies = avatars()
maters = materials()

mod = model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)

mater = material(name='PLEXx', materialType='RIGID', density=100.)
maters.addMaterial(mater)

#generating a square lattice of square shape
nb_part = (max_vert-min_vert+1)*nb_each
nb_box_ele = int( math.ceil(nb_part**(1./3.)) )
coors = cubicLattice3D(nb_box_ele, nb_box_ele, nb_box_ele, 2.*max(1.,rad))
coors.shape=[len(coors)/3,3]

#first half of particles with radius of 1.
for i in range(0,nb_part/2):
  body = rigidPolyhedron(mod, mater, center=coors[i], generation_type='random', nb_vertices=min_vert+i/nb_each)
  bodies += body

#second half of particles with radius of 1.5
for i in range(nb_part/2,nb_part):
  body = rigidPolyhedron(mod, mater, generation_type='random', nb_vertices=min_vert+i/nb_each, radius=rad, tol=0.1)
  body.translate(dx=coors[i][0],dy=coors[i][1],dz=coors[i][2])
  bodies += body

# sometimes...
try:
  visuAvatars(bodies)
except:
  pass

# writing files but only bodies (no contacts)
import os
if not os.path.isdir('DATBOX'):
  os.mkdir('DATBOX')
writeBodies(bodies, chemin='DATBOX/')
writeDrvDof(bodies, chemin='DATBOX/')
writeDofIni(bodies, chemin='DATBOX/')
writeBulkBehav(maters, chemin='DATBOX/', dim=dim)

# now trying to generate paraview outfile
from pylmgc90 import chipy
chipy.checkDirectories()
chipy.Initialize()
chipy.SetDimension(3)
chipy.ReadBehaviours()
chipy.ReadBodies()
chipy.LoadBehaviours()
chipy.ReadIniDof()
chipy.ReadDrivenDof()
chipy.LoadTactors()

chipy.OpenDisplayFiles()
chipy.WriteDisplayFiles()
chipy.CloseDisplayFiles()

chipy.Finalize()
