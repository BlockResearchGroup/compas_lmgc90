import os,sys

from pylmgc90.chipy import *

##### TP2 : look for spheres on which to add a force  #####

# TODO 1 : write a function to look for particles
#          on which to add force. Its input must
# input : number of layers
# output : a numpy array with 0 on particles without force
#          and 1 on particles with a force
#
# needed functions:
# - RBDY3_GetNbRBDY3
# - SPHER_GetNbSPHER
# - SPHER_GetPtrSPHER2RBDY3
# - np.min, np.max
# - np.empty
# - np.nonzero
# - np.linalg.norm

def particles_with_force(nb_layers):

  # initialize returned array to 0.
 
  # get coordinates and radius of all spheres
  # tip : same thing than in TP1

  # compute the distance to the axis of each particles
  # and store it in a numpy array
  # tip : use np.linalg.norm

  # compute the heigh of the cylinder and the size of each layer
  # tip : same thing than in TP1

  # create a list which for each layer will store the list
  # of particles inside it

  # for each layer:
  # - compute the outer radius of the layer
  # - look for the particles in a crown of inner radius 2*r_max
  # - for those particles store 1. in returned array

  # return array
  return 

checkDirectories()

dt = 1.e-1
nb_steps = 1

theta = 0.5

tol = 0.166e-4
relax = 1.0
norm = 'Quad '
gs_it1 = 33
gs_it2 = 100
storage='Stored_Delassus_Loops         '
nlgs_SetWithQuickScramble()

freq_write   = 10
freq_display = 1
ref_radius   = 1.

confinent_p = 5.e6
nb_layers   = 10

SetDimension(3)

TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

ReadBehaviours()
ReadBodies()
LoadBehaviours()
ReadIniDof()
ReadDrivenDof()

LoadTactors()

ReadIniVlocRloc()

WriteBodies()
WriteBehaviours()
WriteDrivenDof()

OpenDisplayFiles()
OpenPostproFiles()

ComputeMass()

nbs = SPHER_GetNbSPHER()
s2b = SPHER_GetPtrSPHER2RBDY3()
x = np.empty([nbs,4],dtype=np.double)
for i in range(nbs):
  x[i,:3] = RBDY3_GetBodyVector('Coorb',int(s2b[i,0]))[:3]
  x[i,3]  = SPHER_GetContactorRadius(i+1)

h0 = np.min(x[:,2]-x[:,3])
h  = np.max(x[:,2]+x[:,3]) - h0
dh = h / nb_layers

slayerId = np.empty([nbs])
for i in range(nbs):
  slayerId[i] = int((x[i,2]-h0)/dh) + 1

layerID = {'SPHER':slayerId}
layerID['PLANx'] = np.zeros(PLANx_GetNbPLANx())

# TODO 2:
#
# Call your function to get the returned array.
# Add your 'rigid' field to WriteDisplayFiles call.
# To see the result, open the rigids.pvd file in
# paraview and 'clip' it to see only rigid particles
# with 1. on your field
WriteDisplayFiles(freq_display,layerID=('tactor',layerID))

CloseDisplayFiles()
ClosePostproFiles()

