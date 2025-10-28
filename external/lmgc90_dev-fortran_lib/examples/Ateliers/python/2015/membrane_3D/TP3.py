import os,sys

from pylmgc90.chipy import *

##### TP3 : apply pressure to particles  #####

# TODO 1 : modify the function 'particles_with_force'
#          in 'applyPressure' to really add a force
#          to the particles.
# input : number of layers, time step and confinment pressure
# output : a numpy array with 0 on particles without force
#          and 1 on particles with a force
#
# needed functions:
# - RBDY3_PutBodyVector on field 'Fext_'

def applyPressure(dt, nb_layers,pressure):

  has_p = np.zeros(RBDY3_GetNbRBDY3(),dtype=np.double)
 
  nbs = SPHER_GetNbSPHER()
  s2b = SPHER_GetPtrSPHER2RBDY3()
  x = np.empty([nbs,4],dtype=np.double)
  for i in range(nbs):
    x[i,:3] = RBDY3_GetBodyVector('Coorb',int(s2b[i,0]))[:3]
    x[i,3]  = SPHER_GetContactorRadius(i+1)

  r  = np.empty([x.shape[0]])
  for i in range(x.shape[0]):
    r[i]  = np.linalg.norm(x[i,:2])

  h0 = np.min(x[:,2]-x[:,3])
  h  = np.max(x[:,2]+x[:,3]) - h0
  dh = h/nb_layers

  layer = []
  for i in range(nb_layers):
    layer.append([])
  for i in range(s2b.shape[0]):
    layer[int((x[i,2]-h0)/dh)].append(i)

  # Add impulse on each sphere in the crown.
  # The impulse value is a force times a time.
  f_ext = np.zeros([6])

  for k in layer:
    k = np.array(k)
    r_out = np.max( r[k] + x[k,3] )
    dr    = 2.*np.max( x[k,3] )
    sic   = np.nonzero( r[k] > r_out - dr )
    # Compute the equivalent force applied by
    # the confinment pressure on the surface of
    # the cylinder of a layer
    # F = P * 2 * pi * Radius * H
    for i in sic[0]:
      has_p[k[i]] = 1.
      # compute the force to apply to each sphere
      # F_i = radius_i / sum(radius_i)
      # F_i_x = - F * X_x/norm(X)
      # F_i_y = - F * X_y/norm(X)
      # add F_i times time step to external forces

  return has_p

checkDirectories()

dt = 1.e-1
nb_steps = 1000

theta = 0.5

tol = 0.166e-4
relax = 1.0
norm = 'Quad '
gs_it1 = 33
gs_it2 = 100
storage='Stored_Delassus_Loops         '
nlgs_SetWithQuickScramble()

freq_write   = 10
freq_display = 10
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

has_p = applyPressure(dt,nb_layers,confinent_p)

WriteDisplayFiles(freq_display,has_p=('rigid', has_p),layerID=('tactor',layerID))

for k in range(nb_steps):
  #
  IncrementStep()
  
  ComputeFext()
  # TODO 3:
  #
  # Call your function to get the returned array
  # and add the computed impulse
  
  ComputeBulk()
  ComputeFreeVelocity()
  
  SelectProxTactors()
  
  RecupRloc()
  ExSolver(storage,norm,tol,relax,gs_it1,gs_it2)
  StockRloc()
  
  ComputeDof()
  UpdateStep()
  
  WriteLastDof()
  WriteLastVlocRloc()

  WriteDisplayFiles(freq_display,has_p=('rigid', has_p),layerID=('tactor',layerID))

  # TODO 4:
  #
  # Break out of the time loop if
  # the maximum velocity of the sphere is
  # less than a chosen value
  #
  # needed functions:
  # - RBDY3_GetBodyVector for field 'V____'
  # - np.linalg.norm

CloseDisplayFiles()
ClosePostproFiles()

