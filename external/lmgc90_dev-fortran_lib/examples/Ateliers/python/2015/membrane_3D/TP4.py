import os,sys

from pylmgc90.chipy import *

##### TP4 : turn the moving hollow cylinders and walls invisble #####
#
# TODO 1 : Modify TP_sample.py to modify generation to add moving
#          hollow cylinders and walls at top and bottom of sample
#          to compute the compaction

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

  f_ext = np.zeros([6])

  for k in layer:
    k = np.array(k)
    r_out = np.max( r[k] + x[k,3] )
    dr    = 2.*np.max( x[k,3] )
    sic   = np.nonzero( r[k] > r_out - dr )
    F  = pressure * 2.*np.pi*r_out * dh / np.sum( r[k[sic[0]]] )
    for i in sic[0]:
      has_p[k[i]] = 1.
      f_ext[:2] = - x[k[i],3] * F * x[k[i],:2] / r[k[i]]
      RBDY3_PutBodyVector('Fext_',int(s2b[k[i],0]),dt*f_ext)

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

# TODO 1:
#
# Add DNLYC contactory type to your layerID dictionnary
#
# needed functions:
# - DNLYC_GetNbDNLYC

# TODO 2:
#
# Turn the moving DNLYC and PLANx invisible
# by checking the contactor color
#
# needed functions:
# - PLANx_GetPtrPLANx2RBDY3
# - DNLYC_GetPtrDNLYC2RBDY3
# - RBDY3_SetInvisible
# - RBDY3_GetContactorColor

has_p = applyPressure(dt,nb_layers,confinent_p)

WriteDisplayFiles(freq_display,has_p=('rigid', has_p),layerID=('tactor',layerID))

for k in range(nb_steps):
  #
  IncrementStep()
  
  ComputeFext()
  has_p = applyPressure(dt, nb_layers, confinent_p)
  
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

  max_v = 0.
  for i in range(s2b.shape[0]):
    max_v = max(max_v, np.linalg.norm( RBDY3_GetBodyVector('V____',int(s2b[i,0]))[:3] ) )

  utilities_logMes('max v : '+str(max_v))
  if max_v < 1.e-4:
    break

CloseDisplayFiles()
ClosePostproFiles()

