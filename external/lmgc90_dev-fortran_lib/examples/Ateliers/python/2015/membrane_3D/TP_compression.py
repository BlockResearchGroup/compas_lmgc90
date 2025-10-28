import os, shutil

from pylmgc90.chipy import *

##### TP final : create a new directory to run the compression  #####
#
# TODO :
#  create a new directory
#  copy the current DATBOX in it
#  copy the OUTBOX/*.LAST files as .INI file in the DATBOX of the computation directory
#  change the computation directory     
#
# needed function
#

comp_path = 'COMPRESSION'
if not os.path.isdir(comp_path):
  os.mkdir(comp_path)

comp_datbox = os.path.join(comp_path,'DATBOX')
if os.path.isdir(comp_datbox):
  shutil.rmtree(comp_datbox)

shutil.copytree('DATBOX', comp_datbox)
shutil.copy('OUTBOX/DOF.LAST', os.path.join(comp_datbox,'DOF.INI'))
shutil.copy('OUTBOX/Vloc_Rloc.LAST', os.path.join(comp_datbox,'Vloc_Rloc.INI'))

overall_SetWorkingDirectory(comp_path+'/')

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

dt = 1.e-2
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
layerID['DNLYC'] = np.zeros(DNLYC_GetNbDNLYC())

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

CloseDisplayFiles()
ClosePostproFiles()

