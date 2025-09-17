
import numpy as np

from pylmgc90 import chipy

chipy.Initialize()

chipy.checkDirectories()
#chipy.utilities_DisableLogMes()

# space dimension
dim = 2

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1

# time evolution parameters
dt = 5e-3
nb_steps = 200

# theta integrator parameter
theta = 0.5

# interaction parameters
Rloc_tol = 5.e-3

# nlgs parameters
tol = 1e-4
relax = 1.0
norm = 'Quad '
gs_it1 = 1
gs_it2 = 1
stype='Stored_Delassus_Loops         '

# write parameter
freq_write = 1

# display parameters
freq_display = 1

chipy.SetDimension(dim,mhyp)

chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

chipy.utilities_logMes('READ')
chipy.ReadBodies()
chipy.ReadBehaviours()
chipy.LoadBehaviours()
chipy.ReadDrivenDof()
chipy.LoadTactors()
chipy.ReadIni()

if freq_display > 0 :
  chipy.OpenDisplayFiles()
if freq_write > 0 :
  chipy.InitHDF5('lmgc90.h5')
  chipy.OpenPostproFiles()
  
chipy.utilities_logMes('INIT COMPUTE')

chipy.ComputeMass()
chipy.ComputeBulk()

gap = []
r_n = []
gap2= []
r_n2= []
vanished = False

for k in range(1, nb_steps + 1, 1):
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()

  chipy.utilities_logMes('COMPUTE')
  chipy.ComputeFext()
  chipy.ComputeBulk()
  chipy.ComputeFreeVelocity()
 
  chipy.utilities_logMes('SELECT PROX TACTORS')
  chipy.SelectProxTactors()

  chipy.utilities_logMes('RESOLUTION' )
  chipy.RecupRloc()
  chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
  chipy.UpdateTactBehav()
  chipy.StockRloc()

  chipy.utilities_logMes('COMPUTE/UPDATE')
  chipy.ComputeDof()
  chipy.UpdateStep()

  if freq_write > 0 :
    chipy.WriteOut(freq_write)
    chipy.WritePostproFiles()

  if freq_display > 0 :
    chipy.WriteDisplayFiles(freq_display)

  chipy.checkInteractiveCommand()
  chipy.overall_CleanWriteOutFlags()

  inters = chipy.getInteractions()
  if inters.size == 0:
    break

  for inter in inters:
    gap.append(inter['gapTT'])
    r_n.append(inter['rl'][1])
  
if freq_display > 0 :
  chipy.CloseDisplayFiles()
if freq_write > 0 :
  chipy.ClosePostproFiles()

chipy.Finalize()

from matplotlib import pyplot as pl

pl.plot(gap,r_n,'bo')
pl.plot(gap2,r_n2,'rx')
pl.xlabel('gap')
pl.ylabel('r_n')
pl.grid(True)
pl.title('Contact law graph')
pl.show()

