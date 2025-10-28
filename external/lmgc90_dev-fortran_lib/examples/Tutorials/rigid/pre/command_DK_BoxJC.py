# importing chipy module
from pylmgc90 import chipy

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()

# logMes
# chipy.utilities_DisableLogMes()

#
# defining some variables
#

# space dimension
dim = 2

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1

# time evolution parameters
dt = 1e-3
#nb_steps = 3000
nb_steps = 500

# theta integrator parameter
theta = 0.5

# deformable  yes=1, no=0
deformable = 0

# interaction parameters
Rloc_tol = 5.e-2

# nlgs parameters
tol = 1e-4
relax = 1.0
norm = 'Quad '
gs_it1 = 500
gs_it2 = 10
stype='Stored_Delassus_Loops         '

# write parameter
freq_write   = 10

# display parameters
freq_display = 10

#
# read and load
#

# Set space dimension
chipy.SetDimension(dim,mhyp)
#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
#
chipy.utilities_logMes('READ BEHAVIOURS')
chipy.ReadBehaviours()
if deformable: chipy.ReadModels()
#
chipy.utilities_logMes('READ BODIES')
chipy.ReadBodies()
#
chipy.utilities_logMes('LOAD BEHAVIOURS')
chipy.LoadBehaviours()
if deformable: chipy.LoadModels()
#
chipy.utilities_logMes('READ DRIVEN DOF')
chipy.ReadDrivenDof()
#
chipy.utilities_logMes('LOAD TACTORS')
chipy.LoadTactors()
#
chipy.utilities_logMes('READ INI')
chipy.ReadIni()

#
# paranoid writes
#
chipy.utilities_logMes('WRITE BODIES')
chipy.WriteBodies()
chipy.utilities_logMes('WRITE BEHAVIOURS')
chipy.WriteBehaviours()
chipy.utilities_logMes('WRITE DRIVEN DOF')
chipy.WriteDrivenDof()

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.InitHDF5('lmgc90.h5')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

#
# simulation part ...
#

# ... calls a simulation time loop
# since constant compute elementary mass once
chipy.ComputeMass()

for k in range(nb_steps):
  #
  chipy.IncrementStep()

  chipy.ComputeFext()
  chipy.ComputeBulk()
  chipy.ComputeFreeVelocity()

  chipy.SelectProxTactors()

  chipy.RecupRloc(Rloc_tol)

  chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
  chipy.UpdateTactBehav()

  chipy.StockRloc()

  chipy.ComputeDof()

  chipy.UpdateStep()

  chipy.WriteOut(freq_write)

  chipy.WriteDisplayFiles(freq_display)
  chipy.WritePostproFiles()

  chipy.checkInteractiveCommand()
#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()
