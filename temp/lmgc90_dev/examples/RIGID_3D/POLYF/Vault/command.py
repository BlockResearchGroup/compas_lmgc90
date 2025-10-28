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
dim = 3

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 0

# time evolution parameters
dt = 1e-3
nb_steps = 10

# theta integrator parameter
theta = 0.5

# deformable  yes=1, no=0
deformable = 0

# interaction parameters
Rloc_tol = 5.e-2

# nlgs parameters
chipy.nlgs_3D_DiagonalResolution()
chipy.nlgs_3D_SetWithQuickScramble()
tol = 1.666e-4
relax = 1.
# you can consider using relax = 1./4. to have a symmetrical solution
norm = 'QM/16'
gs_it1 = 100
gs_it2 = 10
solver_type='Stored_Delassus_Loops         '

# write parameter
freq_write   = 10

# display parameters
freq_display = 10

## there are non convex blocks
chipy.POLYR_SkipAutomaticReorientation()
chipy.POLYR_TopologyAngle(5.0)
chipy.POLYR_SkipHEBuild()

chipy.PRPRx_ShrinkPolyrFaces(0.05)
chipy.PRPRx_UseCpF2fExplicitDetection(1e-2)

#chipy.PRPRx_LowSizeArrayPolyr(10)

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
chipy.ReadDatbox(deformable)
#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles(write_f2f=True)
chipy.OpenPostproFiles()

#
# simulation part ...
#

# ... calls a simulation time loop
# since constant compute elementary mass once
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

for k in range(0,nb_steps):
  #
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()

  chipy.utilities_logMes('COMPUTE Fext')
  chipy.ComputeFext()
  chipy.utilities_logMes('COMPUTE Fint')
  chipy.ComputeBulk()
  chipy.utilities_logMes('COMPUTE Free Vlocy')
  chipy.ComputeFreeVelocity()

  chipy.utilities_logMes('SELECT PROX TACTORS')
  chipy.SelectProxTactors()

  chipy.utilities_logMes('RESOLUTION' )
  chipy.RecupRloc(Rloc_tol)

  chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
  chipy.UpdateTactBehav()

  chipy.StockRloc()

  chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
  chipy.ComputeDof()

  chipy.utilities_logMes('UPDATE DOF, FIELDS')
  chipy.UpdateStep()

  chipy.utilities_logMes('WRITE OUT DOF')
  chipy.WriteOutDof(freq_write)
  chipy.utilities_logMes('WRITE OUT Rloc')
  chipy.WriteOutVlocRloc(freq_write)

  chipy.utilities_logMes('VISU & POSTPRO')
  chipy.WriteDisplayFiles(freq_display)
  chipy.WritePostproFiles()

#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()
