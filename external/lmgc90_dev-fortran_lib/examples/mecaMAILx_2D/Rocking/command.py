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
nb_steps = 200

# theta integrator parameter
theta = 0.51

# deformable  yes=1, no=0
deformable = 1

# interaction parameters
Rloc_tol = 5.e-2

# nlgs parameters
tol = 1.666e-4
relax = 1.0
norm = 'QM/16'
gs_it1 = 50
gs_it2 = 500
solver_type='Stored_Delassus_Loops         '

# write parameter
freq_write   = 10

# display parameters
freq_display = 10

idr = chipy.timer_GetNewTimer('load')
idd = chipy.timer_GetNewTimer('display')
idw = chipy.timer_GetNewTimer('write')

chipy.timer_StartTimer(idr)

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

chipy.timer_StopTimer(idr)

#
# open display & postpro
#

chipy.timer_StartTimer(idd)

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()
chipy.timer_StopTimer(idd)

#
# simulation part ...
#

# ... calls a simulation time loop
# since constant compute elementary mass matrices once
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

# since constant compute elementary stiffness matrices once
chipy.utilities_logMes('COMPUTE STIFFNESS')
chipy.ComputeBulk()

# since constant compute iteration matrix once
chipy.utilities_logMes('ASSEMB KT')
chipy.AssembleMechanicalLHS()

chipy.mecaMAILx_SetPreconAllBodies()
chipy.CLxxx_PushPreconNodes()
chipy.ALpxx_PushPreconNodes()
chipy.mecaMAILx_ComputePreconW()

for k in range(1, nb_steps + 1, 1):
  #
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()
  #
  chipy.utilities_logMes('COMPUTE Fext')
  chipy.ComputeFext()
  #
  chipy.utilities_logMes('COMPUTE Fint')
  chipy.ComputeBulk()
  #
  chipy.utilities_logMes('ASSEMB RHS')
  chipy.AssembleMechanicalRHS()
  #
  chipy.utilities_logMes('COMPUTE Free Vlocy')
  chipy.ComputeFreeVelocity()
  #
  chipy.utilities_logMes('SELECT PROX TACTORS')
  chipy.SelectProxTactors()
  #
  chipy.utilities_logMes('RESOLUTION' )
  chipy.RecupRloc()
  #
  chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
  chipy.UpdateTactBehav()
  #
  chipy.StockRloc()
  #
  chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
  chipy.ComputeDof()
  #
  chipy.utilities_logMes('UPDATE DOF, FIELDS')
  chipy.UpdateStep()
  #

  chipy.timer_StartTimer(idw)

  chipy.utilities_logMes('WRITE OUT')
  chipy.WriteOut(freq_write)
  #

  chipy.timer_StopTimer(idw)

  chipy.timer_StartTimer(idd)

  chipy.utilities_logMes('VISU & POSTPRO')
  chipy.WriteDisplayFiles(freq_display)
  chipy.WritePostproFiles()

  chipy.timer_StopTimer(idd)

#
# close display & postpro
#
chipy.timer_StartTimer(idd)

chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

chipy.timer_StopTimer(idd)

# this is the end
chipy.Finalize()
