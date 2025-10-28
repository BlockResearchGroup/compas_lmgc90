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
dt = 1e-7
t_final = 8.e-5
dt_min = dt
dt_max = dt

NR_max_iter = 50
NR_adapt = 9999999
NR_tol = 1.e-3


# theta integrator parameter
theta = 0.56

# interaction parameters
Rloc_tol = 5.e-2

# nlgs parameters
tol = 1e-4
relax = 1.0
norm = 'Quad '
gs_it1 = 50
gs_it2 = 10
solver_type='Stored_Delassus_Loops         '

# write parameter
freq_write   = 5

# display parameters
freq_display = 5

#
# read and load
#

# Set space dimension
chipy.SetDimension(dim,mhyp)
#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

# Newton loop parameters:
chipy.NewtonRaphson_SetFinalTime(t_final)
chipy.NewtonRaphson_SetMinTimeStep(dt_min)
chipy.NewtonRaphson_SetMaxTimeStep(dt_max)
chipy.NewtonRaphson_SetMaxIter(NR_max_iter)
chipy.NewtonRaphson_SetIncPatience(NR_adapt)

#
chipy.ReadDatbox()

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

#
# simulation part ...
#
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

while chipy.TimeEvolution_GetTime() < t_final :
  #
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()

  chipy.utilities_logMes('COMPUTE Fext')
  chipy.ComputeFext()

  # Newton loop
  chipy.NewtonRaphson_Initialize(NR_tol)
  is_converged = 1
  k=0
  #looping until something changes in CheckConvergence
  while is_converged == 1 :
    k+=1
    chipy.utilities_logMes('COMPUTE BULK')
    chipy.ComputeBulk()

    chipy.utilities_logMes('ASSEMB RHS/KT')
    chipy.AssembleMechanicalRHS()
    chipy.AssembleMechanicalLHS()

    chipy.utilities_logMes('COMPUTE Free Vlocy')
    chipy.ComputeFreeVelocity()
    #
    chipy.utilities_logMes('SELECT PROX TACTORS')
    chipy.SelectProxTactors()
    #
    ### Signorini Coulomb
    chipy.RecupRloc()
    chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
    chipy.StockRloc()
    ###
    chipy.utilities_logMes('COMPUTE DOF')
    chipy.ComputeDof()
    #
    if k > 1:
      NR_norm = chipy.mecaMAILx_ComputeResidueNorm()
      is_converged = chipy.NewtonRaphson_CheckConvergence(NR_norm)

  ### end while NR

  chipy.utilities_logMes('COMPUTE TIME STEP')
  #istate = 1 => redo step
  #istate = 2 => stop

  istate = chipy.NewtonRaphson_ComputeTimeStep()

  if not istate == 1 :

    chipy.utilities_logMes('UPDATE TACT BEHAV')
    chipy.UpdateTactBehav()
    chipy.StockRloc()

    chipy.utilities_logMes('UPDATE DOF')
    chipy.UpdateStep()
    #
    ### write results ###
    #
    chipy.WriteOut(freq_write)

    chipy.WriteDisplayFiles(freq=freq_display)
    chipy.WritePostproFiles()

    if istate == 2 :
      # istate => Stop
      break

### end while time loop ###

#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()
