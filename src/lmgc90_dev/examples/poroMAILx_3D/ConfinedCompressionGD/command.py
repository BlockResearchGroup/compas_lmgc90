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
dt = 25.
t_final = 1.e3
dt_min = dt
dt_max = dt

NR_max_iter = 50
NR_adapt = 999999
NR_tol = 1.e-10

# theta integrator parameter
theta = 1.

# write parameter
freq_write   = 1

# display parameters
freq_display = 1

#
# read and load
#
chipy.poroMAILx_SparseStorage()


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

# Setting some parameters
chipy.bulk_behav_SetConductivity('biot_', 0, 7.5e-01)
chipy.bulk_behav_SetBiot('biot_', 0, 1.0)
chipy.bulk_behav_SetCapacity('biot_', 0, 0.0)
chipy.bulk_behav_SetDensity('biot_', 0.0)
chipy.bulk_behav_SetExternalFlux('biot_', 0, 0.0)

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

#
# simulation part ...
#
while chipy.TimeEvolution_GetTime() < t_final :
  #
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()

  # Newton loop
  chipy.NewtonRaphson_Initialize(NR_tol)
  is_converged = 1
  k=0
  #looping until something changes in CheckConvergence
  while is_converged == 1 :
    k+=1
    chipy.utilities_logMes('COMPUTE Fext')
    chipy.ComputeFext()
    chipy.utilities_logMes('COMPUTE Mass')
    chipy.ComputeMass()
    chipy.utilities_logMes('COMPUTE Damping')
    chipy.poroMAILx_ComputeDamping()
    chipy.utilities_logMes('COMPUTE BULK')
    chipy.ComputeBulk()

    chipy.utilities_logMes('ASSEMB RHS/KT')
    chipy.AssemblePoroRHS()
    chipy.AssemblePoroLHS()

    chipy.utilities_logMes('COMPUTE Free Vlocy')
    chipy.ComputeFreeVelocity()

    chipy.utilities_logMes('COMPUTE DOF')
    chipy.ComputeDof()
    #
    if k > 1:
      NR_norm = chipy.poroMAILx_ComputeResidueNorm()
      is_converged = chipy.NewtonRaphson_CheckConvergence(NR_norm)

  ### end while NR

  chipy.utilities_logMes('COMPUTE TIME STEP')
  #istate = 1 => redo step
  #istate = 2 => stop

  istate = chipy.NewtonRaphson_ComputeTimeStep()

  if not istate == 1 :

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
