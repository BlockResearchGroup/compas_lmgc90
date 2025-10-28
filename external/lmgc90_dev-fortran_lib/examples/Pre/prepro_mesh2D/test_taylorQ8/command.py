
from pylmgc90 import chipy

chipy.checkDirectories()

chipy.Initialize()

#chipy.utilities_DisableLogMes()

# Time discretization:
dt = 1.e-7

theta=0.56

# driving time subdivision scheme
dt_max=1e-8
dt_min=1e-7
t_final = 2.e-4

# driving Newton Raphson
nb_NR_iter_max = 20
tol_NR=1e-10

freq_display = 10
ref_size=2.e-5
freq_write = 10

# 2D AXISym
chipy.SetDimension(dim=2,mod=3)
#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

# Newton loop parameters:
chipy.NewtonRaphson_SetFinalTime(t_final)
chipy.NewtonRaphson_SetMinTimeStep(dt)
chipy.NewtonRaphson_SetMaxTimeStep(dt)
chipy.NewtonRaphson_SetMaxIter(nb_NR_iter_max)
chipy.NewtonRaphson_SetIncPatience(999999)

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
  chipy.NewtonRaphson_Initialize(tol_NR)
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

    chipy.utilities_logMes('UPDATE DOF')
    chipy.UpdateStep()
    #
    ### write results ###
    #
    chipy.WriteOut(freq_write)

    chipy.WriteDisplayFiles(freq=freq_display)
    #chipy.WritePostproFiles()

    if istate == 2 :
      # istate => Stop
      break

### end while time loop ###

#
# close display & postpro
#
chipy.CloseDisplayFiles()
#chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()
