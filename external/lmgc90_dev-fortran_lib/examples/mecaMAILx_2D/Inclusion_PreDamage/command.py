import os, sys

# importing chipy module
from pylmgc90 import chipy

import numpy as np

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()

# logMes
chipy.utilities_DisableLogMes()

#
# defining some variables
#

# space dimension
dim = 2

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1

# time evolution parameters
dt = 1.e-6
t_final = 2000*dt
dt_min = dt
dt_max = dt

NR_max_iter = 20
NR_adapt = 9999999
NR_tol = 1.e-3

# theta integrator parameter
theta = 0.5

# deformable  yes=1, no=0
deformable = 1
nb_CL_by_Edge = 2

# interaction parameters
freq_detect = 1
Rloc_tol = 5.e-2

# nlgs parameters
tol = 1e-5
relax = 1.0
norm = 'Quad '
gs_it1 = 50
gs_it2 = 10
solver_type='Stored_Delassus_Loops         '

# write parameter
freq_write   = 25

# display parameters
freq_display = 50

# for customized postpro
nbsteps   = int( t_final/dt )

nbf = 100 # nombre de pas pour la force 
nbc = 150 # nombre de pas avant d'appliquer le pre endommagement

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
chipy.ReadDatbox(deformable)
chipy.CLxxx_SetNbNodesByCLxxx(nb_CL_by_Edge)

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

# Set beta internal field of interaction to be added to visu
chipy.registerInterInternals(['beta', 'TPSini'])

### Pressure Law ###
# variable for pre-damaged

# rank of tact behav to be modified - law04 
TactLawnb = 4
#Value of beta at initial time
betamin_ini = 0.

# Caracteristic time
tau=100*dt
# exponent in the pressure-damage coupling
alpha = 1
# Pressure (Pa)
pF  = 0.
pFt = 1.e7

# const -- once beta=0 -- p = pF + pFt * (TPS -TPSini)
#chipy.tact_behav_SetPressureParameters(1,2,[pF, pFt, tau, alpha])

# lin pF + pFt * min(1,TPS-TPSini/tau))*(1-beta)**alpha
chipy.tact_behav_SetPressureParameters(TactLawnb,2,[pF, pFt, tau, alpha])

# exp pF + pFt * (1 - exp(-(TPS-TPSini)/tau)))*(1-beta)**alpha
#chipy.tact_behav_SetPressureParameters(3,2,[pF, pFt, tau, alpha])

# user pext * (1-beta)**alpha 
# chipy.tact_behav_SetPressureParameters(4,2,[pF, pFt, tau, alpha])

#
# simulation part ...
#
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()


while chipy.TimeEvolution_GetTime() < t_final :

  #
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()

  #chipy.TimeEvolution_DisplayStep()
  
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
    chipy.SelectProxTactors(freq_detect)
    #
    ### Signorini Coulomb
    chipy.RecupRloc()
    chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
    chipy.StockRloc()

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

    # Looping to change all beta of Tactlawnb 
    if  chipy.TimeEvolution_GetStep() == nbc:
       inters = chipy.getInteractions(this=True, human=False)
       inters_tact = inters[ inters['behav'] == TactLawnb ]
       chipy.setInternalArray('beta'  , inters_tact, betamin_ini)
       chipy.setInternalArray('TPSini', inters_tact, chipy.TimeEvolution_GetTime())

       chipy.StockRloc()

    ###
    
    chipy.utilities_logMes('UPDATE DOF')
    chipy.UpdateStep()
    #
    ### write results ###
    #
    chipy.WriteOut(freq_write)

    inters = chipy.getInteractions()
    beta   = chipy.getInternalArray('beta', inters)
    chipy.WriteDisplayFiles(freq_display, beta=('ptc', beta,))
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
