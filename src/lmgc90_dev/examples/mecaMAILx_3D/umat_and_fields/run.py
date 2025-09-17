import sys

import numpy as np

# importing chipy module
from pylmgc90 import chipy

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()


#utilities_DisableLogMes()

#
# defining some variables
#

# space dimension
dim = 3

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1

# time evolution parameters
dt = 1e-3
dt_min  = dt/1000
dt_max  = dt*1

t_apply = 10*dt
t_final = 20*dt

NR_max_iter = 20
NR_adapt = 5
#NR_tol = 1.e-4
NR_tol = 1.e-3

# theta integrator parameter
theta = 0.5

# deformable  yes=1, no=0
deformable = 1

# interaction parameters
Rloc_tol = 5.e-2

chipy.CSASp_SkipAutoContact()
chipy.CSASp_SetNonSymmetricDetection()
chipy.CSASp_SetTrimAngle(10.)
chipy.CSASp_Trim()

# nlgs parameters
tol = 1e-5
relax = 1.0
norm = 'Quad '
gs_it1 = 400
gs_it2 = 50
solver_type='Stored_Delassus_Loops         '

chipy.nlgs_3D_DiagonalResolution()


# write parameter
freq_write   = 100
freq_write   = 1

# display parameters
freq_display = 100
freq_display = 1


#
# read and load
#

# Set space dimension
chipy.SetDimension(dim,mhyp)

# Newton loop parameters:

chipy.NewtonRaphson_SetFinalTime(t_final)
chipy.NewtonRaphson_SetMinTimeStep(dt_min)
chipy.NewtonRaphson_SetMaxTimeStep(dt_max)
chipy.NewtonRaphson_SetMaxIter(NR_max_iter)
chipy.NewtonRaphson_SetIncPatience(NR_adapt)

#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
#
chipy.utilities_logMes('READ DATBOX')
chipy.ReadDatbox(deformable=True)

# Getting index of some internal parameters beta, surface, etc for each kind of law
chipy.registerInterInternals('beta')
chipy.addRegistersToDisplay(True)

status=[]
materials=[]

nbm = chipy.mecaMAILx_GetNbMecaMAILx()
for im in range(1,nbm+1):
  status.append(chipy.mecaMAILx_GetDofStatus(im))
  materials.append(chipy.mecaMAILx_GetMaterials(im))

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

#
# simulation part ...
#

# ... calls a simulation time loop


###########################################################################################
#
#  P H A S E   1  :    P O I D S     P R O P R E     G L U E
#
###########################################################################################

print('BEGIN PHASE 1 : SW glue')

T0 = 293.
n_step = 0
rho0=0.1950e+4

while chipy.TimeEvolution_GetTime() < t_final :


  display_T = []
  display_rho = []

  if chipy.TimeEvolution_GetTime() > t_apply:
   T0 = min(T0+((t_final-t_apply)*200.),393.)
      
  for im in range(1,nbm+1):
    T=T0*np.ones(chipy.mecaMAILx_GetNbNodes(im)) 
    chipy.mecaMAILx_SetScalarFieldByNode(im,1,T)
    display_T.append(T)

    rho=(im+1)*rho0*np.ones(chipy.mecaMAILx_GetNbNodes(im)) 
    chipy.mecaMAILx_SetScalarFieldByNode(im,2,rho)
    display_rho.append(rho)


  chipy.utilities_logMes('COMPUTE MASS')
  chipy.ComputeMass()

  print('\n--------------------------------------------------')
  print('step ',n_step,' :: tps = ',chipy.TimeEvolution_GetTime())
  
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()

  scale=chipy.TimeEvolution_GetTime()/t_apply
  scale=min(1.,scale)
  chipy.bulk_behav_SetGravity([0.,0.,-9.81*scale])      
  print('step ',n_step,' :: g = ',-9.81*scale)
  
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
    chipy.mecaMAILx_ComputeRayleighDamping(0.,0.01)

    chipy.utilities_logMes('ASSEMB RHS/KT')
    chipy.AssembleMechanicalRHS()
    chipy.AssembleMechanicalLHS()

    chipy.utilities_logMes('COMPUTE Free Vlocy')
    chipy.ComputeFreeVelocity()

    if k == 1:
       chipy.utilities_logMes('SELECT PROX TACTORS')
       chipy.SelectProxTactors()

    chipy.RecupRloc()
    #chipy.CSASp_RecupRlocByPosition(5.e-3)
    chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
    chipy.StockRloc()

    chipy.utilities_logMes('COMPUTE DOF')
    chipy.ComputeDof()

    if k > 1:
      NR_norm = chipy.mecaMAILx_ComputeResidueNorm()
      is_converged = chipy.NewtonRaphson_CheckConvergence(NR_norm)

  ### end while NR
  
  chipy.utilities_logMes('COMPUTE TIME STEP')
  #istate = 1 => redo step
  #istate = 2 => stop
  
  istate = chipy.NewtonRaphson_ComputeTimeStep()

  if not istate == 1 :

    chipy.mecaMAILx_ComputeField()

    chipy.utilities_logMes('UPDATE TACT BEHAV')
    chipy.UpdateTactBehav()
    chipy.StockRloc()

    chipy.utilities_logMes('UPDATE DOF')
    chipy.UpdateStep()

    chipy.WriteOutDof(freq_write)
    chipy.WriteOutGPV(freq_write)
    chipy.WriteOutVlocRloc(freq_write)

    chipy.WriteDisplayFiles(freq_display,
                            DrvDof=('mecafe','node',status),
                            Material=('mecafe','element',materials),
                            T=('mecafe','node',display_T),
                            rho=('mecafe','node',display_rho))

    chipy.WritePostproFiles()

    if istate == 2 :
      break

    chipy.checkInteractiveCommand()

    n_step += 1


### end while time loop ###

print('END PHASE 1 : SW glue')

#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

chipy.utilities_logMes('WRITE LAST DOF')
chipy.WriteLastDof()

chipy.utilities_logMes('WRITE LAST Vloc Rloc')
chipy.WriteLastVlocRloc()

chipy.utilities_logMes('WRITE LAST GPV')
chipy.WriteLastGPV()

# this is the end
chipy.Finalize()
