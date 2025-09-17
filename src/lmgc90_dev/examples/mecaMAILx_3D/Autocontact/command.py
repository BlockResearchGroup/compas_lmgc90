
# coding: utf-8

def Principal_Stress(Stress):
	#Contrainte principale 1
	S1 = []
	#Contrainte principale 2
	S2 = []
	for i in range(Stress.shape[0]):
		A = np.zeros((2,2))
		A[0][0] = Stress[i][0]
		A[0][1] = Stress[i][2]
		A[1][0] = A[0][1]
		A[1][1]= Stress[i][1]
		B = np.linalg.eig(A)
		S1.append(B[0][0])
		S2.append(B[0][1])
	return S1, S2

# importing chipy module
from pylmgc90 import chipy
import numpy as np

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()

# logMes
# utilities_DisableLogMes()

#
# defining some variables
#

# space dimension
dim =3

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 0

# time evolution parameters
dt = 0.01
nb_steps = 200

# theta integrator parameter
theta = 0.5

# interaction parameters
Rloc_tol = 5.e-4

# nlgs parameters
tol    = 1e-8
relax  = 1.0
norm   = 'Quad '
gs_it1 = 100
gs_it2 = 30
stype  ='Stored_Delassus_Loops         '
chipy.nlgs_3D_DiagonalResolution()

# write parameter
freq_write   = 100

# display parameters
freq_display = 5 

# Additional parameters are necessary at the beginning of the script for non-linear pb
t_final = nb_steps*dt
dt_min = dt
dt_max = dt

NR_max_iter = 20
NR_adapt = 9999999
NR_tol = 1.e-3

#
# read and load
#

# Set space dimension
chipy.SetDimension(dim)
#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
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

# ... calls a simulation time loop

# Newton loop parameters:
chipy.NewtonRaphson_SetFinalTime(t_final)
chipy.NewtonRaphson_SetMinTimeStep(dt_min)
chipy.NewtonRaphson_SetMaxTimeStep(dt_max)
chipy.NewtonRaphson_SetMaxIter(NR_max_iter)
chipy.NewtonRaphson_SetIncPatience(NR_adapt)

chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

# to see which nodes have driven dof 
status=[]
status.append(chipy.mecaMAILx_GetDofStatus(1))

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

      # Additional parameter for damping :
      chipy.mecaMAILx_ComputeRayleighDamping(0.0,0.1)
      
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
      chipy.RecupRloc(Rloc_tol)
      chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
      chipy.StockRloc()
      
      ###
      chipy.utilities_logMes('COMPUTE DOF')
      chipy.ComputeDof()
      chipy.mecaMAILx_ComputeField()
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

      chipy.utilities_logMes('COMPUTE DOF')
      chipy.ComputeDof()
      chipy.utilities_logMes('UPDATE DOF')
      chipy.mecaMAILx_FatalDamping()
      chipy.UpdateStep()
      #
      ### write results ###
      #
      chipy.WriteOut(freq_write)

      #~ Stress = chipy.mecaMAILx_GetStrain(1)
      #~ E, S2 = Principal_Stress(Stress)
  
      chipy.WriteDisplayFiles(freq=freq_display,DrvDof=('mecafe','node',status) )
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

