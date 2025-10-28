import numpy

from pylmgc90.chipy import *

# Time discretization:
dt = 1.e-2
theta=0.5

# driving time subdivision scheme
dt_max=100.
dt_min=1.e-2
t_final = 10.

# driving Newton Raphson
NR_nb_iter_max = 20
NR_tol=1e-6

freq_display = 10
freq_write = 100
ref_radius = 2.e-5

echo=0

checkDirectories()

#####################################

# 3D
SetDimension(3,0)

TimeEvolution_SetTimeStep(dt)
NewtonRaphson_SetFinalTime(t_final)
NewtonRaphson_SetMinTimeStep(dt_min)
NewtonRaphson_SetMaxTimeStep(dt_max)
NewtonRaphson_SetIncPatience(10000)
NewtonRaphson_SetGoodIter(3)
NewtonRaphson_SetBadIter(3)

# Newton loop parameters:
NewtonRaphson_SetMaxIter(NR_nb_iter_max)

# Initialize theta integrator
Integrator_InitTheta(theta)

mecaMAILx_BandStorage()

### model reading ###
utilities_logMes('READ BODIES')
MAILx_ReadBodies()

utilities_logMes('READ MODELS')
models_ReadModels()

utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()

### model writing ###
utilities_logMes('WRITE BODIES')
overall_WriteBodies()
MAILx_WriteBodies()

utilities_logMes('WRITE MODELS')
models_WriteModels()

utilities_logMes('WRITE BEHAVIOURS')
bulk_behav_WriteBehaviours()

### models initialization ###
utilities_logMes('INIT MODELS')
models_InitModels()
ExternalModels_InitModels()

#LOADS
mecaMAILx_LoadModels()

mecaMAILx_LoadBehaviours()

mecaMAILx_PushProperties()
models_StoreProperties()
ExternalModels_CheckProperties()

### initial and boundary conditions ###
utilities_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
mecaMAILx_ReadIniDof()

TimeEvolution_ReadIniGPV()
mecaMAILx_ReadIniGPV()

utilities_logMes('READ DRIVEN DOF')
mecaMAILx_ReadDrivenDof()

utilities_logMes('WRITE DRIVEN DOF')
overall_WriteDrivenDof()
mecaMAILx_WriteDrivenDof()

### post3D ##
utilities_logMes('INIT POSTPRO')
OpenDisplayFiles()
OpenPostproFiles()

utilities_logMes('COMPUTE MASS')
mecaMAILx_ComputeMass()

while TimeEvolution_GetTime() < t_final :
   #
   #utilities_logMes('INCREMENT STEP')
   TimeEvolution_IncrementStep()
   mecaMAILx_IncrementStep()

   #utilities_logMes('DISPLAY TIMES')
   TimeEvolution_DisplayStep()

   #utilities_logMes('COMPUTE Fext')
   mecaMAILx_ComputeFext()

   # Newton loop
   k=0
   is_converged = 1
   NewtonRaphson_Initialize(NR_tol)

   while is_converged == 1 : #looping until something changes in CheckNlConvergence
            
      k+=1
      utilities_logMes('iteration NR: '+str(k))
      
      #utilities_logMes('COMPUTE (gd) NL BULK')
      mecaMAILx_ComputeBulk()
      ##mecaMAILx_ComputeRayleighDamping(0.01,0.01)
      
      #utilities_logMes('ASSEMB RHS')
      mecaMAILx_AssembRHS()
      #utilities_logMes('ASSEMB KT')
      mecaMAILx_AssembKT()

      #utilities_logMes('COMPUTE Free Vlocy')
      mecaMAILx_ComputeFreeVelocity()

      #utilities_logMes('COMPUTE DOF')
      mecaMAILx_ComputeDof()
      #
      if k > 1 :
        norm = mecaMAILx_ComputeResidueNorm()
        ### 0=cv 1=continue 2=dv
        is_converged=NewtonRaphson_CheckConvergence(norm)

   ### end while

   #utilities_logMes('COMPUTE TIME STEP')
   ### istate = 1 redo, istate = 2 stop 
   istate = NewtonRaphson_ComputeTimeStep()

   if not istate == 1 :

     mecaMAILx_ComputeField()

     #utilities_logMes('UPDATE DOF')
     TimeEvolution_UpdateStep()
     mecaMAILx_UpdateDof()
     mecaMAILx_UpdateBulk()
     #
     TimeEvolution_WriteOutDof(freq_write)
     mecaMAILx_WriteOutDof()
     TimeEvolution_WriteOutGPV(freq_write)
     MAILx_WriteOutGPV()
     ### post3D ###
     WriteDisplayFiles(freq_display)
     WritePostproFiles()
     ### writeout handling ###
     overall_CleanWriteOutFlags()

     if istate == 2 :
       break
### end of time loop ###

### postpro ###
CloseDisplayFiles()
ClosePostproFiles()
