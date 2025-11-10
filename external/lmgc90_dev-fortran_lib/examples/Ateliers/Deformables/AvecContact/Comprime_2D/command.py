import sys

from pylmgc90.chipy import *

checkDirectories()

#utilities_DisableLogMes()

# Time discretization:
dt = 5.e-3
t_final = 1.

# Initialize theta integrator
theta = 0.5

# info ecriture fichier de sortie
freq_write = 5

# info contact

#       123456789012345678901234567890
type = 'Stored_Delassus_Loops         '
norm = 'Quad '
tol = 0.1666E-05
relax = 0.1
gs_it1 = 200
gs_it2 = 50

# 2D PSTRAIN
overall_DIME(2, 1)

nlgs_SetWithQuickScramble()

# Newton loop parameters:
NR_tol=1e-6
NewtonRaphson_SetFinalTime(t_final)
NewtonRaphson_SetMinTimeStep(dt)
NewtonRaphson_SetMaxTimeStep(dt)
NewtonRaphson_SetMaxIter(20)
NewtonRaphson_SetIncPatience(999999)

NewtonRaphson_CheckConvergence(NR_tol)

TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)
Integrator_SetContactDetectionConfiguration(1.-theta,0.)
#Integrator_SetContactDetectionConfiguration(theta*(1.-theta),theta*theta)
### model reading ###
utilities_logMes('READ BODIES')
MAILx_ReadBodies()

utilities_logMes('READ MODELS')
models_ReadModels()

utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

### models initialization ###
utilities_logMes('INIT MODELS')
models_InitModels()
ExternalModels_InitModels()

#LOADS
mecaMAILx_LoadModels()
ALpxx_LoadTactors()
CLxxx_LoadTactors()

mecaMAILx_LoadBehaviours()

mecaMAILx_PushProperties()
models_StoreProperties()
ExternalModels_CheckProperties()

### initial and boundary conditions ###
utilities_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
mecaMAILx_ReadIniDof()

utilities_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
CLALp_ReadIniVlocRloc()

TimeEvolution_ReadIniGPV()
mecaMAILx_ReadIniGPV()

utilities_logMes('READ DRIVEN DOF')
mecaMAILx_ReadDrivenDof()

### paranoiac writing ###

utilities_logMes('WRITE BODIES')
overall_WriteBodies()
MAILx_WriteBodies()

utilities_logMes('WRITE BEHAVIOURS')
bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

utilities_logMes('WRITE MODELS')
models_WriteModels()

utilities_logMes('WRITE DRIVEN DOF')
overall_WriteDrivenDof()
mecaMAILx_WriteDrivenDof()

### postpro ###
postpro_PostproBeforeComputation()

CLxxx_SetNbNodesByCLxxx(2)

### initializing box detection algorithm ###
#CLALp_SetNonSymetricDetection()

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
   NewtonRaphson_Initialize(NR_tol)
   is_converged = 1
   k=0
   while is_converged == 1 : #looping until something changes in CheckNlConvergence

      k+=1
            
      #utilities_logMes('COMPUTE (gd) NL BULK')
      mecaMAILx_ComputeBulk()
      #mecaMAILx_ComputeRayleighDamping(0.05,0.05)      

      #utilities_logMes('ASSEMB NL RHS/KT')
      mecaMAILx_AssembRHS()
      mecaMAILx_AssembKT()

      #utilities_logMes('COMPUTE Free Vlocy')
      mecaMAILx_ComputeFreeVelocity()
      #
      #utilities_logMes('SELECT PROX TACTORS')
      mecaMAILx_ComputeContactDetectionConfiguration()
      overall_SelectProxTactors()
      CLALp_SelectProxTactors()
      #
      ### Signorini Coulomb
      CLALp_RecupRloc()
      nlgs_ExSolver(type, norm, tol, relax, gs_it1, gs_it2)
      CLALp_StockRloc()
      ###

      #utilities_logMes('COMPUTE DOF')
      mecaMAILx_ComputeDof()
      #
      if k > 1:
        NR_norm = mecaMAILx_ComputeResidueNorm()
        is_converged = NewtonRaphson_CheckConvergence(NR_norm)

   ### end while

   #utilities_logMes('COMPUTE TIME STEP')
   istate = NewtonRaphson_ComputeTimeStep()
   if not istate == 1 :

     #utilities_logMes('UPDATE TACT BEHAV')
     nlgs_UpdateTactBehav()
     CLALp_StockRloc()
     ### postpro ###
     postpro_PostproDuringComputation()
     #utilities_logMes('UPDATE DOF')
     mecaMAILx_ComputeField()
     TimeEvolution_UpdateStep()
     mecaMAILx_UpdateDof()
     mecaMAILx_UpdateBulk()
     #
     ### write results ###
     #utilities_logMes('WRITE LAST DOF')
     TimeEvolution_WriteOutDof(freq_write)
     mecaMAILx_WriteOutDof()
     #
     #utilities_logMes('WRITE LAST Vloc Rloc')
     TimeEvolution_WriteOutVlocRloc(freq_write)
     CLALp_WriteOutVlocRloc()
     #
     #utilities_logMes('WRITE LAST GPV')
     TimeEvolution_WriteOutGPV(freq_write)
     MAILx_WriteOutGPV()
     #
     ### writeout handling ###
     overall_CleanWriteOutFlags()

   if istate == 2 :
     # istate => Stop
     break

### end of time loop ###

### write results ###

### postpro ###
postpro_ClosePostproFiles()
