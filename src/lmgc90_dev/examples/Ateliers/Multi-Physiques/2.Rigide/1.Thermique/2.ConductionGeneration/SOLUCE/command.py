
from pylmgc90.chipy import *

checkDirectories()

### definition des parametres du calcul ### 

dt=4.e-4
theta=0.5
nb_steps_meca=1000

freq_display=100

quad = 'Quad '
tol = 1.666e-5
relax = 1.
it1 = 33
it2 = 101


utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

utilities_logMes('READ BODIES')
RBDY2_ReadBodies()

utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

utilities_logMes('LOAD BULK & TACTORS')
RBDY2_LoadBehaviours()
DISKx_LoadTactors()
JONCx_LoadTactors()

mp_solver_ReadMpBehaviour()
RBDY2_MP_LoadBehaviours(0.,'therm')
#RBDY2_MP_LoadBehaviours(0.,'sener')


RBDY2_SetPeriodicCondition(0.1)
DKDKx_SetPeriodicCondition(0.1)
post2D_SetPeriodicCondition(0.1)

RBDY2_ComputeMass()

utilities_logMes('READ INITIAL STATE')
TimeEvolution_ReadIniDof()
RBDY2_ReadIniDof()

TimeEvolution_ReadIniVlocRloc()
DKDKx_ReadIniVlocRloc()
DKJCx_ReadIniVlocRloc()

RBDY2_ReadDrivenDof()

utilities_logMes('WRITE DATBOX COPY')
overall_WriteBodies()
RBDY2_WriteBodies()

tact_behav_WriteBehaviours()
bulk_behav_WriteBehaviours()
mp_solver_WriteMpBehaviour()

utilities_logMes('INIT GMV VISUALISATION')
post2D_SetDisplayedField('AVERAGE VELOCITY')
post2D_SetDisplayedField('HEAT            ')
post2D_Init()

overall_WriteOutDisplayFile(1)
post2D_WriteOutDisplayFile(0)

utilities_logMes('INIT POSTPRO')
postpro_PostproBeforeComputation()

utilities_logMes('START THERMO-MECA LOOP')

for k in range(1,nb_steps_meca+1,1):
   #
   utilities_logMes('INCREMENT STEP')
   TimeEvolution_IncrementStep()
   RBDY2_IncrementStep()
   
   utilities_logMes('DISPLAY TIME')
   TimeEvolution_DisplayStep()

   utilities_logMes('COMPUTE RHS')
   RBDY2_ComputeFext()
   RBDY2_ComputeBulk()
   RBDY2_ComputeFreeVelocity()

   utilities_logMes('CONTACT DETECTION')
   overall_SelectProxTactors()
   DKDKx_SelectProxTactors()
   DKJCx_SelectProxTactors()

   utilities_logMes('CONTACT RESOLUTION')
   DKDKx_RecupRloc()
   DKJCx_RecupRloc()
   nlgs_ExSolver('Stored_Delassus_Loops        ',quad,tol,relax,it1,it2)
   DKDKx_StockRloc()
   DKJCx_StockRloc()

   utilities_logMes('THERMAL RESOLUTION')
   mp_solver_RecupTemperature()
   mp_solver_SolveThermoProblem()

   RBDY2_ComputeDof()

   TimeEvolution_UpdateStep()
   RBDY2_UpdateDof()

   overall_WriteOutDisplayFile(freq_display)
   post2D_WriteOutDisplayFile(0)

   postpro_PostproDuringComputation()
   overall_CleanWriteOutFlags()

#
postpro_ClosePostproFiles()
