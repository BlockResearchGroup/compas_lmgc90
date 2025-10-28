import os,sys

from pylmgc90.chipy import *

checkDirectories()
### lecture du modele ###

### model reading ###
RBDY2_ReadBodies()

bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

#LOADS
DISKx_LoadTactors()
JONCx_LoadTactors()
RBDY2_LoadBehaviours()

TimeEvolution_ReadIniDof()
RBDY2_ReadIniDof()

TimeEvolution_ReadIniVlocRloc()
DKDKx_ReadIniVlocRloc()
DKJCx_ReadIniVlocRloc()

RBDY2_ReadDrivenDof()

### ecriture paranoiaque du modele ###
overall_WriteBodies()
RBDY2_WriteBodies()

bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

overall_WriteDrivenDof()
RBDY2_WriteDrivenDof()

### definition des parametres du calcul ### 
dt = 0.01
theta = 0.5
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

### post2D ##
post2D_SetDisplayedField('CONTACT POINT   ')
post2D_SetDisplayedField('TACTOR          ')
post2D_Init()

#postpro_PostproBeforeComputation()

### preparation de l'algo de detection par les boites ###

RBDY2_ComputeMass()

tol = 0.1666e-3
relax = 1.0
quad = 'Quad '
gs_it1 = 51
gs_it2 = 1001
freq_gmv = 1

timer_InitializeTimers()
id_contact_solve = timer_GetNewTimer("CONTACT SOLVER")
id_free_solve    = timer_GetNewTimer("FREE STATE SOLVER")

for k in range(1,10,1):
   #
   #print 'itere : ',k
   #
   #print 'INCREMENT STEP'
   TimeEvolution_IncrementStep()
   RBDY2_IncrementStep()

   #print 'DISPLAY TIMES'
   TimeEvolution_DisplayStep()

   #print 'COMPUTE Fext'
   RBDY2_ComputeFext()

   #print 'COMPUTE Fint'
   RBDY2_ComputeBulk()
   
   #print 'COMPUTE Free Vlocy'
   timer_StartTimer(id_free_solve)
   RBDY2_ComputeFreeVelocity()
   timer_StopTimer(id_free_solve)
   #
   #print 'SELECT PROX TACTORS'
   overall_SelectProxTactors()
   DKJCx_SelectProxTactors()
   DKDKx_SelectProxTactors()
   #
   DKJCx_RecupRloc()
   DKDKx_RecupRloc()
   timer_StartTimer(id_contact_solve)
   nlgs_ExSolver('Stored_Delassus_Loops         ',quad, tol, relax, gs_it1, gs_it2)
   timer_StopTimer(id_contact_solve)
   DKJCx_StockRloc()
   DKDKx_StockRloc()
   #
   #print 'COMPUTE DOF'
   RBDY2_ComputeDof()
   #
   #print 'UPDATE DOF'
   TimeEvolution_UpdateStep()
   RBDY2_UpdateDof()
   #
   #print 'WRITE LAST DOF'
   #TimeEvolution_WriteLastDof()
   #RBDY2_WriteLastDof()
   ##
   ##print 'WRITE LAST Vloc Rloc'
   #TimeEvolution_WriteLastVlocRloc()
   #DKDKx_WriteLastVlocRloc()
   #DKJCx_WriteLastVlocRloc()
   #
   ### post2D ###
   #overall_WriteOutDisplayFile(freq_gmv)
   #post2D_WriteOutDisplayFile(0)
   #postpro_PostproDuringComputation()
   ### wrtieout handling ###
   overall_CleanWriteOutFlags()

#postpro_ClosePostproFiles()
timer_WriteOutTimers()
