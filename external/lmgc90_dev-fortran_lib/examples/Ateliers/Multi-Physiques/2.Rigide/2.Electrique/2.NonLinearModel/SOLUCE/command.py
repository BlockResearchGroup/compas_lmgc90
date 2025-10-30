
from pylmgc90.chipy import *

checkDirectories()

### definition des parametres du calcul ### 

dt = 0.0002
nb_steps=4000
theta = 0.5

freq_display = 40
freq_write = 100

tol = 0.1666e-4
relax = 1.0
quad = 'Quad '
gs_it1 = 33
gs_it2 = 101

nlgs_SetWithQuickScramble()

utilites_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

### lecture du modele ###

### model reading ###
utilites_logMes('READ BODIES')
RBDY2_ReadBodies()
DISKx_LoadTactors()
JONCx_LoadTactors()

utilites_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

RBDY2_LoadBehaviours()
mp_solver_ReadMpBehaviour()
RBDY2_MP_LoadBehaviours(0.)

utilites_logMes('READ INI')
TimeEvolution_ReadIniDof()
RBDY2_ReadIniDof()

TimeEvolution_ReadIniVlocRloc()
DKJCx_ReadIniVlocRloc()
DKDKx_ReadIniVlocRloc()

RBDY2_ReadDrivenDof()

utilites_logMes('WRITE')
overall_WriteBodies()
RBDY2_WriteBodies()
bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()
mp_solver_WriteMpBehaviour()

overall_WriteDrivenDof()
RBDY2_WriteDrivenDof()

### post2D ##
utilites_logMes('INIT GMV')
post2D_SetDisplayedField('TACTOR          ')
post2D_SetDisplayedField('CONTACT POINT   ')
post2D_SetDisplayedField('OXIDE           ')
post2D_SetDisplayedField('ELECTRO         ')
post2D_Init()

### postpro ###
postpro_PostproBeforeComputation()

utilites_logMes('COMPUTE MASS')
RBDY2_ComputeMass()

for k in range(1,nb_steps+1,1):
   #
   utilites_logMes('INCREMENT STEP')
   TimeEvolution_IncrementStep()
   RBDY2_IncrementStep()

   utilites_logMes('DISPLAY TIMES')
   TimeEvolution_DisplayStep()

   utilites_logMes('COMPUTE Fext')
   RBDY2_ComputeFext()

   utilites_logMes('COMPUTE Fint')
   RBDY2_ComputeBulk()
   
   utilites_logMes('COMPUTE Free Vlocy')
   RBDY2_ComputeFreeVelocity()
   #
   utilites_logMes('SELECT PROX TACTORS')
   overall_SelectProxTactors()
   DKJCx_SelectProxTactors()
   DKDKx_SelectProxTactors()
   #
   DKJCx_RecupRloc()
   DKDKx_RecupRloc()
   nlgs_ExSolver('Stored_Delassus_Loops         ',quad, tol, relax, gs_it1, gs_it2)
   DKJCx_StockRloc()
   DKDKx_StockRloc()
   #
   mp_solver_SolveNlElectro1G()
   #
   utilites_logMes('COMPUTE DOF')
   RBDY2_ComputeDof()
   #
   utilites_logMes('UPDATE DOF')
   TimeEvolution_UpdateStep()
   RBDY2_UpdateDof()
   #
   ### post2D ###
   overall_WriteOutDisplayFile(freq_display)
   post2D_WriteOutDisplayFile(0)

   ### postpro ###
   postpro_PostproDuringComputation()

   ### writeout handling ###
   overall_CleanWriteOutFlags()
   #
### postpro ###
#
utilites_logMes('WRITE LAST DOF')
TimeEvolution_WriteLastDof()
RBDY2_WriteLastDof()
#
utilites_logMes('WRITE LAST Vloc Rloc')
TimeEvolution_WriteLastVlocRloc()
DKDKx_WriteLastVlocRloc()
DKJCx_WriteLastVlocRloc()
#
postpro_ClosePostproFiles()
