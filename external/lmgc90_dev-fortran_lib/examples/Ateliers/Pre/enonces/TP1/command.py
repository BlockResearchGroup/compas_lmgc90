
from pylmgc90.chipy import *
from numpy import *
####

checkDirectories()

utilities_DisableLogMes()

####
# info gestion du temps
dt = 1.e-4
theta = 0.505
nb_steps = 1000

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 10

# info contact

#       123456789012345678901234567890
type = 'Stored_Delassus_Loops         '
norm = 'Quad '
tol = 0.1e-3
relax = 1.0
gs_it1 = 100
gs_it2 = 10

###
overall_DIME(2,1)

### definition des parametres du calcul ### 
utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

### lecture du modele ###
utilities_logMes('READ BODIES')
RBDY2_ReadBodies()
MAILx_ReadBodies()

utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

utilities_logMes('READ MODELS')
models_ReadModels()

utilities_logMes('INIT MODELS')
# on dimensionne et on initie la construction des mapping
models_InitModels()
ExternalModels_InitModels()

utilities_logMes('LOADS')
# on charge les choses et on construit les mapping
mecaMAILx_LoadModels()
mecaMAILx_LoadBehaviours()
RBDY2_LoadBehaviours()

utilities_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
mecaMAILx_ReadIniDof()
mecaMAILx_ReadIniGPV()
RBDY2_ReadIniDof()

#CLxxx_LoadTactors()
ALpxx_LoadTactors()
#JONCx_LoadTactors()
DISKx_LoadTactors()

utilities_logMes('PUSH')
mecaMAILx_PushProperties()
#
utilities_logMes('STORE')
# on finalise la construction des mapping
models_StoreProperties()

utilities_logMes('CHECK')
ExternalModels_CheckProperties()

utilities_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
DKALp_ReadIniVlocRloc()

utilities_logMes('READ DRIVEN DOF')
mecaMAILx_ReadDrivenDof()
RBDY2_ReadDrivenDof()

### ecriture paranoiaque du modele ###
utilities_logMes('WRITE BODIES')
overall_WriteBodies()
MAILx_WriteBodies()
RBDY2_WriteBodies()

utilities_logMes('WRITE MODELS')
models_WriteModels()

utilities_logMes('WRITE BEHAVIOURS')
bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

utilities_logMes('WRITE DRIVEN DOF')
overall_WriteDrivenDof()
mecaMAILx_WriteDrivenDof()
RBDY2_WriteDrivenDof()

### post2D ##
post2D_SetReferenceRadius(0.01)
post2D_SetDisplayedField('MECHANICAL GPV  ')
post2D_SetDisplayedField('CONTACT POINT   ')
post2D_SetDisplayedField('TACTOR          ')
post2D_Init()

### postpro ###
postpro_PostproBeforeComputation()

utilities_logMes('COMPUTE MASS')
mecaMAILx_ComputeMass()
RBDY2_ComputeMass()

utilities_logMes('COMPUTE STIFFNESS')
mecaMAILx_ComputeBulk()

####
mecaMAILx_SetPreconAllBodies()
ALpxx_PushPreconNodes()
mecaMAILx_ComputePreconW()
####
mecaMAILx_AssembKT()
#
overall_WriteOutDisplayFile(1)
post2D_WriteOutDisplayFile(1)
for k in range(1, nb_steps + 1, 1):
   #
   utilities_logMes('increment : '+str(k))
   #
   utilities_logMes('INCREMENT STEP')
   TimeEvolution_IncrementStep()
   mecaMAILx_IncrementStep()
   RBDY2_IncrementStep()
   
   utilities_logMes('DISPLAY TIMES')
   TimeEvolution_DisplayStep()

   utilities_logMes('COMPUTE Fext')
   mecaMAILx_ComputeFext()
   RBDY2_ComputeFext()

   utilities_logMes('COMPUTE Fint')
   mecaMAILx_ComputeBulk()
   RBDY2_ComputeBulk()

   utilities_logMes('ASSEMBLAGE')
   mecaMAILx_AssembRHS()

   utilities_logMes('COMPUTE Free Vlocy')
   mecaMAILx_ComputeFreeVelocity()
   RBDY2_ComputeFreeVelocity()
   #
   utilities_logMes('SELECT PROX TACTORS')
   overall_SelectProxTactors()
   DKALp_SelectProxTactors()
   #
   DKALp_RecupRloc()
   #utilities_logMes(type,norm,tol, relax, gs_it1, gs_it2 )
   nlgs_ExSolver(type, norm, tol, relax, gs_it1, gs_it2)
   DKALp_StockRloc()
   #
   utilities_logMes('COMPUTE DOF, FIELDS, etc.')
   mecaMAILx_ComputeDof()
   mecaMAILx_ComputeBulk()
   RBDY2_ComputeDof()
   #
   utilities_logMes('UPDATE DOF, FIELDS')
   TimeEvolution_UpdateStep()
   mecaMAILx_UpdateDof()
   mecaMAILx_UpdateBulk()
   RBDY2_UpdateDof()
   #
   utilities_logMes('WRITE LAST DOF')
   TimeEvolution_WriteLastDof()
   mecaMAILx_WriteLastDof()
   RBDY2_WriteLastDof()
   #
   utilities_logMes('WRITE LAST Vloc Rloc')
   TimeEvolution_WriteLastVlocRloc()
   DKALp_WriteLastVlocRloc()

   ### post2D ###
   overall_WriteOutDisplayFile(freq_display)
   post2D_WriteOutDisplayFile(echo)

   ### postpro ###
   postpro_PostproDuringComputation()

   ### gestion des writeout ###
   overall_CleanWriteOutFlags()

### postpro ###
postpro_ClosePostproFiles()
