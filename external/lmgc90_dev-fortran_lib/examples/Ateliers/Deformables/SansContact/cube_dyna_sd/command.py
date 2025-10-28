
from pylmgc90.chipy import *
from numpy import *

####
# info gestion du temps
dt = 1.e-5
theta = 0.50
nstep = 100

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 5
freq_write = 5
ref_radius = 0.05

# info contact

###
checkDirectories()
###
SetDimension(3,0)

utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

### lecture du modele ###
utilities_logMes('READ BODIES')
MAILx_ReadBodies()

utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()

utilities_logMes('READ MODELS')
models_ReadModels()

# on dimensionne et on initie la construction des mapping
utilities_logMes('INIT MODELS')
models_InitModels()
ExternalModels_InitModels()

# on charge les choses et on construit les mapping
utilities_logMes('LOADS')
mecaMAILx_LoadModels()
mecaMAILx_LoadBehaviours()
utilities_logMes('PUSH')
mecaMAILx_PushProperties()
utilities_logMes('STORE')
models_StoreProperties()
utilities_logMes('CHECK')
ExternalModels_CheckProperties()
#
utilities_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
mecaMAILx_ReadIniDof()
#
utilities_logMes('READ INI GPV')
TimeEvolution_ReadIniGPV()
mecaMAILx_ReadIniGPV()
#
utilities_logMes('READ DRIVEN DOF')
mecaMAILx_ReadDrivenDof()
#
### ecriture paranoiaque du modele ###
utilities_logMes('WRITE BODIES')
overall_WriteBodies()
MAILx_WriteBodies()
#
utilities_logMes('WRITE MODELS')
models_WriteModels()
#
utilities_logMes('WRITE BEHAVIOURS')
bulk_behav_WriteBehaviours()
#
utilities_logMes('WRITE DRIVEN DOF')
overall_WriteDrivenDof()
mecaMAILx_WriteDrivenDof()
#
utilities_logMes('WRITE DOF')
TimeEvolution_WriteLastDof()
mecaMAILx_WriteLastDof()

### definition des parametres du calcul ### 

### post3D ##
OpenDisplayFiles()
OpenPostproFiles()

### parameters setting ###

utilities_logMes('COMPUTE MASS')
mecaMAILx_ComputeMass()

utilities_logMes('COMPUTE STIFFNESS')

mecaMAILx_ComputeBulk()
#mecaMAILx_ComputeRayleighDamping(0.,0.000001)

mecaMAILx_AssembKT()

for k in range(1,nstep+1,1):
   #
   utilities_logMes('increment : '+str(k))
   #
   utilities_logMes('INCREMENT STEP')
   TimeEvolution_IncrementStep()
   mecaMAILx_IncrementStep()
   
   utilities_logMes('DISPLAY TIMES')
   TimeEvolution_DisplayStep()

   utilities_logMes('COMPUTE Fext')
   mecaMAILx_ComputeFext()

   utilities_logMes('COMPUTE Fint')
   mecaMAILx_ComputeBulk()

   utilities_logMes('ASSEMBLAGE')
   mecaMAILx_AssembRHS()

   utilities_logMes('COMPUTE Free Vlocy')
   mecaMAILx_ComputeFreeVelocity()

   utilities_logMes('COMPUTE DOF, FIELDS, etc.')
   mecaMAILx_ComputeDof()
   mecaMAILx_ComputeField()

   utilities_logMes('UPDATE DOF, FIELDS')
   TimeEvolution_UpdateStep()
   mecaMAILx_UpdateDof()
   mecaMAILx_UpdateBulk()

   utilities_logMes('WRITE OUT DOF')
   TimeEvolution_WriteOutDof(freq_write)
   mecaMAILx_WriteOutDof()

   utilities_logMes('WRITE OUT GPV')
   TimeEvolution_WriteOutGPV(freq_write)
   MAILx_WriteOutGPV()

   ### visu ###
   WriteDisplayFiles(freq_display)
   WritePostproFiles()

   ### gestion des writeout ###
   overall_CleanWriteOutFlags()

### postpro ###
CloseDisplayFiles()
ClosePostproFiles()
