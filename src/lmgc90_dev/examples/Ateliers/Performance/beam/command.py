import sys

from pylmgc90.chipy import *
from numpy import *

checkDirectories()
utilities_DisableLogMes()

###
# probleme 3D
Initialize()
SetDimension(3)

####
# info gestion du temps
dt = 1.e-2
theta = 1.
nsteps = 10
# N.B.: l'exemple etant quasi-statique, theta est pris egal a 1 de sorte que le pas de chargement en deplacement
# soit donne par : du=v*dt, ou v est la vitesse imposee comme condition a la limite

# info generation fichier visu
freq_display = 10
freq_write = 10

mecaMAILx_BandStorage()
#mecaMAILx_ExplodedStorage()
#mecaMAILx_SparseStorage()

### definition des parametres du calcul ### 
utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(0.01)
Integrator_InitTheta(1.)

### lecture du modele ###
utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()

utilities_logMes('READ MODELS')
models_ReadModels()

utilities_logMes('READ BODIES')
MAILx_ReadBodies()

utilities_logMes('INIT MODELS')
# on dimensionne et on initie la construction des mapping
models_InitModels()
ExternalModels_InitModels()

utilities_logMes('LOADS')
# on charge les choses et on construit les mapping
mecaMAILx_LoadModels()
mecaMAILx_LoadBehaviours()

utilities_logMes('PUSH')
mecaMAILx_PushProperties()
#
utilities_logMes('STORE')
# on finalise la construction des mapping
models_StoreProperties()

utilities_logMes('CHECK')
ExternalModels_CheckProperties()

utilities_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
mecaMAILx_ReadIniDof()

TimeEvolution_ReadIniGPV()
mecaMAILx_ReadIniGPV()

utilities_logMes('READ DRIVEN DOF')
mecaMAILx_ReadDrivenDof()

### ecriture paranoiaque du modele ###
utilities_logMes('WRITE BODIES')
overall_WriteBodies()
MAILx_WriteBodies()

utilities_logMes('WRITE MODELS')
models_WriteModels()

utilities_logMes('WRITE BEHAVIOURS')
bulk_behav_WriteBehaviours()

utilities_logMes('WRITE DRIVEN DOF')
overall_WriteDrivenDof()
mecaMAILx_WriteDrivenDof()

### display ##
OpenDisplayFiles()

### postpro ###
OpenPostproFiles()

utilities_logMes('COMPUTE MASS')
mecaMAILx_ComputeMass()

utilities_logMes('COMPUTE STIFFNESS')
mecaMAILx_ComputeBulk()

mecaMAILx_AssembKT()
#
for k in range(1, nsteps + 1):
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
   #
   utilities_logMes('COMPUTE DOF, FIELDS, etc.')
   mecaMAILx_ComputeDof()
   mecaMAILx_ComputeField()
   #
   utilities_logMes('UPDATE DOF, FIELDS')
   TimeEvolution_UpdateStep()
   mecaMAILx_UpdateDof()
   mecaMAILx_UpdateBulk()
   #
   utilities_logMes('WRITE OUT DOF')
   TimeEvolution_WriteOutDof(freq_write)
   mecaMAILx_WriteOutDof()

   utilities_logMes('WRITE OUT GPV')
   TimeEvolution_WriteOutGPV(freq_write)
   MAILx_WriteOutGPV()
   #
   ### display ##
   WriteDisplayFiles()

   ### postpro ###
   WritePostproFiles()


   ### gestion des writeout ###
   overall_CleanWriteOutFlags()

   ### display ###### postpro ###
### display ##
CloseDisplayFiles()

### postpro ###
ClosePostproFiles()
Finalize()
