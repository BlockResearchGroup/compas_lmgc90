import os,sys
from pylmgc90.chipy import *

overall_DIME(3,0)

# Nombre de decoupe suivant l'axe x et y
nb_sdmx  = 2
nb_sdmy  = 1
nb_sdmz  = 1
# Type de methode de DDM : 1 -> NSCDD 
ddm_type = 1

# Pilotage des pas de temps
nb_steps = 50
dt = 5.e-3

# Parametre du schema d'integration
# de la dynamique des corps
theta = 0.5

# Frequence de detection des contacts "grossiers" qui donne
# aussi la frequence de partionnement en sous-domaine
freq_detect  = 500

# Frequence d'ecriture des visu VTK
freq_display = 10
# Frequence d'ecritue des DOF et VlocRloc
freq_write   = 10
# Frequence des sauvegardes des DOF, VlocRloc et timers
freq_last    = 500
# Frequence d'ecriture dans les fichiers de postpro
freq_postpro = 1

# Pilotage du Gauss-Seisel
#       123456789012345678901234567890
#type = 'Exchange_Local_Global         '
type = 'Stored_Delassus_Loops         '
tol = 1.e-4
relax = 1.0
quad = 'Quad '
gs_it1 = 50
gs_it2 = 200

nlgs_SetWithQuickScramble()

### Initialisation de MPI ### 
overall_Initialize()

### WD par sous-domaine ###
DDM_3D_SetDDWorkingDirectory()

### Gestion des messages de log ### 
#utilities_DisableLogMes()
utilities_EnableLogMes()

### Pour ne pas ecrire les donnees ###
### des corps invisibles dans les  ###
### les fichiers par sous-domaine  ###
RBDY3_SkipInvisible()

### Definition des parametres du calcul ### 
utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

### Model reading ###
utilities_logMes('READ BODIES')
RBDY3_ReadBodies()

utilities_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
RBDY3_ReadIniDof()

utilities_logMes('READ DRIVEN DOF')
RBDY3_ReadDrivenDof()

### LOADS ###
POLYR_LoadTactors()
SPHER_LoadTactors()

utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()
RBDY3_LoadBehaviours()

utilities_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
SPSPx_ReadIniVlocRloc()
PRPRx_ReadIniVlocRloc()

### Ecriture paranoiaque du modele ###
utilities_logMes('WRITE BODIES')
overall_WriteBodies()
RBDY3_WriteBodies()

utilities_logMes('WRITE BEHAVIOURS')
bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

utilities_logMes('WRITE DRIVEN DOF')
overall_WriteDrivenDof()
RBDY3_WriteDrivenDof()

utilities_logMes('COMPUTE MASS')
RBDY3_ComputeMass()

### Initialisation du module DDM_2D ###
utilities_logMes('Initializing DDM')
DDM_3D_Initialize(nb_sdmx,nb_sdmy,nb_sdmz,ddm_type)
utilities_logMes('Set DDM parameters')
DDM_3D_SetParameters(freq_detect,freq_write,freq_last,freq_postpro,freq_display)

#PRPRx_CundallIteration(200)
PRPRx_UseCpF2fExplicitDetection(1.e-3)
PRPRx_ShrinkPolyrFaces(0.05)
PRPRx_LowSizeArrayPolyr(10.)

nlgs_3D_DiagonalResolution()

### Boucle en temps ###
utilities_logMes('Starting Time Loop')

for k in range(1,nb_steps+1,1):
   #
   TimeEvolution_IncrementStep()
   TimeEvolution_DisplayStep()
   
   overall_SelectProxTactors(freq_detect)
   ### Detection grossiere et partionnement en sous-domaines ###
   DDM_3D_Partitioning()

   RBDY3_IncrementStep()
   RBDY3_ComputeFext()
   RBDY3_ComputeBulk()

   ### Ajout de la contribution des sous-domaines 
   ### interconnectes sur les corps d'interface ###
   DDM_3D_AddToFext()

   RBDY3_ComputeFreeVelocity()

   #
   # Functions embedded in SPSPx_SelectProxTactors(), PRPRx_SelectProxTactors()
   # are replaced by functions at the beginning of DDM_3D_ExSolver
   # because of a specificity of DDM current implementation.
   # Removing this specificity should be added to the TODO list !
   #PRPRx_SelectProxTactors()
   #
   # Functions embedded in PRPRx_RecpRloc()
   # are replaced by functions at the beginning of DDM_3D_ExSolver
   # because of a specificity of DDM current implementation.
   # Removing this specificity should be added to the TODO list !
   #PRPRx_RecupRloc()

   DDM_3D_ExSolver(type,quad, tol, relax, gs_it1, gs_it2)

   # Functions embedded in nlgs_UpdateTactBehav()
   # are replaced by functions at the very end of DDM_3D_ExSolver
   #nlgs_UpdateTactBehav()

   PRPRx_StockRloc()
   #
   DDM_3D_ComputeDof()
   #
   TimeEvolution_UpdateStep()
   RBDY3_UpdateDof()
   #
   ### Output : write, out, postpro and last ###
   DDM_3D_Post()

   ### writeout handling ###
   overall_CleanWriteOutFlags()

### LAST en fin de simulation ###
DDM_3D_WriteLast()

### postpro ###
DDM_3D_Finalize()
overall_Finalize()
