import os,sys
from pylmgc90.chipy import *

overall_DIME(2,0)

# Nombre de decoupe suivant l'axe x et y
nb_sdmx  = 2
nb_sdmy  = 2
# Type de methode de DDM : 0 -> Schwarz ; 1 -> NSCDD 
ddm_type = 0

# Pilotage des pas de temps
nb_steps = 200
dt = 5.e-3

# Parametre du schema d'integration
# de la dynamique des corps
theta = 0.5

# Frequence de detection des contacts "grossiers" qui donne
# aussi la frequence de partionnement en sous-domaine
freq_detect  = 5

# Frequence d'ecriture des visu VTK
freq_display = 10
# Frequence d'ecritue des DOF et VlocRloc
freq_write   = 10
# Frequence des sauvegardes des DOF, VlocRloc et timers
freq_last    = 1000
# Frequence d'ecriture dans les fichiers de postpro
freq_postpro = 1

# Pilotage du Gauss-Seisel
#       123456789012345678901234567890
type = 'Stored_Delassus_Loops         '
#type = 'Exchange_Local_Global         '
tol    = 1.e-4
relax  = 1.0
quad   = 'QM/16'
gs_it1 = 50
gs_it2 = 200

nlgs_SetWithQuickScramble()

### Initialisation de MPI ### 
overall_Initialize()

### WD par sous-domaine ###
DDM_2D_SetDDWorkingDirectory()

### Gestion des messages de log ### 
#utilities_DisableLogMes()
utilities_EnableLogMes()

### Pour ne pas ecrire les donnees ###
### des corps invisibles dans les  ###
### les fichiers par sous-domaine  ###
RBDY2_SkipInvisible()

### Definition des parametres du calcul ### 
utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

### Model reading ###
utilities_logMes('READ BODIES')
RBDY2_ReadBodies()

utilities_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
RBDY2_ReadIniDof()

utilities_logMes('READ DRIVEN DOF')
RBDY2_ReadDrivenDof()

### LOADS ###
DISKx_LoadTactors()

utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()
RBDY2_LoadBehaviours()


utilities_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
DKDKx_ReadIniVlocRloc()

### Ecriture paranoiaque du modele ###
utilities_logMes('WRITE BODIES')
overall_WriteBodies()
RBDY2_WriteBodies()

utilities_logMes('WRITE BEHAVIOURS')
bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

utilities_logMes('WRITE DRIVEN DOF')
overall_WriteDrivenDof()
RBDY2_WriteDrivenDof()

utilities_logMes('COMPUTE MASS')
RBDY2_ComputeMass()

### Initialisation du module DDM_2D ###
utilities_logMes('Initializing DDM')
DDM_2D_Initialize(nb_sdmx,nb_sdmy,ddm_type)
utilities_logMes('Set DDM parameters')
DDM_2D_SetParameters(freq_detect,freq_write,freq_last,freq_postpro,freq_display)

### Boucle en temps ###
utilities_logMes('Starting Time Loop')

for k in range(1,nb_steps+1,1):
   #
   TimeEvolution_IncrementStep()
   TimeEvolution_DisplayStep()
   #
   ### Detection grossiere et partionnement en sous-domaines ###
   overall_SelectProxTactors(freq_detect)
   DDM_2D_Partitioning()
   #
   RBDY2_IncrementStep()
   RBDY2_ComputeFext()
   RBDY2_ComputeBulk()
   #
   ### Ajout de la contribution des sous-domaines 
   ### interconnectes sur les corps d'interface ###
   DDM_2D_AddToFext()
   #
   RBDY2_ComputeFreeVelocity()
   #
   # Functions embedded in DKDKx_SelectProxTactors()
   # are replaced by functions at the beginning of DDM_2D_ExSolver
   # because of a specificity of DDM current implementation.
   # Removing this specificity should be added to the TODO list !
   #DKDKx_SelectProxTactors()
   #
   # Functions embedded in DKDKx_RecpRloc()
   # are replaced by functions at the beginning of DDM_2D_ExSolver
   # because of a specificity of DDM current implementation.
   # Removing this specificity should be added to the TODO list !
   #DKDKx_RecupRloc()
   DDM_2D_ExSolver(type,quad, tol, relax, gs_it1, gs_it2)
   DKDKx_StockRloc()
   #
   DDM_2D_ComputeDof()
   #
   TimeEvolution_UpdateStep()
   RBDY2_UpdateDof()
   #
   ### Output : write, out, postpro and last ###
   DDM_2D_Post()
   ### writeout handling ###
   overall_CleanWriteOutFlags()
   #
### LAST en fin de simulation ###
DDM_2D_WriteLast()

### postpro ###
DDM_2D_Finalize()
overall_Finalize()
