from __future__ import print_function

import os,sys
from pylmgc90.chipy import *

import sys

overall_DIME(3,0)

# Nombre de decoupe suivant les axes x, y et z
if(len(sys.argv) > 1 ):
  print(sys.argv)
  nb_sdmx = int(sys.argv[1])
  nb_sdmy = int(sys.argv[2])
  nb_sdmz = int(sys.argv[3])
  print(nb_sdmx)
else:
  nb_sdmx  = 1
  nb_sdmy  = 1
  nb_sdmz  = 1
# Type de methode de DDM : 0 -> Schwarz ; 1 -> NSCDD 
ddm_type = 1

# Pilotage des pas de temps
nb_steps = 2000
dt = 1.e-2

# Parametre du schema d'integration
# de la dynamique des corps
theta = 0.5e0

RBDY3_NewRotationScheme()

# Frequence de detection des contacts "grossiers" qui donne
# aussi la frequence de partionnement en sous-domaine
freq_detect  = 1

# Frequence d'ecriture des visu VTK
freq_display = 100
# Frequence d'ecritue des DOF et VlocRloc
freq_write   = 100
# Frequence des sauvegardes des DOF, VlocRloc et timers
freq_last    = 2001
# Frequence d'ecriture dans les fichiers de postpro
freq_postpro = 1

# Pilotage du Gauss-Seisel
#       123456789012345678901234567890
#type = 'Exchange_Local_Global         '
type = 'Stored_Delassus_Loops         '
tol    = 1.e-4
relax  = 1.e0
quad   = 'Maxm '
gs_it1 = 100
gs_it2 = 10

#nlgs_3D_SetWithQuickScramble()

xperiode = 15.
yperiode = 15.


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


RBDY3_SetXPeriodicCondition(xperiode)
RBDY3_SetYPeriodicCondition(yperiode)
SPSPx_SetXPeriodicCondition(xperiode)
SPSPx_SetYPeriodicCondition(yperiode)
post3D_SetXPeriodicCondition(xperiode)
post3D_SetYPeriodicCondition(yperiode)

### Initialisation du module DDM_2D ###
utilities_logMes('Initializing DDM')
DDM_3D_Initialize(nb_sdmx,nb_sdmy,nb_sdmz,ddm_type)
utilities_logMes('Set DDM parameters')
DDM_3D_SetParameters(freq_detect,freq_write,freq_last,freq_postpro,freq_display)

#nlgs_3D_DiagonalResolution()

### Boucle en temps ###
utilities_logMes('Starting Time Loop')

for k in range(1,nb_steps+1,1):
   #
   TimeEvolution_IncrementStep()
   TimeEvolution_DisplayStep()
   
   ### Detection grossiere et partionnement en sous-domaines ###
   overall_SelectProxTactors()
   DDM_3D_Partitioning()
   #DDM_3D_ExperimentalPartitioning()

   RBDY3_IncrementStep()
   RBDY3_ComputeFext()
   RBDY3_ComputeBulk()

   ### Ajout de la contribution des sous-domaines 
   ### interconnectes sur les corps d'interface ###
   DDM_3D_AddToFext()

   RBDY3_ComputeFreeVelocity()

   #
   # Functions embedded in SPSPx_SelectProxTactors(), PRPRx_SelectProxTactors()
   # are replaced by functions at the beginning of DDM_3D_SelectproxTactors
   # because of a specificity of DDM current implementation.
   # Removing this specificity should be added to the TODO list
   # (wich is the meanning of "experimental" DDM_3D routines)  !
   DDM_3D_SelectProxTactors()

   # Experimental, do not acivate yet
   #DDM_3D_ConstructContactListInterface()
   #

   SPSPx_RecupRloc()
   DDM_3D_ExSolver(type,quad, tol, relax, gs_it1, gs_it2)

   # If you want to update tactor behavior
   #nlgs_3D_UpdateTactBehav()

   SPSPx_StockRloc()
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
