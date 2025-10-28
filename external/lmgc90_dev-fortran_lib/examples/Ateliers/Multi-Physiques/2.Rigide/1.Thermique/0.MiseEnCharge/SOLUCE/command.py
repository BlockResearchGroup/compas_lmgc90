import os,sys
# chemin vers ChiPy

from pylmgc90.chipy import *

checkDirectories()

# desactivation des messages de log
utilities_DisableLogMes()

####
# info gestion du temps
dt = 4.e-4
theta = 0.5
nb_steps = 500

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 50
ref_radius = 0.1e-2

# info contact

#       123456789012345678901234567890
type = 'Stored_Delassus_Loops         '
norm = 'Quad '
tol = 0.1666e-3
relax = 1.0
gs_it1 = 51
gs_it2 = 1001

SetDimension(2)

### definition des parametres du calcul ### 
utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

### lecture du modele ###

### model reading ###
utilities_logMes('READ BODIES')
RBDY2_ReadBodies()

utilities_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
RBDY2_ReadIniDof()

utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

#LOADS
DISKx_LoadTactors()
JONCx_LoadTactors()
RBDY2_LoadBehaviours()

utilities_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
DKDKx_ReadIniVlocRloc()
DKJCx_ReadIniVlocRloc()

utilities_logMes('READ DRIVEN DOF')
RBDY2_ReadDrivenDof()

### ecriture paranoiaque du modele ###
utilities_logMes('WRITE BODIES')
overall_WriteBodies()
RBDY2_WriteBodies()

utilities_logMes('WRITE BEHAVIOURS')
bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

utilities_logMes('WRITE DRIVEN DOF')
overall_WriteDrivenDof()
RBDY2_WriteDrivenDof()

### post2D ##
OpenDisplayFiles()

### postpro ###
postpro_PostproBeforeComputation()

#
utilities_logMes('COMPUTE MASS')
RBDY2_ComputeMass()

for k in range(1, nb_steps + 1, 1):
   #
   utilities_logMes('itere : '+str(k))
   #
   utilities_logMes('INCREMENT STEP')
   TimeEvolution_IncrementStep()
   RBDY2_IncrementStep()

   utilities_logMes('DISPLAY TIMES')
   TimeEvolution_DisplayStep()

   utilities_logMes('COMPUTE Fext')
   RBDY2_ComputeFext()

   utilities_logMes('COMPUTE Fint')
   RBDY2_ComputeBulk()
   
   utilities_logMes('COMPUTE Free Vlocy')
   RBDY2_ComputeFreeVelocity()
   #
   utilities_logMes('SELECT PROX TACTORS')
   overall_SelectProxTactors()
   DKJCx_SelectProxTactors()
   DKDKx_SelectProxTactors()
   #
   DKJCx_RecupRloc()
   DKDKx_RecupRloc()
   nlgs_ExSolver(type, norm, tol, relax, gs_it1, gs_it2)
   DKJCx_StockRloc()
   DKDKx_StockRloc()
   #
   utilities_logMes('COMPUTE DOF')
   RBDY2_ComputeDof()
   #
   utilities_logMes('UPDATE DOF')
   TimeEvolution_UpdateStep()
   RBDY2_UpdateDof()
   #
   utilities_logMes('WRITE LAST DOF')
   TimeEvolution_WriteLastDof()
   RBDY2_WriteLastDof()
   #
   utilities_logMes('WRITE LAST Vloc Rloc')
   TimeEvolution_WriteLastVlocRloc()
   DKDKx_WriteLastVlocRloc()
   DKJCx_WriteLastVlocRloc()
   #
   ### post2D ###
   WriteDisplayFiles(freq_display)

   ### postpro ###
   postpro_PostproDuringComputation()

   ### wrtieout handling ###
   overall_CleanWriteOutFlags()

### postpro ###
CloseDisplayFiles()
postpro_ClosePostproFiles()
