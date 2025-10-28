
from pylmgc90.chipy import *

from numpy import *

checkDirectories()

# desactivation des messages de log
#utilities_EnableLogMes()
utilities_DisableLogMes()

####
# info gestion du temps
dt = 5.e-3
nb_steps = 200

# Parametre du schema d'integration
# de la dynamique des corps
theta = 0.5

# info generation fichier visu
freq_display = 10
liste_tactors=['DISKx','JONCx']
liste_inters=['DKDKx','DKJCx']

# info contact
freq_detect = 10
nlgs_SetWithQuickScramble()

#       123456789012345678901234567890
type = 'Stored_Delassus_Loops         '
norm = 'QM/16'
tol = 1.e-4
relax = 1.0
gs_it1 = 50
gs_it2 = 200

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
RBDY2_LoadBehaviours()

utilities_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
DKDKx_ReadIniVlocRloc()

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

utilities_logMes('COMPUTE MASS')
RBDY2_ComputeMass()

### post2D ##
postpro_PostproBeforeComputation()

tactors_dict={}
InitTactorsToVTK(liste_tactors,tactors_dict)

inters_dict={}
InitIntersToVTK(liste_inters,inters_dict)

fit = startCollection('tacts.pvd')
fii = startCollection('inters.pvd')

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
   overall_SelectProxTactors(freq_detect)
   DKDKx_SelectProxTactors()
   #
   DKDKx_RecupRloc()
   nlgs_ExSolver(type, norm, tol, relax, gs_it1, gs_it2)
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
   #
   ### viz ###
   if k % freq_display == 0:
     utilities_logMes('tact')
     writeTactorsToVTK('./DISPLAY/tacts'+'_'+str(k)+'.vtp',fit,tactors_dict)
     utilities_logMes('inter')
     writeIntersToVTK('./DISPLAY/inters'+'_'+str(k)+'.vtp',fii,inters_dict,1e0)
     utilities_logMes('---')
   ### postpro ###
   postpro_PostproDuringComputation()
   ### wrtieout handling ###
   overall_CleanWriteOutFlags()

utilities_logMes('WRITE LAST Vloc Rloc')
TimeEvolution_WriteLastVlocRloc()
DKDKx_WriteLastVlocRloc()
#
postpro_ClosePostproFiles()
stopCollection(fit)
stopCollection(fii)
