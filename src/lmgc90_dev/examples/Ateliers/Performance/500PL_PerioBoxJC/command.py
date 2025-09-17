
from pylmgc90.chipy import *

# add timer definition and start here

checkDirectories()

### definition des parametres du calcul ### 
dt=5e-4
nb_steps=10
theta=0.5

# peridioc length
lx=1.1

freq_display = 1
freq_write = 1

tol     = 0.1666e-4
relax   = 1.0
norm    = 'QM/16'
gs_it1  = 100
gs_it2  = 200
gs_type ='Stored_Delassus_Loops         '


utilites_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

### lecture du modele ###
utilites_logMes('READ BODIES')
RBDY2_ReadBodies()

utilites_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

#LOADS
POLYG_LoadTactors()
JONCx_LoadTactors()
RBDY2_LoadBehaviours()

utilites_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
RBDY2_ReadIniDof()

utilites_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
PLPLx_ReadIniVlocRloc()
PLJCx_ReadIniVlocRloc()

utilites_logMes('READ DRIVEN DOF')
RBDY2_ReadDrivenDof()

### ecriture paranoiaque du modele ###
utilites_logMes('WRITE BODIES')
overall_WriteBodies()
RBDY2_WriteBodies()

utilites_logMes('WRITE BEHAVIOURS')
bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

utilites_logMes('WRITE DRIVEN DOF')
overall_WriteDrivenDof()
RBDY2_WriteDrivenDof()


### post2D ##
names= ['DOF','Vloc_Rloc']
tacts_dict={}
InitTactorsToVTK(['POLYG','JONCx'],tacts_dict)

inters_dict={}
InitIntersToVTK(['PLPLx','PLJCx'],inters_dict)

fit = startCollection('./DISPLAY/tacts.pvd')
fii = startCollection('./DISPLAY/inters.pvd')

### postpro ###
postpro_PostproBeforeComputation()

#
RBDY2_SetPeriodicCondition(lx)
PLPLx_SetPeriodicCondition(lx)
post2D_SetPeriodicCondition(lx)

RBDY2_ComputeMass()

for k in range(1,nb_steps+1,1):
   #
   TimeEvolution_IncrementStep()
   RBDY2_IncrementStep()
   
   TimeEvolution_DisplayStep()

   RBDY2_ComputeFext()

   RBDY2_ComputeBulk()
   
   RBDY2_ComputeFreeVelocity()
   #
   overall_SelectProxTactors()
   PLPLx_SelectProxTactors()
   PLJCx_SelectProxTactors()
   #
   PLPLx_RecupRloc()
   PLJCx_RecupRloc()
   nlgs_ExSolver(gs_type,norm,tol,relax,gs_it1,gs_it2)
   PLPLx_StockRloc()
   PLJCx_StockRloc()
   #
   RBDY2_ComputeDof()
   #
   TimeEvolution_UpdateStep()
   RBDY2_UpdateDof()
   #
   #TimeEvolution_WriteLastDof()
   #RBDY2_WriteLastDof()
   ##
   #TimeEvolution_WriteLastVlocRloc()
   #PLPLx_WriteLastVlocRloc()
   #PLJCx_WriteLastVlocRloc()
   ### post2D ###
   writeTactorsToVTK('./DISPLAY/tacts'+'_'+str(k)+'.vtp',fit,tacts_dict)
   writeIntersToVTK('./DISPLAY/inters'+'_'+str(k)+'.vtp',fii,inters_dict,2.5e-2)

   ### postpro ###
   postpro_PostproDuringComputation()

   ### gestion des writeout ###
   overall_CleanWriteOutFlags()


### postpro ###
postpro_ClosePostproFiles()

stopCollection(fit)
stopCollection(fii)

# Stop and write timers here

