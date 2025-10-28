import os,sys

from pylmgc90.chipy import *

checkDirectories()

### definition des parametres du calcul ### 

dt=0.01
theta=0.5
nb_steps_meca=10
nb_steps_ther=20000

# info generation fichier visu
freq_display = 200
ref_radius = 0.1e-2
liste_tactors=['DISKx','JONCx']
liste_inters=['DKDKx','DKJCx']

type = 'Stored_Delassus_Loops         '
quad = 'Quad '
tol = 1.666e-5
relax = 1.
it1 = 33
it2 = 101

RBDY2_SetSurfaceSectors(1)

utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

utilities_logMes('READ BODIES')
RBDY2_ReadBodies()

utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

utilities_logMes('LOAD BULK & TACTORS')
RBDY2_LoadBehaviours()
DISKx_LoadTactors()
JONCx_LoadTactors()

mp_solver_ReadMpBehaviour()

RBDY2_MP_LoadBehaviour(0.,'therm')
#RBDY2_MP_LoadBehaviours(0.,'sener')

RBDY2_ComputeMass()

utilities_logMes('READ INITIAL STATE')
TimeEvolution_ReadIniDof()
RBDY2_ReadIniDof()

TimeEvolution_ReadIniVlocRloc()
DKDKx_ReadIniVlocRloc()
DKJCx_ReadIniVlocRloc()

RBDY2_ReadDrivenDof()

utilities_logMes('WRITE DATBOX COPY')
overall_WriteBodies()
RBDY2_WriteBodies()

tact_behav_WriteBehaviours()
bulk_behav_WriteBehaviours()
mp_solver_WriteMpBehaviour()

utilities_logMes('INIT POSTPRO')
postpro_PostproBeforeComputation()

#
utilities_logMes('COMPUTE MASS')
RBDY2_ComputeMass()

##################

tactors_dict={}
InitTactorsToVTK(liste_tactors,tactors_dict)

inters_dict={}
InitIntersToVTK(liste_inters,inters_dict)

fit = startCollection('DISPLAY/tacts.pvd')
fii = startCollection('DISPLAY/inters.pvd')

##################

utilities_logMes('START THERMO-MECA LOOP')

for k in range(1,nb_steps_meca+1,1):
   #
   utilities_logMes('INCREMENT STEP')
   TimeEvolution_IncrementStep()
   RBDY2_IncrementStep()
   
   utilities_logMes('DISPLAY TIME')
   TimeEvolution_DisplayStep()

   utilities_logMes('COMPUTE RHS')
   RBDY2_ComputeFext()
   RBDY2_ComputeBulk()
   RBDY2_ComputeFreeVelocity()

   utilities_logMes('CONTACT DETECTION')
   overall_SelectProxTactors()
   DKDKx_SelectProxTactors()
   DKJCx_SelectProxTactors()

   utilities_logMes('CONTACT RESOLUTION')
   DKDKx_RecupRloc()
   DKJCx_RecupRloc()
   nlgs_ExSolver(type,quad,tol,relax,it1,it2)
   DKDKx_StockRloc()
   DKJCx_StockRloc()

   utilities_logMes('THERMAL RESOLUTION')
   mp_solver_RecupTemperature()
   mp_solver_SolveThermoProblem()

   RBDY2_ComputeDof()

   TimeEvolution_UpdateStep()
   RBDY2_UpdateDof()

   postpro_PostproDuringComputation()
   overall_CleanWriteOutFlags()

#
RBDY2_NullifyVelocities()

utilities_logMes('THERMAL ONLY')

for i in range(nb_steps_ther+1):
   TimeEvolution_IncrementStep()
   RBDY2_IncrementStep()

   mp_solver_SolveThermoProblem()
   TimeEvolution_UpdateStep()
   RBDY2_UpdateDof()

   postpro_PostproDuringComputation()
   
   ### viz ###
   if i % freq_display == 0:
     utilities_logMes('tact')
     writeTactorsToVTK('./DISPLAY/tacts'+'_'+str(i)+'.vtp',fit,tactors_dict)
     utilities_logMes('inter')
     writeIntersToVTK('./DISPLAY/inters'+'_'+str(i)+'.vtp',fii,inters_dict)
     utilities_logMes('---')

   ### gestion des writeout ###
   overall_CleanWriteOutFlags()
#
utilities_logMes('WRITE LAST THERMAL DOF')
overall_WriteOutMpValues(1)
mp_solver_WriteOutMpValues()
#
postpro_ClosePostproFiles()
