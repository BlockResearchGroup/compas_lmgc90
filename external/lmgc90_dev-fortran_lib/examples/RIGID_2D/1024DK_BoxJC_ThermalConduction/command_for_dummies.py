
from pylmgc90.chipy import *

checkDirectories()

# on indique qu'on travaille en deformation planes
SetDimension(2,1)

dt = 0.0002
theta = 0.5
nb_steps_meca=10
nb_steps_ther=10000

tol = 0.1666e-3
relax = 1.0
norm = 'Quad '
gs_it1 = 33
gs_it2 = 101
storage='Stored_Delassus_Loops         '

### definition des parametres du calcul ### 
utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

### lecture du modele ###
utilities_logMes('READ BODIES')
ReadBodies()

utilities_logMes('READ BEHAVIOURS')
ReadBehaviours()

utilities_logMes('LOAD BEHAVIOURS')
LoadBehaviours()

utilities_logMes('LOAD TACTORS')
LoadTactors()

utilities_logMes('READ MP BEHAVIOURS')
ReadMpBehaviours(0,'therm')

utilities_logMes('READ INI DOF')
ReadIniDof()

utilities_logMes('READ INI Vloc Rloc')
ReadIniVlocRloc()

utilities_logMes('READ DRIVEN DOF')
ReadDrivenDof()

### ecriture paranoiaque du modele ###
utilities_logMes('WRITE BODIES')
WriteBodies()
utilities_logMes('WRITE BEHAVIOURS')
WriteBehaviours()
utilities_logMes('WRITE MP BEHAVIOURS')
WriteMpBehaviours()
utilities_logMes('WRITE DRIVEN DOF')
WriteDrivenDof()

postpro_PostproBeforeComputation()

utilities_logMes('COMPUTE MASS')
ComputeMass()

for k in range(0,nb_steps_meca,1):
   #
   IncrementStep()

   ComputeFext()
   ComputeBulk()
   ComputeFreeVelocity()

   SelectProxTactors()

   RecupRloc()
   nlgs_ExSolver(storage,norm,tol,relax,gs_it1,gs_it2)
   StockRloc()

   mp_solver_RecupTemperature()
   mp_solver_SolveThermoProblem()

   ComputeDof()

   UpdateStep()
   #
   ### postpro ###
   postpro_PostproDuringComputation()

RBDY2_NullifyVelocities()

for i in range(0,nb_steps_ther,1):
   #
   IncrementStep()
   mp_solver_RecupTemperature()
   mp_solver_SolveThermoProblem()
   mecaMAILx_ComputeField()
   UpdateStep()

   ### postpro ###
   postpro_PostproDuringComputation()

### postpro ###
postpro_ClosePostproFiles()
