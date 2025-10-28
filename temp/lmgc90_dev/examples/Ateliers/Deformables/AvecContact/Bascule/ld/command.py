
from pylmgc90.chipy import *
from numpy import *
####

utilities_DisableLogMes()
# info gestion du temps
dt = 1.e-3
theta = 0.50
nstep = 2000

# driving time subdivision scheme
dt_max=1.e-3
dt_min=1.e-3
t_final = 2.
NR_tol=1e-4
NR_nb_iter_max = 20

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 250
freq_write = 1000
ref_radius = 5.e-2

# info contact

#       123456789012345678901234567890
type = 'Stored_Delassus_Loops         '
#type = 'Exchange_Local_Global         '
quad = 'QM/16'
tol = 0.1666e-3
relax = 0.2
gs_it1 = 500
gs_it2 = 10

nlgs_3D_SetWithQuickScramble()
nlgs_3D_DiagonalResolution()

###
checkDirectories()
###
SetDimension(3,0)

utilities_logMes( 'INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

#
NewtonRaphson_SetFinalTime(t_final)
NewtonRaphson_SetMinTimeStep(dt_min)
NewtonRaphson_SetMaxTimeStep(dt_max)
NewtonRaphson_SetIncPatience(99999)
NewtonRaphson_SetGoodIter(3)
NewtonRaphson_SetBadIter(15)
NewtonRaphson_SetMaxIter(NR_nb_iter_max)
#

### lecture du modele ###
utilities_logMes( 'READ BODIES')
MAILx_ReadBodies()
RBDY3_ReadBodies()

utilities_logMes( 'READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

utilities_logMes( 'READ MODELS')
models_ReadModels()

utilities_logMes( 'INIT MODELS')
# on dimensionne et on initie la construction des mapping
models_InitModels()
ExternalModels_InitModels()

utilities_logMes( 'LOADS')
# on charge les choses et on construit les mapping
mecaMAILx_LoadModels()
mecaMAILx_LoadBehaviours()
RBDY3_LoadBehaviours()

utilities_logMes( 'PUSH')
mecaMAILx_PushProperties()
#
utilities_logMes( 'STORE')
# on finalise la construction des mapping
models_StoreProperties()

utilities_logMes( 'CHECK')
ExternalModels_CheckProperties()

#
POLYR_LoadTactors()
CSxxx_LoadTactors()
ASpxx_LoadTactors()

#
utilities_logMes( 'READ INI DOF')
TimeEvolution_ReadIniDof()
mecaMAILx_ReadIniDof()
RBDY3_ReadIniDof()

utilities_logMes( 'READ INI GPV')
TimeEvolution_ReadIniGPV()
mecaMAILx_ReadIniGPV()

utilities_logMes( 'READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
CSPRx_ReadIniVlocRloc()
CSASp_ReadIniVlocRloc()

#
utilities_logMes( 'READ DRIVEN DOF')
mecaMAILx_ReadDrivenDof()
RBDY3_ReadDrivenDof()

### ecriture paranoiaque du modele ###
utilities_logMes( 'WRITE BODIES')
overall_WriteBodies()
MAILx_WriteBodies()
RBDY3_WriteBodies()

utilities_logMes( 'WRITE MODELS')
models_WriteModels()

utilities_logMes( 'WRITE BEHAVIOURS')
bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

utilities_logMes( 'WRITE DRIVEN DOF')
overall_WriteDrivenDof()
mecaMAILx_WriteDrivenDof()
RBDY3_WriteDrivenDof()

utilities_logMes( 'WRITE DOF')
TimeEvolution_WriteLastDof()
mecaMAILx_WriteLastDof()
RBDY3_WriteLastDof()

utilities_logMes( 'WRITE Vloc_Rloc')
TimeEvolution_WriteLastVlocRloc()
CSASp_WriteLastVlocRloc()
CSPRx_WriteLastVlocRloc()

### definition des parametres du calcul ### 

### post3D ##
OpenDisplayFiles()
OpenPostproFiles()

### parameters setting ###

utilities_logMes( 'COMPUTE MASS')
mecaMAILx_ComputeMass()
RBDY3_ComputeMass()

for k in range(1,nstep+1,1):
   #
   utilities_logMes( 'INCREMENT STEP')
   TimeEvolution_IncrementStep()
   mecaMAILx_IncrementStep()
   RBDY3_IncrementStep()
   
   utilities_logMes( 'DISPLAY TIMES')
   TimeEvolution_DisplayStep()

   utilities_logMes( 'COMPUTE Fext')
   mecaMAILx_ComputeFext()
   RBDY3_ComputeFext()

   RBDY3_ComputeBulk()

   RBDY3_ComputeFreeVelocity()

   # Newton loop
   NewtonRaphson_Initialize(NR_tol)
   kk=0
   is_converged = 1
   while is_converged == 1 : #looping until something changes in CheckNlConvergence
            
     kk+=1

     utilities_logMes( 'iteration NR: '+str(kk))

     utilities_logMes( 'COMPUTE Fint')
     mecaMAILx_ComputeBulk()
     mecaMAILx_ComputeRayleighDamping(0.,0.0001)

     utilities_logMes( 'ASSEMBLAGE')
     mecaMAILx_AssembRHS()
     mecaMAILx_AssembKT()

     utilities_logMes( 'COMPUTE Free Vlocy')
     mecaMAILx_ComputeFreeVelocity()

     utilities_logMes( 'SELECT PROX TACTORS')
     overall_SelectProxTactors()
     CSPRx_SelectProxTactors()
     CSASp_SelectProxTactors()

     CSASp_RecupRloc()
     CSPRx_RecupRloc()

     utilities_logMes( 'RESOLUTION') 
     #utilities_logMes( type,quad,tol, relax, gs_it1, gs_it2 
     nlgs_3D_ExSolver(type,quad, tol, relax, gs_it1, gs_it2)
    
     CSASp_StockRloc()
     CSPRx_StockRloc()
     #
     utilities_logMes( 'COMPUTE DOF, FIELDS, etc.')
     mecaMAILx_ComputeDof()

     if kk > 1:
       norm = mecaMAILx_ComputeResidueNorm()
       ### 0=cv  2=dv
       is_converged = NewtonRaphson_CheckConvergence(norm) 

     ### end while

     ### istate = 1 redo, istate = 2 stop 
     istate = NewtonRaphson_ComputeTimeStep()

   if not istate == 1 :

     nlgs_3D_UpdateTactBehav()
     CSASp_StockRloc()
     CSPRx_StockRloc()

     RBDY3_ComputeDof()

     mecaMAILx_ComputeField()
     #
     utilities_logMes( 'UPDATE DOF, FIELDS')
     TimeEvolution_UpdateStep()
     mecaMAILx_UpdateDof()
     mecaMAILx_UpdateBulk()
     RBDY3_UpdateDof()
     #
     utilities_logMes( 'WRITE LAST DOF')
     TimeEvolution_WriteOutDof(freq_write)
     mecaMAILx_WriteOutDof()
     RBDY3_WriteOutDof(-1,100000)
     #
     utilities_logMes( 'WRITE Vloc_Rloc')
     TimeEvolution_WriteOutVlocRloc(freq_write)
     CSPRx_WriteOutVlocRloc()
     CSASp_WriteOutVlocRloc()

     ### post3D ###
     WriteDisplayFiles(freq_display)
     WritePostproFiles()

     ### gestion des writeout ###
     overall_CleanWriteOutFlags()

     if istate == 2 :
       break

### postpro ###
CloseDisplayFiles()
ClosePostproFiles()
