
from pylmgc90.chipy import *
from numpy import *
####
# info gestion du temps
dt = 1.e-2
theta = 0.5
nstep = 100

# bavardage de certaines fonctions
echo = 0
#utilities_DisableLogMes()

# info generation fichier visu
freq_display = 1
freq_write = 1
ref_radius = 5.e-2

# info contact

#       123456789012345678901234567890
type = 'Stored_Delassus_Loops         '
#type = 'Exchange_Local_Global         '
quad = 'QM/16'
tol = 0.1666e-5
relax = 0.1 #0.01
gs_mix = 100
gs_it1 = 20*gs_mix
gs_it2 = 10

#nlgs_3D_SetWithQuickScramble()
nlgs_3D_DiagonalResolution()

###
checkDirectories()
###
SetDimension(3,0)

utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)
Integrator_SetContactDetectionConfiguration(1.-theta,0.)

### lecture du modele ###
utilities_logMes('READ BODIES')
MAILx_ReadBodies()
RBDY3_ReadBodies()

utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

utilities_logMes('READ MODELS')
models_ReadModels()

utilities_logMes('INIT MODELS')
# on dimensionne et on initie la construction des mapping
models_InitModels()
ExternalModels_InitModels()

utilities_logMes('LOADS')
# on charge les choses et on construit les mapping
mecaMAILx_LoadModels()
mecaMAILx_LoadBehaviours()
RBDY3_LoadBehaviours()

utilities_logMes('PUSH')
mecaMAILx_PushProperties()
#
utilities_logMes('STORE')
# on finalise la construction des mapping
models_StoreProperties()

utilities_logMes('CHECK')
ExternalModels_CheckProperties()

#
POLYR_LoadTactors()
CSxxx_LoadTactors()

#
utilities_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
mecaMAILx_ReadIniDof()
RBDY3_ReadIniDof()

utilities_logMes('READ INI GPV')
TimeEvolution_ReadIniGPV()
mecaMAILx_ReadIniGPV()

utilities_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
CSPRx_ReadIniVlocRloc()

#
utilities_logMes('READ DRIVEN DOF')
mecaMAILx_ReadDrivenDof()
RBDY3_ReadDrivenDof()

### ecriture paranoiaque du modele ###
utilities_logMes('WRITE BODIES')
overall_WriteBodies()
MAILx_WriteBodies()
RBDY3_WriteBodies()

utilities_logMes('WRITE MODELS')
models_WriteModels()

utilities_logMes('WRITE BEHAVIOURS')
bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

utilities_logMes('WRITE DRIVEN DOF')
overall_WriteDrivenDof()
mecaMAILx_WriteDrivenDof()
RBDY3_WriteDrivenDof()

utilities_logMes('WRITE DOF')
TimeEvolution_WriteLastDof()
mecaMAILx_WriteLastDof()
RBDY3_WriteLastDof()

utilities_logMes('WRITE Vloc_Rloc')
TimeEvolution_WriteLastVlocRloc()

CSPRx_WriteLastVlocRloc()

### definition des parametres du calcul ### 

### post3D ##
OpenDisplayFiles()
OpenPostproFiles()

### parameters setting ###

utilities_logMes('COMPUTE MASS')
mecaMAILx_ComputeMass()
RBDY3_ComputeMass()

utilities_logMes('COMPUTE STIFFNESS')
mecaMAILx_ComputeBulk()
mecaMAILx_ComputeRayleighDamping(0.07,0.07)

####
mecaMAILx_SetPreconAllBodies()
CSxxx_PushPreconNodes()
mecaMAILx_ComputePreconW()

mecaMAILx_AssembKT()

for k in range(1,nstep+1,1):
   #
   utilities_logMes('increment : '+str(k))
   #
   utilities_logMes('INCREMENT STEP')
   TimeEvolution_IncrementStep()
   mecaMAILx_IncrementStep()
   RBDY3_IncrementStep()
   
   utilities_logMes('DISPLAY TIMES')
   TimeEvolution_DisplayStep()

   mecaMAILx_ComputeContactDetectionConfiguration()
   RBDY3_ComputeContactDetectionConfiguration()

   utilities_logMes('SELECT PROX TACTORS')

   overall_SelectProxTactors()
   CSPRx_SelectProxTactors()


   utilities_logMes('COMPUTE Fext')
   mecaMAILx_ComputeFext()
   RBDY3_ComputeFext()

   CSpxx_ApplyPressure(2,1e+6)

   vector = mecaMAILx_GetBodyVector('Fext_', 1)
   vector.shape=[125,3]

   #utilities_logMes('Pression appliquee')
   #i=1
   #for fx,fy,fz in vector:
   #   if i > 100: 
   #     s=str(i)+' '+str(fx)+' '+str(fy) +' '+str(fz)
   #     utilities_logMes(s)
   #   i+=1

   utilities_logMes('COMPUTE Fint')
   mecaMAILx_ComputeBulk()
   RBDY3_ComputeBulk()

   utilities_logMes('ASSEMBLAGE')
   mecaMAILx_AssembRHS()

   utilities_logMes('COMPUTE Free Vlocy')
   mecaMAILx_ComputeFreeVelocity()
   RBDY3_ComputeFreeVelocity()
   #
   #
   CSPRx_RecupRloc()

   utilities_logMes('RESOLUTION') 

   nlgs_3D_SetCheckType(quad, tol, relax)
   nlgs_3D_ExPrep(type)

   for i_block_iter in range(1,gs_it2+1):
      utilities_logMes('it block gs: '+str(i_block_iter))
      #nlgs_3D_QuickScrambleContactOrder()
      if (i_block_iter == 1):
        for i_mix in range(2*gs_it1/gs_mix):
          #nlgs_3D_QuickScrambleContactOrder()
          nlgs_3D_ExIter(gs_mix)

        #nlgs_3D_ExIter(2*gs_it1)
        iconv = nlgs_3D_AfterIterCheck()
        nlgs_3D_DisplayAfterIterCheck()

      else :   
        nlgs_3D_SetCheckType(quad, tol, 1.0)                    
        for i_mix in range(gs_it1/gs_mix):
          #nlgs_3D_QuickScrambleContactOrder()
          nlgs_3D_ExIter(gs_mix)

        #nlgs_3D_ExIter(gs_it1)
        iconv = nlgs_3D_AfterIterCheck()
        nlgs_3D_DisplayAfterIterCheck()
        if (iconv == 0): break

   nlgs_3D_ExPost()


   nlgs_3D_UpdateTactBehav()
   CSPRx_StockRloc()

   #
   utilities_logMes('COMPUTE DOF, FIELDS, etc.')
   mecaMAILx_ComputeDof()
   mecaMAILx_ComputeField()
   RBDY3_ComputeDof()

   utilities_logMes('UPDATE DOF, FIELDS')
   TimeEvolution_UpdateStep()
   mecaMAILx_UpdateDof()
   mecaMAILx_UpdateBulk()
   RBDY3_UpdateDof()
   #
   utilities_logMes('WRITE LAST DOF')
   TimeEvolution_WriteOutDof(freq_write)
   mecaMAILx_WriteOutDof()
   RBDY3_WriteOutDof(-1,100000)
   #
   utilities_logMes('WRITE Vloc_Rloc')
   TimeEvolution_WriteOutVlocRloc(freq_write)
   CSPRx_WriteOutVlocRloc()

   ### post3D ###
   WriteDisplayFiles(freq_display)
   WritePostproFiles()

   ### gestion des writeout ###
   overall_CleanWriteOutFlags()

### postpro ###
CloseDisplayFiles()
ClosePostproFiles()
