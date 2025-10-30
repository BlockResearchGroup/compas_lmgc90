
from pylmgc90 import chipy
from numpy import *
####
# info gestion du temps
dt = 1.e-2
theta = 0.5
nstep = 100

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 1
freq_write = 1
ref_radius = 5.e-2

# info contact

#        123456789012345678901234567890
stype = 'Stored_Delassus_Loops         '
#stype = 'Exchange_Local_Global         '
quad = 'QM/16'
tol = 0.1666e-5

#1./48 = 0.0208
relax = 0.01

# 1/144 = 0.006
#relax = 0.005 

gs_it1 = 2000
gs_it2 = 10

#nlgs_3D_SetWithQuickScramble()
chipy.nlgs_3D_DiagonalResolution()

###
chipy.checkDirectories()
###
chipy.Initialize()
chipy.SetDimension(3,0)

chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
chipy.Integrator_SetContactDetectionConfiguration(1.-theta,0.)

### lecture du modele ###
chipy.utilities_logMes('READ BODIES')
chipy.MAILx_ReadBodies()
chipy.RBDY3_ReadBodies()

chipy.utilities_logMes('READ BEHAVIOURS')
chipy.bulk_behav_ReadBehaviours()
chipy.tact_behav_ReadBehaviours()

chipy.utilities_logMes('READ MODELS')
chipy.models_ReadModels()

chipy.utilities_logMes('INIT MODELS')
# on dimensionne et on initie la construction des mapping
chipy.models_InitModels()
chipy.ExternalModels_InitModels()

chipy.utilities_logMes('LOADS')
# on charge les choses et on construit les mapping
chipy.mecaMAILx_LoadModels()
chipy.mecaMAILx_LoadBehaviours()
chipy.RBDY3_LoadBehaviours()

chipy.utilities_logMes('PUSH')
chipy.mecaMAILx_PushProperties()
#
chipy.utilities_logMes('STORE')
# on finalise la construction des mapping
chipy.models_StoreProperties()

chipy.utilities_logMes('CHECK')
chipy.ExternalModels_CheckProperties()

#
chipy.POLYR_LoadTactors()
chipy.CSxxx_LoadTactors()

#
chipy.utilities_logMes('READ INI DOF')
chipy.TimeEvolution_ReadIniDof()
chipy.mecaMAILx_ReadIniDof()
chipy.RBDY3_ReadIniDof()

chipy.utilities_logMes('READ INI GPV')
chipy.TimeEvolution_ReadIniGPV()
chipy.mecaMAILx_ReadIniGPV()

chipy.utilities_logMes('READ INI Vloc Rloc')
chipy.TimeEvolution_ReadIniVlocRloc()
chipy.CSPRx_ReadIniVlocRloc()

#
chipy.utilities_logMes('READ DRIVEN DOF')
chipy.mecaMAILx_ReadDrivenDof()
chipy.RBDY3_ReadDrivenDof()

### ecriture paranoiaque du modele ###
chipy.utilities_logMes('WRITE BODIES')
chipy.overall_WriteBodies()
chipy.MAILx_WriteBodies()
chipy.RBDY3_WriteBodies()

chipy.utilities_logMes('WRITE MODELS')
chipy.models_WriteModels()

chipy.utilities_logMes('WRITE BEHAVIOURS')
chipy.bulk_behav_WriteBehaviours()
chipy.tact_behav_WriteBehaviours()

chipy.utilities_logMes('WRITE DRIVEN DOF')
chipy.overall_WriteDrivenDof()
chipy.mecaMAILx_WriteDrivenDof()
chipy.RBDY3_WriteDrivenDof()

chipy.utilities_logMes('WRITE DOF')
chipy.TimeEvolution_WriteLastDof()
chipy.mecaMAILx_WriteLastDof()
chipy.RBDY3_WriteLastDof()

chipy.utilities_logMes('WRITE Vloc_Rloc')
chipy.TimeEvolution_WriteLastVlocRloc()

chipy.CSPRx_WriteLastVlocRloc()

### definition des parametres du calcul ### 

### post3D ##
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

### parameters setting ###

chipy.utilities_logMes('COMPUTE MASS')
chipy.mecaMAILx_ComputeMass()
chipy.RBDY3_ComputeMass()

chipy.utilities_logMes('COMPUTE STIFFNESS')
chipy.mecaMAILx_ComputeBulk()
chipy.mecaMAILx_ComputeRayleighDamping(0.07,0.07)

####
chipy.mecaMAILx_SetPreconAllBodies()
chipy.CSxxx_PushPreconNodes()
chipy.mecaMAILx_ComputePreconW()

chipy.mecaMAILx_AssembKT()

for k in range(1,nstep+1,1):
   #
   chipy.utilities_logMes('increment : '+str(k))
   #
   chipy.utilities_logMes('INCREMENT STEP')
   chipy.TimeEvolution_IncrementStep()
   chipy.mecaMAILx_IncrementStep()
   chipy.RBDY3_IncrementStep()
   
   chipy.utilities_logMes('DISPLAY TIMES')
   chipy.TimeEvolution_DisplayStep()

   chipy.mecaMAILx_ComputeContactDetectionConfiguration()
   chipy.RBDY3_ComputeContactDetectionConfiguration()

   chipy.utilities_logMes('SELECT PROX TACTORS')

   chipy.overall_SelectProxTactors()
   chipy.CSPRx_SelectProxTactors()

   chipy.utilities_logMes('COMPUTE Fext')
   chipy.mecaMAILx_ComputeFext()
   chipy.RBDY3_ComputeFext()

   chipy.CSpxx_ApplyPressure(2,1e+6)

   vector = chipy.mecaMAILx_GetBodyVector('Fext_', 1)
   vector.shape=[125,3]

   #chipy.utilities_logMes('Pression appliquee')
   #i=1
   #for fx,fy,fz in vector:
   #   if i > 100: 
   #     s=str(i)+' '+str(fx)+' '+str(fy) +' '+str(fz)
   #     chipy.utilities_logMes(s)
   #   i+=1

   chipy.utilities_logMes('COMPUTE Fint')
   chipy.mecaMAILx_ComputeBulk()
   chipy.RBDY3_ComputeBulk()

   chipy.utilities_logMes('ASSEMBLAGE')
   chipy.mecaMAILx_AssembRHS()

   chipy.utilities_logMes('COMPUTE Free Vlocy')
   chipy.mecaMAILx_ComputeFreeVelocity()
   chipy.RBDY3_ComputeFreeVelocity()
   #
   #
   chipy.CSPRx_RecupRloc()

   chipy.utilities_logMes('RESOLUTION') 

   chipy.nlgs_3D_SetCheckType(quad, tol, relax)
   chipy.nlgs_3D_ExPrep(stype)

   for i_block_iter in range(1,gs_it2+1):
      chipy.utilities_logMes('it block gs: '+str(i_block_iter))
      #chipy.nlgs_QuickScrambleContactOrder()
      if (i_block_iter == 1):
        chipy.nlgs_3D_ExIterJacobi(2*gs_it1)
        iconv = chipy.nlgs_3D_AfterIterCheckJacobi()
        chipy.nlgs_3D_DisplayAfterIterCheck()
      else :   
        chipy.nlgs_3D_ExIterJacobi(gs_it1)
        iconv = chipy.nlgs_3D_AfterIterCheckJacobi()
        chipy.nlgs_3D_DisplayAfterIterCheck()
        if (iconv == 0): break

   chipy.nlgs_3D_ExPostJacobi()

   chipy.nlgs_3D_UpdateTactBehav()
   chipy.CSPRx_StockRloc()
   #
   chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
   chipy.mecaMAILx_ComputeDof()
   chipy.RBDY3_ComputeDof()

   vector = chipy.mecaMAILx_GetBodyVector('Reac_', 1)
   vector.shape=[125,3]
   coor = chipy.mecaMAILx_GetBodyVector('Coor_', 1)
   coor.shape=[125,3]

   chipy.utilities_logMes('reaction nodale obtenue')
   i=1
   for fx,fy,fz in vector:
      if i > 25: break
      s=str(i)+' '+str(fx)+' '+str(fy)+' '+str(fz)+' '+str(coor[i-1,2])
      chipy.utilities_logMes(s)
      i+=1

   # on initialise raux
   chipy.mecaMAILx_NullifyReac('Raux_',1)

   nb_CSPRx = chipy.inter_handler_3D_getNb( chipy.CSPRx_ID )
   chipy.utilities_logMes('nb CSPRx= '+str(nb_CSPRx))
   all = chipy.inter_handler_3D_getAll( chipy.CSPRx_ID )
   for i in range(nb_CSPRx):
      info = chipy.CSPRx_GetInfo(i+1)
      icsxxx= int(info[2])

      #print icsxxx

      # r = ft t + fn n + fs s
      r = (all[i,13]*all[i,3:6]) + (all[i,14]*all[i,6:9]) + (all[i,15]*all[i,9:12])

      #print r

      #utilities_logMes(str(info[0])+' '+str(info[2]))
      #utilities_logMes('rn('+str(i)+')= '+str(all[i,14]))

      chipy.CSxxx_AddReac('Raux_',icsxxx,r) 

   vector = chipy.mecaMAILx_GetBodyVector('Raux_', 1)
   vector.shape=[125,3]

   chipy.utilities_logMes('reaction auxiliaire obtenue avec re integration')
   i=1
   for fx,fy,fz in vector:
      if i > 25: break
      s=str(i)+' '+str(fx)+' '+str(fy)+' '+str(fz)
      chipy.utilities_logMes(s)
      i+=1

   #
   chipy.utilities_logMes('UPDATE DOF, FIELDS')
   chipy.mecaMAILx_ComputeField()
   chipy.TimeEvolution_UpdateStep()
   chipy.mecaMAILx_UpdateDof()
   chipy.mecaMAILx_UpdateBulk()
   chipy.RBDY3_UpdateDof()
   #
   chipy.utilities_logMes('WRITE LAST DOF')
   chipy.TimeEvolution_WriteOutDof(freq_write)
   chipy.mecaMAILx_WriteOutDof()
   chipy.RBDY3_WriteOutDof(-1,100000)
   #
   chipy.utilities_logMes('WRITE Vloc_Rloc')
   chipy.TimeEvolution_WriteOutVlocRloc(freq_write)
   chipy.CSPRx_WriteOutVlocRloc()

   ### post3D ###
   chipy.WriteDisplayFiles(freq_display)
   chipy.WritePostproFiles()

   ### gestion des writeout ###
   chipy.overall_CleanWriteOutFlags()

### postpro ###
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()
