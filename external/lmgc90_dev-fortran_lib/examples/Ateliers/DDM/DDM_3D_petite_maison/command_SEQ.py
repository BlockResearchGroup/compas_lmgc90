
from pylmgc90.chipy import *
from numpy import *

checkDirectories()

utilities_EnableLogMes()

overall_DIME(3,0)

# Time parameters
nb_steps = 50
dt = 0.005

# Parametre du schema d'integration
# de la dynamique des corps
theta = 0.5

# info generation fichier visu
freq_display = 10
liste_tactors=['POLYR']
liste_inters=['PRPRx']

freq_write = 10
freq_last  = 100

freq_detect = 501
nlgs_SetWithQuickScramble()

# Pilotage du Gauss-Seisel
#       123456789012345678901234567890
#type = 'Exchange_Local_Global         '
type   = 'Stored_Delassus_Loops         '
tol    = 1.e-4
relax  = 1.0
norm   = 'Quad '
gs_it1 = 50
gs_it2 = 200

utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

### model reading ###
utilities_logMes('READ BODIES')
RBDY3_ReadBodies()
PLANx_LoadTactors()
POLYR_LoadTactors()

utilities_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

#LOADS
RBDY3_LoadBehaviours()

utilities_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
RBDY3_ReadIniDof()

utilities_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
PRPRx_ReadIniVlocRloc()
PRPLx_ReadIniVlocRloc()

utilities_logMes('READ DRIVEN DOF')
RBDY3_ReadDrivenDof()

utilities_logMes('WRITE BODIES')
overall_WriteBodies()
RBDY3_WriteBodies()

utilities_logMes('WRITE BEHAVIOURS')
bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

utilities_logMes('WRITE DRIVEN DOF')
overall_WriteDrivenDof()
RBDY3_WriteDrivenDof()

### post3D ##
postpro_3D_PostproBeforeComputation()

tactors_dict={}
InitTactorsToVTK(liste_tactors,tactors_dict)

inters_dict={}
InitIntersToVTK(liste_inters,inters_dict)

fit = startCollection('tacts.pvd')
fii = startCollection('inters.pvd')

### compute masses ###
utilities_logMes('COMPUTE MASS')
RBDY3_ComputeMass()

#PRPRx_CundallIteration(200)
PRPRx_UseCpF2fExplicitDetection(1.e-3)
PRPRx_ShrinkPolyrFaces(0.05)
PRPRx_LowSizeArrayPolyr(10)

nlgs_3D_DiagonalResolution()

for k in range(nb_steps):
    #
    utilities_logMes('itere : '+str(k))
    #
    utilities_logMes('INCREMENT STEP')
    TimeEvolution_IncrementStep()
    RBDY3_IncrementStep()
    #
    utilities_logMes('COMPUTE Fext')
    RBDY3_ComputeFext()
    #
    utilities_logMes('COMPUTE Fint')
    RBDY3_ComputeBulk()
    # 
    utilities_logMes('COMPUTE Free Vlocy')
    RBDY3_ComputeFreeVelocity()
    #
    utilities_logMes('SELECT PROX TACTORS')
    overall_SelectProxTactors(freq_detect)
    PRPRx_SelectProxTactors()
    PRPLx_SelectProxTactors()
    
    PRPRx_RecupRloc()
    PRPLx_RecupRloc()

    nlgs_3D_ExSolver(type, norm, tol, relax, gs_it1, gs_it2)

    nlgs_3D_UpdateTactBehav()

    PRPRx_StockRloc()
    PRPLx_StockRloc()
    #
    utilities_logMes('COMPUTE DOF')
    RBDY3_ComputeDof()
    #
    utilities_logMes('UPDATE DOF')
    TimeEvolution_UpdateStep()
    RBDY3_UpdateDof()

    #### LAST ###
    #TimeEvolution_WriteLastVlocRloc(freq_last)
    #PRPRx_WriteLastVlocRloc()
    #TimeEvolution_WriteLastDof(freq_last)
    #RBDY3_WriteLastDof()
    ### OUTBOX ###
    TimeEvolution_WriteOutVlocRloc(freq_write)
    PRPRx_WriteOutVlocRloc()
    TimeEvolution_WriteOutDof(freq_write)
    RBDY3_WriteOutDof()

    ### DISPLAY ###
    if k % freq_display == 0:
      utilities_logMes('tact')
      writeTactorsToVTK('./DISPLAY/tacts'+'_'+str(k)+'.vtp',fit,tactors_dict)
      utilities_logMes('inter')
      writeIntersToVTK('./DISPLAY/inters'+'_'+str(k)+'.vtp',fii,inters_dict,1e0)
      utilities_logMes('---')

    ### postpro ###
    postpro_3D_PostproDuringComputation()

    ### writeout handling ###
    overall_CleanWriteOutFlags()

TimeEvolution_WriteLastVlocRloc()
PRPRx_WriteLastVlocRloc()
TimeEvolution_WriteLastDof()
RBDY3_WriteLastDof()

postpro_3D_ClosePostproFiles()
stopCollection(fit)
stopCollection(fii)
