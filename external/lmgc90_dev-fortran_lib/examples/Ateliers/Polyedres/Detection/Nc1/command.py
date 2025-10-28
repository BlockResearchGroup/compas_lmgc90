import os,sys

from pylmgc90.chipy import *
from numpy import *

checkDirectories()

### computation's parameters definition ### 
dt = 1e-3
nb_steps = 10000 

theta = 0.5

freq_display = 100 

POLYR_SkipAutomaticReorientation()
POLYR_TopologyAngle(90.)
POLYR_FlatnessAngle(0.)

PRPRx_UseNcDetection(0.1)
PRPRx_WithNodalContact()

PRPRx_LowSizeArrayPolyr(4000)

norm='Stored_Delassus_Loops         '
tol = 1e-5
relax = 1.0
quad = 'Maxm '
gs_it1 = 200
gs_it2 = 10

nlgs_3D_DiagonalResolution()

SetDimension(3,0)

utilites_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

### model reading ###
utilites_logMes('READ BODIES')
RBDY3_ReadBodies()
PLANx_LoadTactors()
POLYR_LoadTactors()

utilites_logMes('READ BEHAVIOURS')
bulk_behav_ReadBehaviours()
tact_behav_ReadBehaviours()

#LOADS
RBDY3_LoadBehaviours()

utilites_logMes('READ INI DOF')
TimeEvolution_ReadIniDof()
RBDY3_ReadIniDof()

utilites_logMes('READ INI Vloc Rloc')
TimeEvolution_ReadIniVlocRloc()
PRPLx_ReadIniVlocRloc()
PRPRx_ReadIniVlocRloc()

utilites_logMes('READ DRIVEN DOF')
RBDY3_ReadDrivenDof()

overall_WriteBodies()
RBDY3_WriteBodies()

bulk_behav_WriteBehaviours()
tact_behav_WriteBehaviours()

overall_WriteDrivenDof()
RBDY3_WriteDrivenDof()

### post3D ##
OpenDisplayFiles()
OpenPostproFiles()

### compute masses ###
RBDY3_ComputeMass()

for k in range(nb_steps):
    #
    utilites_logMes('INCREMENT STEP')
    TimeEvolution_IncrementStep()
    RBDY3_IncrementStep()
    #
    utilites_logMes('DISPLAY TIMES')
    TimeEvolution_DisplayStep()
    #
    utilites_logMes('COMPUTE Fext')
    RBDY3_ComputeFext()
    #
    utilites_logMes('COMPUTE Fint')
    RBDY3_ComputeBulk()
    # 
    utilites_logMes('COMPUTE Free Vlocy')
    RBDY3_ComputeFreeVelocity()
    #
    utilites_logMes('SELECT PROX TACTORS')
    overall_SelectProxTactors()

    PRPLx_SelectProxTactors()
    PRPRx_SelectProxTactors()
    
    PRPLx_RecupRloc()
    PRPRx_RecupRloc()

    nlgs_3D_ExSolver(norm,quad, tol, relax, gs_it1, gs_it2)

    PRPLx_StockRloc()
    PRPRx_StockRloc()
    #
    utilites_logMes('COMPUTE DOF')
    RBDY3_ComputeDof()
    #
    utilites_logMes('UPDATE DOF')
    TimeEvolution_UpdateStep()
    RBDY3_UpdateDof()

    utilities_logMes('WRITE OUT Vloc Rloc')
    TimeEvolution_WriteOutVlocRloc(freq_display)
    PRPLx_WriteOutVlocRloc()
    PRPRx_WriteOutVlocRloc()
    
    ### post3D ###
    WriteDisplayFiles(freq=freq_display)
    WritePostproFiles()

    ### writeout handling ###
    overall_CleanWriteOutFlags()

CloseDisplayFiles()
ClosePostproFiles()
