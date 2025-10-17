
from pylmgc90.chipy import *
from numpy import *

checkDirectories()

### computation's parameters definition ### 

nb_steps = 300
dt = 0.001
theta = 0.5

freq_display = 10

#PRPRx_CundallIteration(200)
PRPRx_UseCpF2fExplicitDetection(1.e-3)
PRPRx_ShrinkPolyrFaces(0.05)
PRPRx_LowSizeArrayPolyr(10)

nlgs_3D_DiagonalResolution()

tol = 0.1666e-3
relax = 1.0
quad = 'QM/16'
stype = 'Stored_Delassus_Loops         '
gs_it1 = 51
gs_it2 = 501


#nlgs_3D_SetWithQuickScramble()

SetDimension(3)
Initialize()

utilities_logMes('INIT TIME STEPPING')
TimeEvolution_SetTimeStep(dt)
Integrator_InitTheta(theta)

RBDY3_NewRotationScheme()

### model reading ###
utilities_logMes('READ BODIES')
ReadBodies()

utilities_logMes('READ BEHAVIOURS')
ReadBehaviours()

#LOADS
LoadBehaviours()

utilities_logMes('READ INI DOF')
ReadIniDof()

LoadTactors()

utilities_logMes('READ INI Vloc Rloc')
ReadIniVlocRloc()

utilities_logMes('READ DRIVEN DOF')
ReadDrivenDof()

utilities_logMes('WRITE BODIES')
WriteBodies()
utilities_logMes('WRITE BEHAVIOURS')
WriteBehaviours()
utilities_logMes('WRITE DRIVEN DOF')
WriteDrivenDof()

### set periodic conditions ###
#RBDY3_SetSourcePointWithIni(2, 5.0, 12.0, 12.0, 4.0)

OpenDisplayFiles()

### compute masses ###
ComputeMass()

for k in range(nb_steps):
    if TimeEvolution_GetStep() == 150:
      RBDY3_PutBodyVector('Vbeg_',RBDY3_GetNbRBDY3(),[2.,0.,0.,0.,0.,0.])
    #
    IncrementStep()
    #
    ComputeFext()
    ComputeBulk()
    ComputeFreeVelocity()
    #
    SelectProxTactors()
    
    RecupRloc()
    ExSolver(stype, quad, tol, relax, gs_it1, gs_it2)
    StockRloc()
    #
    ComputeDof()
    UpdateStep()

    WriteLastDof()
    WriteLastVlocRloc()

    WriteDisplayFiles(freq_display,0.01)

    overall_CleanWriteOutFlags()

WriteLastDof()
WriteLastVlocRloc()

CloseDisplayFiles()

Finalize()
