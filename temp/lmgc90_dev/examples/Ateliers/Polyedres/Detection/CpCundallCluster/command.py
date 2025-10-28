
from pylmgc90.chipy import *

checkDirectories()

### computation's parameters definition ### 

dt = 0.01
nb_steps = 100
theta = 0.5

freq_display = 1
ref_radius   = 0.01

PRPRx_UseCpCundallDetection(300)
PRPRx_LowSizeArrayPolyr(70)


#PRPRx_VerboseF2F(1,2)

tol    = 0.1666e-3
relax  = 1.0
stype  = 'Stored_Delassus_Loops         '
quad   = 'Maxm '
gs_it1 = 20
gs_it2 = 10

#nlgs_3D_SetWithQuickScramble()

SetDimension(3)

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
OpenPostproFiles()

### compute masses ###
ComputeMass()
RBDY3_PutBodyVector('Xbeg_', 3, [0., 0., 0.3, 0., 0., 0.])

for k in range(nb_steps):
    #
    IncrementStep()
    #
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

    WriteDisplayFiles(freq_display)
    WritePostproFiles()

    overall_CleanWriteOutFlags()

WriteLastDof()
WriteLastVlocRloc()

CloseDisplayFiles()
ClosePostproFiles()

Finalize()
