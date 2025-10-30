
from pylmgc90.chipy import *

checkDirectories()

### computation's parameters definition ### 

dt = 0.002
nb_steps = 3000 #40000
theta = 0.5

freq_display = 10
ref_radius   = 0.25

xperiode = 25.0
yperiode = 25.0


PRPRx_UseCpCundallDetection(300)
PRPRx_LowSizeArrayPolyr(70)


#PRPRx_VerboseF2F(1,2)

tol = 0.1666e-3
relax = 1.0
quad = 'Maxm '
gs_it1 = 10
gs_it2 = 100

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

SetPeriodicCondition(xperiode,yperiode)

OpenDisplayFiles()
OpenPostproFiles()

### compute masses ###
ComputeMass()

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
    ExSolver('Stored_Delassus_Loops         ',quad, tol, relax, gs_it1, gs_it2)
    StockRloc()
    #
    ComputeDof()
    UpdateStep()

    WriteOutDof(freq_display)
    WriteOutVlocRloc(freq_display)

    WriteDisplayFiles(freq_display)
    WritePostproFiles()

    overall_CleanWriteOutFlags()

WriteLastDof()
WriteLastVlocRloc()

CloseDisplayFiles()
ClosePostproFiles()

Finalize()
