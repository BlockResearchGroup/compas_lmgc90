
from pylmgc90 import chipy

chipy.checkDirectories()

### computation's parameters definition ### 

dt = 0.002
nb_steps = 3000
theta = 0.5

freq_write = 10

freq_display = 10

xperiode = 25.0
yperiode = 25.0


chipy.PRPRx_UseCpCundallDetection(300)
chipy.PRPRx_LowSizeArrayPolyr(70)

chipy.PRPRx_VerboseF2F(1,2)

tol = 0.1666e-3
relax = 1.0
quad = 'Maxm '
gs_it1 = 10
gs_it2 = 100

#chipy.nlgs_3D_SetWithQuickScramble()

chipy.SetDimension(3)

chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

chipy.RBDY3_NewRotationScheme()

### model reading ###
chipy.utilities_logMes('READ BODIES')
chipy.ReadDatbox()

### set periodic conditions ###
#chipy.RBDY3_SetSourcePointWithIni(2, 5.0, 12.0, 12.0, 4.0)

chipy.SetPeriodicCondition(xperiode,yperiode)

chipy.OpenDisplayFiles()#write_f2f=True)
chipy.OpenPostproFiles()

### compute masses ###
chipy.ComputeMass()

for k in range(nb_steps):
    #
    chipy.IncrementStep()
    #
    #
    chipy.ComputeFext()
    chipy.ComputeBulk()
    chipy.ComputeFreeVelocity()
    #
    chipy.SelectProxTactors()
    
    chipy.RecupRloc()
    chipy.ExSolver('Stored_Delassus_Loops         ',quad, tol, relax, gs_it1, gs_it2)
    chipy.StockRloc()
    #
    chipy.ComputeDof()
    chipy.UpdateStep()

    chipy.WriteOut(freq_write)

    chipy.WriteDisplayFiles(freq_display)
    chipy.WritePostproFiles()

chipy.WriteLast()

chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

chipy.Finalize()
