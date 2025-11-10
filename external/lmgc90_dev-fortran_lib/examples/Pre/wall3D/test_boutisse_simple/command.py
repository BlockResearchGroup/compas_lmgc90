
from pylmgc90 import chipy

chipy.checkDirectories()

chipy.utilities_DisableLogMes()

chipy.SetDimension(3)

### computation's parameters definition ### 
nb_iter = 100
dt = 0.0001
theta = 0.5

tol    = 0.1666e-3
relax  = 1.0
stype  = 'Stored_Delassus_Loops         '
norm   = 'QM/16'
gs_it1 = 50
gs_it2 = 500

freq_display = 10

chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

### model reading ###
chipy.ReadDatbox(deformable=False)

### post3D ##
chipy.OpenDisplayFiles()

#chipy.OpenPostproFiles()

#chipy.PRPRx_CundallIteration(200)
chipy.PRPRx_UseCpF2fExplicitDetection(1.e-3)
chipy.PRPRx_ShrinkPolyrFaces(0.05)
chipy.PRPRx_LowSizeArrayPolyr(10)

chipy.nlgs_3D_DiagonalResolution()

### compute masses ###
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

for k in range(nb_iter):
    #
    chipy.utilities_logMes('INCREMENT STEP')
    chipy.IncrementStep()
    #
    chipy.utilities_logMes('COMPUTE Fext')
    chipy.ComputeFext()
    #
    chipy.utilities_logMes('COMPUTE Fint')
    chipy.ComputeBulk()
    # 
    chipy.utilities_logMes('COMPUTE Free Vlocy')
    chipy.ComputeFreeVelocity()
    #
    chipy.utilities_logMes('SELECT PROX TACTORS')
    chipy.SelectProxTactors()
    #
    chipy.RecupRloc()
    
    chipy.utilities_logMes('RESOLUTION' )
    chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
    chipy.UpdateTactBehav()

    chipy.StockRloc()
    
    #
    chipy.utilities_logMes('COMPUTE DOF')
    chipy.ComputeDof()
    #
    chipy.utilities_logMes('UPDATE DOF')
    chipy.UpdateStep()

    ### post3D ###
    chipy.WriteDisplayFiles(freq_display)

    ### postpro ###
    #chipy.WritePostproFiles()

    chipy.WriteOut()

chipy.WriteLastDof()

chipy.CloseDisplayFiles()
#chipyClosePostproFiles()

chipy.Finalize()
