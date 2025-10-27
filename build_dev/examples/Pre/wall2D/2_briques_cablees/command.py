
from pylmgc90 import chipy

chipy.checkDirectories()

# desactivation des messages de log
chipy.utilities_DisableLogMes()

### computation's parameters definition ### 
# time step length
dt = 0.001
# value of the parameter of the theta-method
theta = 0.5
# number of time steps
nb_steps = 1000

### parameters setting ###
#   * detection frequency
#   * visualization frequency
freq_display = 10
ref_radius   = 1.e-3
#   * frequence d'ecriture des fichier de sortie
freq_write = 50
#         123456789012345678901234567890
#   * nlgs solver parameters
stype  = 'Stored_Delassus_Loops         '
norm   = 'Quad '
tol    = 0.1666e-3
relax  = 1.0
gs_it1 = 51
gs_it2 = 501

#
chipy.SetDimension(2,1)

chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
#

### model reading ###
chipy.utilities_logMes('READ DATBOX')
chipy.ReadDatbox(deformable=False)
chipy.PT2Dx_SetDisplayRadius(ref_radius)

### post2D ##
chipy.OpenDisplayFiles()
#chipy.OpenPostproFiles()

### compute masses ###
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

# time loop
for k in range(1, nb_steps + 1):
    #
    chipy.utilities_logMes('itere : '+str(k))
    #
    chipy.utilities_logMes('INCREMENT STEP')
    chipy.IncrementStep()

    chipy.utilities_logMes('COMPUTE Fext')
    chipy.ComputeFext()

    chipy.utilities_logMes('COMPUTE Bulk')
    chipy.ComputeBulk()
    
    chipy.utilities_logMes('COMPUTE Free Vlocy')
    chipy.ComputeFreeVelocity()
    #
    chipy.utilities_logMes('SELECT PROX TACTORS')
    chipy.SelectProxTactors()
    #
    chipy.RecupRloc()
    chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
    chipy.StockRloc()
    #
    chipy.utilities_logMes('COMPUTE DOF')
    chipy.ComputeDof()
    #
    chipy.utilities_logMes('UPDATE DOF')
    chipy.UpdateStep()
    #
    chipy.utilities_logMes('WRITE OUT')
    chipy.WriteOut(freq_write)
    #
    ### post2D ###
    chipy.WriteDisplayFiles(freq_display)
    #chipy.WritePostproFiles()

chipy.WriteLastDof()
chipy.WriteLastVlocRloc()

chipy.CloseDisplayFiles()
#chipy.ClosePostproFiles()

chipy.Finalize()
