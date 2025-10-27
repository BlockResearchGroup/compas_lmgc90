import sys

from pylmgc90 import chipy

chipy.checkDirectories()

# desactivation des messages de log
chipy.utilities_DisableLogMes()

# a 3D example is considered
chipy.SetDimension(3)

### computation's parameters definition ### 
chipy.utilities_logMes('INIT TIME STEPPING')
# time step length
dt = 1.e-4
# value of the parameter of the theta-method
theta = 0.5
# number of time steps
nb_steps = 15000

# interaction parameters
Rloc_tol = 5.e-2

# bavardage de certaines fonctions
echo = 0

### parameters setting ###
#   * detection frequency
#   * visualization frequency
freq_display = 100
#   * frequence d'ecriture des fichier de sortie
freq_write = 100
h5_file    = 'lmgc90.h5'
#       123456789012345678901234567890
#   * nlgs solver parameters
stype  = 'Stored_Delassus_Loops         '
norm   = 'QM/16'
tol    = 1e-4
relax  = 1.0
gs_it1 = 101
gs_it2 = 20

lx = 1.0
ly = 1.0

chipy.nlgs_3D_DiagonalResolution()

chipy.PRPRx_UseCpCundallDetection(50)

# 
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

### model reading ###
chipy.ReadDatbox(deformable=False)
chipy.InitHDF5(h5_file)

### compute masses ###
chipy.ComputeMass()

### post3D ##
chipy.OpenDisplayFiles()

# compute of a first visualization
#chipy.WriteOutDisplayFile()

# time loop
for k in range(1, nb_steps + 1):
    #
    chipy.utilities_logMes('itere : '+str(k))
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

    chipy.StockRloc()
    #
    chipy.utilities_logMes('COMPUTE DOF')
    chipy.ComputeDof()
    #
    chipy.utilities_logMes('UPDATE DOF')
    chipy.UpdateStep()

    ### post3D ###
    chipy.WriteDisplayFiles(freq_display)

    chipy.WriteOut(freq_write)
    chipy.checkInteractiveCommand()

chipy.WriteLastDof()
chipy.CloseDisplayFiles()

chipy.Finalize()
