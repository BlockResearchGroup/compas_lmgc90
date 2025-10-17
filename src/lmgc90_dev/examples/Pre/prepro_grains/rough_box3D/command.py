
from pylmgc90 import chipy

chipy.Initialize()
chipy.checkDirectories()

# desactivation des messages de log
#chipy.utilities_DisableLogMes()

# a 3D example is considered
chipy.SetDimension(3)

### computation's parameters definition ### 
chipy.utilities_logMes('INIT TIME STEPPING')
# time step length
dt = 1.e-3
# value of the parameter of the theta-method
theta = 0.5
# number of time steps
nb_steps = 1000

# bavardage de certaines fonctions
echo = 0

### parameters setting ###
#   * detection frequency
#   * visualization frequency
freq_display = 50
#   * frequence d'ecriture des fichier de sortie
freq_write = 50
#         123456789012345678901234567890
#   * nlgs solver parameters
stype  = 'Stored_Delassus_Loops         '
norm   = 'QM/16'
tol    = 0.1666e-3
relax  = 1.0
gs_it1 = 51
gs_it2 = 501

# 
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
#

### model reading ###
chipy.ReadDatbox(deformable=False)

### post ##
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

for k in range(nb_steps):
   #
   chipy.utilities_logMes('itere : '+str(k))
   #
   chipy.utilities_logMes('INCREMENT STEP')
   chipy.IncrementStep()

   chipy.utilities_logMes('COMPUTE Fext')
   chipy.ComputeFext()

   chipy.utilities_logMes('COMPUTE Fint')
   chipy.ComputeBulk()
   
   chipy.utilities_logMes('COMPUTE Free Vlocy')
   chipy.ComputeFreeVelocity()
   #
   chipy.utilities_logMes('SELECT PROX TACTORS')
   chipy.SelectProxTactors()

   chipy.RecupRloc()
   chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
   chipy.StockRloc()
   #
   chipy.utilities_logMes('COMPUTE DOF')
   chipy.ComputeDof()
   #
   chipy.utilities_logMes('WRITE OUT')
   chipy.WriteOut(freq_write)
   #
   ### post ###
   chipy.WritePostproFiles()
   chipy.WriteDisplayFiles(freq_display)

   chipy.utilities_logMes('UPDATE DOF')
   chipy.UpdateStep()
   #

chipy.WriteLastDof()
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

chipy.Finalize()
