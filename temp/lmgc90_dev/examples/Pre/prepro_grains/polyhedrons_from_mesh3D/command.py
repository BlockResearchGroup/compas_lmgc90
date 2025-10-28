
from pylmgc90 import chipy

chipy.Initialize()
chipy.checkDirectories()

# suppress logging
chipy.utilities_DisableLogMes()

####
# time management
dt         = 1.e-1
theta      = 0.5
nb_steps   = 10

# output visualization files
freq_display = 1

# output file (used if possible)
freq_write = 1
h5_file    = 'lmgc90.h5'

# contact solver parameters

#         123456789012345678901234567890
stype  = 'Stored_Delassus_Loops         '
norm   = 'Quad '
tol    = 1e-4
relax  = 1.0
gs_it1 = 100
gs_it2 = 20

chipy.SetDimension(3)

chipy.PRPRx_UseCpCundallDetection(10)
#chipy.PRPRx_LowSizeArrayPolyr(70)

### setting time integrator parameters ### 
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

### model reading ###
chipy.ReadDatbox(deformable=False)

### post ##
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()
chipy.InitHDF5(h5_file)

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

chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

chipy.Finalize()
