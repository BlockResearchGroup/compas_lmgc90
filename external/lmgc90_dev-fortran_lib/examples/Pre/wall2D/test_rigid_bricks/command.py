
from pylmgc90 import chipy

chipy.checkDirectories()

### lecture du modele ###

chipy.utilities_DisableLogMes()

### definition des parametres du calcul ### 

dt = 1.e-3
theta = 0.5

tol  = 0.1666e-3
relax  = 1.0
stype  = 'Stored_Delassus_Loops         '
norm   = 'Quad '
gs_it1 = 51
gs_it2 = 1001
freq_display = 50


chipy.SetDimension(2)

chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

### model reading ###
chipy.utilities_logMes('READ DATBOX')
chipy.ReadDatbox(deformable=False)

### post2D ##
chipy.OpenDisplayFiles()

chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

for k in range(1,1001,1):
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
   #chipy.utilities_logMes('WRITE OUT')
   #chipy.WriteOut()
   #
   ### post2D ###
   chipy.WriteDisplayFiles(freq_display)
   #chipy.WritePostproFiles()
   ### writeout handling ###

chipy.WriteLastDof()
chipy.WriteLastVlocRloc()

chipy.CloseDisplayFiles()
#chipy.ClosePostproFiles()
chipy.Finalize()
