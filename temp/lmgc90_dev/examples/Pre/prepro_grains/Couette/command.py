
from pylmgc90 import chipy

chipy.checkDirectories()

chipy.utilities_DisableLogMes()

####
# info gestion du temps
dt = 1.e-2
theta = 0.5
nb_steps = 100

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 50

# info contact

#         123456789012345678901234567890
stype  = 'Stored_Delassus_Loops         '
norm   = 'Quad '
tol    = 0.1666e-3
relax  = 1.0
gs_it1 = 51
gs_it2 = 1001

chipy.SetDimension(2,1)
### definition des parametres du calcul ### 
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

### lecture du modele ###

### model reading ###
chipy.ReadDatbox(deformable=False)

### post2D ##
chipy.OpenDisplayFiles()

chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

for k in range(1, nb_steps + 1, 1):
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
   chipy.utilities_logMes('WRITE LAST DOF')
   chipy.WriteLastDof()
   chipy.utilities_logMes('WRITE LAST VlocRloc')
   chipy.WriteLastVlocRloc()
   #
   ### post2D ###
   chipy.WriteDisplayFiles(freq_display)

chipy.CloseDisplayFiles()

chipy.Finalize()
