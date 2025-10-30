
from pylmgc90 import chipy

chipy.checkDirectories()

#chipy.utilities_DisableLogMes()

####
# info gestion du temps
dt = 1.e-2
theta = 0.505
nb_steps = 20

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 1

freq_write = 1

# info contact

#         123456789012345678901234567890
stype  = 'Stored_Delassus_Loops         '
norm   = 'QM/16'
tol    = 1e-5
relax  = 1.0
gs_it1 = 50
gs_it2 = 10

###
chipy.SetDimension(3,0)

### definition des parametres du calcul ### 
chipy.utilities_logMes('INIT TIME STEPPING')

chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

### lecture du modele ###
chipy.ReadDatbox()

### post3D ##
chipy.OpenDisplayFiles()
#chipy.OpenPostproFiles()

chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

chipy.logMes('COMPUTE STIFFNESS')
chipy.ComputeBulk()

# precondensation
chipy.mecaMAILx_SetPreconAllBodies()
chipy.CSxxx_PushPreconNodes()
chipy.ASpxx_PushPreconNodes()
chipy.mecaMAILx_ComputePreconW()

chipy.AssembleMechanicalLHS()

for k in range(1,nb_steps+1,1):
   #
   chipy.utilities_logMes('INCREMENT STEP')
   chipy.IncrementStep()
   
   chipy.utilities_logMes('DISPLAY TIMES')
   chipy.TimeEvolution_DisplayStep()

   chipy.utilities_logMes('COMPUTE Fext')
   chipy.ComputeFext()

   chipy.utilities_logMes('COMPUTE Fint')
   chipy.ComputeBulk()

   chipy.utilities_logMes('ASSEMBLAGE')
   chipy.AssembleMechanicalRHS()

   utilities_logMes('COMPUTE Free Vlocy')
   chipy.ComputeFreeVelocity()
   #
   chipy.utilities_logMes('SELECT PROX TACTORS')
   chipy.SelectProxTactors()
   #
   chip.RecupRloc()

   chip.utilities_logMes('RESOLUTION' )
   chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
   chipy.UpdateTactBehav()
   chipy.StockRloc()
   #
   chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
   chiyp.ComputeDof()
   #
   chipy.utilities_logMes('UPDATE DOF, FIELDS')
   chipy.UpdateStep()
   #
   chipy.utilities_logMes('WRITE out')
   chipy.WriteOut(freq_write)

   ### post3D ###
   chipy.WriteDisplayFiles(freq_display)
   #chipy.WritePostproFiles()

### postpro ###
chipy.CloseDisplayFiles()
#chipy.ClosePostproFiles()
chipy.Finalize()
