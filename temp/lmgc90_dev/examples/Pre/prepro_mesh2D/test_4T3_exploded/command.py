
from pylmgc90 import chipy

chipy.checkDirectories()

chipy.utilities_DisableLogMes()

####
# info gestion du temps
dt = 1.e-5
theta = 0.505
nb_steps = 100

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 10

# info contact

#         123456789012345678901234567890
stype  = 'Stored_Delassus_Loops         '
norm   = 'Quad '
tol    = 0.1e-3
relax  = 1.0
gs_it1 = 100
gs_it2 = 10

### definition des parametres du calcul ### 
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

###
chipy.SetDimension(2,1)

### lecture du modele ###
chipy.ReadDatbox()

### post2D ##
chipy.OpenDisplayFiles()

### postpro ###
#chipy.OpenPostproFiles()

chipy.CLxxx_SetNbNodesByCLxxx(2)
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

chipy.utilities_logMes('COMPUTE STIFFNESS')
chipy.ComputeBulk()

####
chipy.mecaMAILx_SetPreconAllBodies()
chipy.CLxxx_PushPreconNodes()
chipy.ALpxx_PushPreconNodes()
chipy.mecaMAILx_ComputePreconW()
####
chipy.AssembleMechanicalLHS()
#
for k in range(1, nb_steps + 1, 1):
   #
   chipy.utilities_logMes('increment : '+str(k))
   #
   chipy.utilities_logMes('INCREMENT STEP')
   chipy.IncrementStep()
   
   chipy.utilities_logMes('COMPUTE Fext')
   chipy.ComputeFext()

   chipy.utilities_logMes('COMPUTE Fint')
   chipy.ComputeBulk()

   chipy.utilities_logMes('ASSEMBLAGE')
   chipy.AssembleMechanicalRHS()

   chipy.utilities_logMes('COMPUTE Free Vlocy')
   chipy.ComputeFreeVelocity()
   #
   chipy.utilities_logMes('SELECT PROX TACTORS')
   chipy.SelectProxTactors()
   chipy.RecupRloc()
   chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
   chipy.StockRloc()
   #
   chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
   chipy.ComputeDof()
   #
   chipy.utilities_logMes('UPDATE DOF')
   chipy.UpdateStep()
   #
   ### postpro ###
   chipy.WriteLastDof()
   #
   chipy.utilities_logMes('WRITE LAST Vloc Rloc')
   chipy.WriteLastVlocRloc()
   
   chipy.WriteDisplayFiles(freq=freq_display)
   #chipy.WritePostproFiles()

### postpro ###
#chipy.ClosePostproFiles()
chipy.CloseDisplayFiles()

chipy.Finalize()
