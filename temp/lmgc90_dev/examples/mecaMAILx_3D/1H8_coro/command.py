
from pylmgc90 import chipy

chipy.checkDirectories()

#chipy.utilities_EnableLogMes()

####
# info gestion du temps
dt = 1.e-4
theta = 0.50
nb_steps = 10 #1000

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 1

# info contact
freq_detect = 1

chipy.Initialize()

#        123456789012345678901234567890
stype = 'Stored_Delassus_Loops         '
quad   = 'QM/16'
tol    = 1e-6
relax  = 0.1
gs_it1 = 1000
gs_it2 = 2


###
chipy.SetDimension(3)
#

### definition des parametres du calcul ### 
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

chipy.ReadDatbox(deformable=True)
#
#-> activation du corotationnel
# + calcul des caracteristiques du rigide equivalent
# + calcul de l'opertateur R2D
# rq: important que ca soit fait avant read_ini, comp_mass, etc
chipy.mecaMAILx_SetCoroAllBodies()
#chipy.mecaMAILx_SetRigidAllBodies()


### postpro ###
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

### parameters setting ###

chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

# assemblage de l'operateur D2R a partir des masses elementaires
chipy.mecaMAILx_BuildRigidBodies()

chipy.utilities_logMes('COMPUTE STIFFNESS')
chipy.mecaMAILx_ComputeBulk()

# amortissement de Rayleigh
chipy.mecaMAILx_ComputeRayleighDamping(0.07, 0.07)

chipy.AssembleMechanicalLHS()

for k in range(nb_steps):
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
   chipy.SelectProxTactors(freq_detect)
   #
   chipy.utilities_logMes('INITIALISATION Rloc')
   chipy.RecupRloc()
   #
   chipy.utilities_logMes('RESOLUTION CONTACT')
   chipy.ExSolver(stype, quad, tol, relax, gs_it1, gs_it2)
   # 
   chipy.utilities_logMes('SAUVEGARDE Rloc')
   chipy.StockRloc()
   #
   chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
   chipy.ComputeDof()
   #
   chipy.utilities_logMes('UPDATE DOF, FIELDS')
   chipy.UpdateStep()
   #
   chipy.utilities_logMes('WRITE out DOF')
   chipy.WriteOutDof()

   ### post3D ###
   chipy.WriteDisplayFiles(freq_display)
   chipy.WritePostproFiles(freq_display)

### postpro ###
chipy.ClosePostproFiles()
chipy.CloseDisplayFiles()

chipy.Finalize()

print('Test passed')
