
from pylmgc90 import chipy

chipy.checkDirectories()

#chipy.utilities_DisableLogMes()

####
# info gestion du temps
dt = 0.5
theta = 0.5
nb_steps = 100

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 10

# on indique qu'on travaille en deformation planes
chipy.SetDimension(2,1)

### definition des parametres du calcul ### 

# choix du pas de temps
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)

# initialisation de l'integrateur en temps :
# en thermique, on utilise Cranck-Nicholson et pas la theta-methode
# utilisee en meca
chipy.Integrator_InitCrankNickolson(theta)

### lecture du modele ###
chipy.ReadDatbox()

### post2D ##
chipy.OpenDisplayFiles()
#chipy.OpenPostproFiles()

# boucle en temps
for k in range(1, nb_steps + 1, 1):
   #
   chipy.utilities_logMes('increment : '+str(k))
   #
   chipy.utilities_logMes('INCREMENT STEP')
   chipy.IncrementStep()
   
   # calcul des flux nodaux imposes
   chipy.utilities_logMes('COMPUTE EXTERNAL FLUX')
   chipy.therMAILx_ComputeExternalFlux()
   
   # calcul de la capacite (ie masse)
   chipy.utilities_logMes('COMPUTE CAPACITY')
   chipy.therMAILx_ComputeCapacity()
   
   # calcul de la conductivite
   chipy.utilities_logMes('COMPUTE CONDUCTIVITY')
   chipy.therMAILx_ComputeConductivity()
   
   # calcul du second membre (=0 ici)
   chipy.utilities_logMes('COMPUTE INTERNAL FLUX')
   chipy.therMAILx_ComputeInternalFlux()
   
   # assemblage du second membre
   chipy.utilities_logMes('ASSEMB THERM RHS')
   chipy.therMAILx_AssembThermRHS()
   
   # assemblage de la matrice des iterations
   chipy.utilities_logMes('ASSEMB THERM KT')
   chipy.therMAILx_AssembThermKT()

   # resolution, calcul  de la temperature
   chipy.utilities_logMes('COMPUTE THERM DOF')
   chipy.therMAILx_ComputeThermDof()
   chipy.therMAILx_ComputeThermFields()
   
   # affichage
   #chipy.utilities_logMes('DISPLAY OUT DOF')
   #chipy.therMAILx_DisplayOutDof()

   # mise a jour des valeurs aux noeuds
   chipy.utilities_logMes('UPDATE THERM DOF')
   chipy.therMAILx_UpdateThermDof()
   
   # calcul des flux, grad et valeurs aux pg
   chipy.utilities_logMes('UPDATE THERM BULK')
   chipy.therMAILx_UpdateThermBulk()

   # ecriture des temperatures aux noeuds, calculees precedemment
   chipy.utilities_logMes('WRITE LAST DOF')
   chipy.WriteLastDof()
   
   # ecriture des valeurs aux points de gauss calculees precedemment
   chipy.utilities_logMes('WRITE LAST GPV')
   chipy.WriteLastGPV()
   
   ### post2D ###
   
   chipy.WriteDisplayFiles(freq_display)
   #chipy.WritePostproFiles()

#chipy.ClosePostproFiles()
chipy.CloseDisplayFiles()

chipy.Finalize()
