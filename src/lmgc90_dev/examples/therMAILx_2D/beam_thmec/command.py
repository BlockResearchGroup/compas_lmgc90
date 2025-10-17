import os,sys
from pylmgc90 import chipy

chipy.checkDirectories()

####
# info gestion du temps
dt = 0.001
theta = 0.5
nb_steps = 100

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 5
freq_write = 10

# on indique qu'on travaille en deformation planes
chipy.SetDimension(2,mod=1)

### definition des parametres du calcul ### 

# choix du pas de temps
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

# initialisation de l'integrateur en temps :
# en thermique, on utilise Cranck-Nicholson et pas la theta-methode
# utilisee en meca
chipy.Integrator_InitCrankNickolson(theta)

### lecture du modele ###

# lecture du modele
chipy.ReadDatbox()

### post2D ##
chipy.OpenDisplayFiles()

chipy.utilities_logMes('COMPUTE MASS')
chipy.mecaMAILx_ComputeMass()

chipy.utilities_logMes('COMPUTE STIFFNESS')
chipy.mecaMAILx_ComputeBulk()
chipy.mecaMAILx_AssembKT()

# boucle en temps
for k in range(1, nb_steps + 1, 1):
   #
   chipy.utilities_logMes('INCREMENT STEP')
   chipy.TimeEvolution_IncrementStep()
   chipy.therMAILx_IncrementStep()
   chipy.mecaMAILx_IncrementStep()
   
   chipy.utilities_logMes('DISPLAY TIMES')
   chipy.TimeEvolution_DisplayStep()

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

   T=chipy.therMAILx_GetBodyVector('T____',1)
   T.shape = [T.size] 
   chipy.mecaMAILx_SetScalarFieldByNode(1,1,T)   

   chipy.utilities_logMes('COMPUTE Fext')
   chipy.mecaMAILx_ComputeFext()
   
   chipy.utilities_logMes('COMPUTE Fint')
   chipy.mecaMAILx_ComputeBulk()
   
   chipy.utilities_logMes('ASSEMBLAGE')
   chipy.mecaMAILx_AssembRHS()

   chipy.utilities_logMes('COMPUTE Free Vlocy')
   chipy.mecaMAILx_ComputeFreeVelocity()
   #
   chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
   chipy.mecaMAILx_ComputeDof()
   chipy.mecaMAILx_ComputeField()
   #
   chipy.utilities_logMes('UPDATE DOF, FIELDS')
   chipy.TimeEvolution_UpdateStep()
   chipy.mecaMAILx_UpdateDof()
   chipy.mecaMAILx_UpdateBulk()
   #
   chipy.utilities_logMes('WRITE out DOF')
   chipy.TimeEvolution_WriteOutDof(freq_write)
   chipy.therMAILx_WriteOutDof()
   chipy.mecaMAILx_WriteOutDof()
   
   # ecriture des valeurs aux points de gauss calculees precedemment
   chipy.utilities_logMes('WRITE OUT GPV')
   chipy.TimeEvolution_WriteOutGPV(freq_write)
   chipy.MAILx_WriteOutGPV()

   chipy.WriteDisplayFiles(freq=freq_display)
   
   ### gestion des writeout ###
   chipy.overall_CleanWriteOutFlags()

chipy.CloseDisplayFiles()
chipy.Finalize()
