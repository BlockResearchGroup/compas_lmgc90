
import os,sys

from pylmgc90 import chipy

chipy.checkDirectories()
####

dt=0.5
freq_display=10
# driving time subdivision scheme
dt_max=0.5
dt_min=0.5

t_final = 20.

# driving Newton Raphson
NR_nb_iter_max = 20
NR_tol=1e-10

######################

# on indique qu'on travaille en deformation planes
chipy.SetDimension(2, 1)

### definition des parametres du calcul ### 

# choix du pas de temps
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)

# initialisation de l'integrateur en temps :
# en thermique, on utilise Cranck-Nicholson et pas la theta-methode
# utilisee en meca
chipy.Integrator_InitCrankNickolson(0.5)

# Newton loop parameters:
chipy.NewtonRaphson_SetFinalTime(t_final)
chipy.NewtonRaphson_SetMinTimeStep(dt_min)
chipy.NewtonRaphson_SetMaxTimeStep(dt_max)
chipy.NewtonRaphson_SetIncPatience(999)
chipy.NewtonRaphson_SetGoodIter(999)
chipy.NewtonRaphson_SetBadIter(999)
chipy.NewtonRaphson_SetMaxIter(NR_nb_iter_max)

### lecture du modele ###

# lecture des corps
chipy.ReadDatbox()

### post2D ##

chipy.OpenDisplayFiles()

# boucle en temps
while chipy.TimeEvolution_GetTime() < t_final :
   #
   chipy.utilities_logMes('INCREMENT STEP')
   chipy.IncrementStep()
   
   chipy.utilities_logMes('DISPLAY TIMES')
   chipy.TimeEvolution_DisplayStep()

   # calcul des flux nodaux imposes
   chipy.utilities_logMes('COMPUTE EXTERNAL FLUX')
   chipy.therMAILx_ComputeExternalFlux()

   # Newton loop
   k=0
   is_converged = 1
   chipy.NewtonRaphson_Initialize(NR_tol)
   #
   while is_converged == 1 : #looping until something changes in CheckNlConvergence

      k+=1

      # calcul de T au point de gauss a t+theta_t
      chipy.therMAILx_UpdateThermBulk()

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
      chipy.utilities_logMes('ASSEMB NL THERM RHS')
      chipy.therMAILx_AssembThermRHS()
   
      # assemblage de la matrice des iterations
      chipy.utilities_logMes('ASSEMB THERM KT')
      chipy.therMAILx_AssembThermKT()

      # resolution, calcul  de la temperature
      chipy.utilities_logMes('COMPUTE THERM DOF')
      chipy.therMAILx_ComputeThermDof()
      chipy.therMAILx_ComputeThermFields()

      # Test sur les residus
      if k > 1:
        norm = chipy.therMAILx_ComputeResidueNorm()
        ### 0=cv 1=continue 2=dv
        is_converged = chipy.NewtonRaphson_CheckConvergence(norm)

   ### end while

   #utilities_logMes('COMPUTE TIME STEP')
   ### istate = 1 redo, istate = 2 stop 
   istate = chipy.NewtonRaphson_ComputeTimeStep()

   if not istate == 1 :
   
     chipy.TimeEvolution_UpdateStep()

     # affichage
     chipy.utilities_logMes('DISPLAY OUT DOF')
     chipy.therMAILx_DisplayOutDof()

    # mise a jour des valeurs aux noeuds
     chipy.utilities_logMes('UPDATE THERM DOF')
     chipy.therMAILx_UpdateThermDof()
   
     # calcul des flux, grad et valeurs aux pg
     chipy.utilities_logMes('UPDATE THERM BULK')
     chipy.therMAILx_UpdateThermBulk()

     # ecriture des temperatures aux noeuds, calculees precedemment
     chipy.utilities_logMes('WRITE LAST DOF')
     chipy.TimeEvolution_WriteLastDof()
     chipy.therMAILx_WriteLastDof()
   
     # ecriture des valeurs aux points de gauss calculees precedemment
     chipy.utilities_logMes('WRITE LAST GPV')
     chipy.TimeEvolution_WriteLastGPV()
     chipy.MAILx_WriteLastGPV()
   
     ### post2D ###
    
     # sortie gmv tous les 10 pas
     chipy.WriteDisplayFiles(freq=1)

     if istate == 2 :
        break

chipy.CloseDisplayFiles()
chipy.Finalize()
