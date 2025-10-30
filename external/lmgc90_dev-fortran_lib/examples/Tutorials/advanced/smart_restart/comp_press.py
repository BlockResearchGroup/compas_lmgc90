import os

import math

from pylmgc90 import chipy

chipy.Initialize()
chipy.overall_SetWorkingDirectory('Press')

Rmax              = 0.15
dt                = 1.e-4  # Time step
theta             = 0.5    # Time integrator
nb_steps          = 5000
freq_display      = 1000
freq_outFiles     = 1000
freq_detect       = 1      # Contact detection frequency

h5_file           = 'lmgc90.h5'
#
#         123456789012345678901234567890
stype  = 'Stored_Delassus_Loops         '
norm   = 'QM/16'
tol    = 1e-4
relax  = 1.0
gs_it1 = 50                    # Minimum number of iterations
gs_it2 = 501                   # Maximum number of iterations  = gs_it2 * gs_it1
#
chipy.nlgs_SetWithQuickScramble()
#
#############
chipy.utilities_DisableLogMes()      # Log message management
chipy.checkDirectories()             # Check if all subdirectories are presents
chipy.SetDimension(2)                # Set dimension in chipy for dummies  ###

#####################
### Model reading ###
#####################
chipy.utilities_logMes('READ BODIES')
chipy.ReadBodies()

chipy.utilities_logMes('READ BEHAVIOURS')
chipy.ReadBehaviours()

chipy.utilities_logMes('LOAD BEHAVIOURS')
chipy.LoadBehaviours()

chipy.utilities_logMes('READ INI DOF')
chipy.ReadIniDof()

chipy.utilities_logMes('LOAD TACTORS')
chipy.LoadTactors()

chipy.utilities_logMes('READ INI Vloc Rloc')
chipy.ReadIniVlocRloc()

chipy.utilities_logMes('READ DRIVEN DOF')
chipy.ReadDrivenDof()
############################
### End of Model reading ###
############################

chipy.SetPeriodicCondition(xperiod=50*Rmax)


nb_rbdy2 = chipy.RBDY2_GetNbRBDY2() # Getting the number of grains

#########################################
### Computation parameters definition ###
#########################################
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

### Init postpro ###
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()
chipy.InitHDF5(h5_file)

### COMPUTE MASS ###
chipy.ComputeMass()

############################
### STARTING COMPUTATION ###
############################
print("Freq sauvegarde donnees      = ", freq_outFiles)
print("Freq sauvegarde visu         = ", freq_display)

for k in range(1, nb_steps, 1):
   #
   chipy.IncrementStep()
   chipy.ComputeFext()
   #
   chipy.ComputeBulk()
   chipy.ComputeFreeVelocity()
   #
   chipy.SelectProxTactors(freq_detect)
   #
   chipy.RecupRloc()
   chipy.nlgs_ExSolver(stype,norm,tol,relax,gs_it1,gs_it2)
   chipy.StockRloc()
   #
   chipy.ComputeDof()
   chipy.UpdateStep()
   #
   chipy.WriteOut(freq_outFiles)
   #
   
   ### postpro ###
   chipy.WriteDisplayFiles(freq_display)
   chipy.WritePostproFiles()

   chipy.checkInteractiveCommand()


##########################
### END OF COMPUTATION ###
##########################

chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()
chipy.Finalize()

