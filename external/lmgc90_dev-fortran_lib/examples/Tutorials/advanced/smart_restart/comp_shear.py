import os, fnmatch
import numpy as np

import math

from pylmgc90 import chipy

chipy.Initialize()
chipy.overall_SetWorkingDirectory('Shear')

#############
# Parameters
#############
#
# ... of the simulation
nb_steps    = 10000
dt          = 5.e-4  # Time step
theta       = 0.5    # Time integrator
freq_detect = 1      # Contact detection frequency
Rmax        = 0.15
freq_visu   = 100    # Number of visualization files
freq_write  = 100    # Number of outputs in file

h5_file           = 'lmgc90.h5'

# selecting output to read
if os.path.isfile( os.path.join('Press',h5_file) ):
  import h5py
  with h5py.File('Press/lmgc90.h5', 'r') as hf:
    reading_step= int( hf['Simulation/nb_record'][()] )
else:
  reading_step      = len(fnmatch.filter(os.listdir('../Press/OUTBOX/'), 'DOF.OUT.*'))
    
#
# ... of the computation
#         123456789012345678901234567890
stype  = 'Stored_Delassus_Loops         '
norm   = 'QM/16'
tol    = 1e-5
relax  = 1.0
gs_it1 = 100                   # Minimum number of iterations
gs_it2 = 101                   # Maximum number of iterations = gs_it2 * gs_it1
chipy.nlgs_SetWithQuickScramble()
#
#############
#chipy.utilities_DisableLogMes()      # Log message management
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

chipy.utilities_logMes('LOAD TACTORS')
chipy.LoadTactors()

# THERMO RIGID
chipy.ReadMpBehaviours(0,'therm')

if os.path.isfile( os.path.join('Press',h5_file) ):
  print(reading_step,os.path.join('../Press',h5_file))
  chipy.ReadIni(reading_step,os.path.join('../Press',h5_file))
else:
  chipy.ReadIni(reading_step)
#
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
print("Nb_steps = ",nb_steps)
print("Freq sauvegarde donnees = ", freq_write)
print("Freq sauvegarde visu    = ", freq_visu)

nbdiskx     = chipy.DISKx_GetNbDISKx()
diskx2rbdy2 = chipy.DISKx_GetPtrDISKx2BDYTY()

nbjoncx     = chipy.JONCx_GetNbJONCx()
joncx2rbdy2 = chipy.JONCx_GetPtrJONCx2BDYTY()

nbR2        = chipy.RBDY2_GetNbRBDY2()

Th_DISKx = np.zeros([nbdiskx])
Th_JONCx = np.zeros([nbjoncx])

T_dict = {'DISKx':Th_DISKx, 'JONCx':Th_JONCx}


# parenthesis do matter !!!
chipy.RBDY2_FatalDamping()

for k in range(1, nb_steps, 1):
   #
   chipy.IncrementStep()
   #
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
   # THERMO RIGID
   #
   chipy.mp_solver_RecupTemperature()
   chipy.mp_solver_SolveThermoProblem()

   #
   chipy.ComputeDof()
   chipy.UpdateStep()
   #
   chipy.WriteOut(freq_write)
   #
   # THERMO RIGID
   #
   chipy.WriteOutMpValues(freq_write)
   #
   ### postpro ###
   if (k % freq_visu == 0 ):
      #
      for itacty in range(1,nbdiskx+1,1):
         iddiskx = diskx2rbdy2[itacty-1]
         idR2 = int(iddiskx[0])
         idTy = int(iddiskx[1])
         # THERMO RIGID
         Th_DISKx[itacty-1] = chipy.RBDY2_GetThermalValue(idR2,idTy)
         
      for itacty in range(1,nbjoncx+1,1):
         idjoncx = joncx2rbdy2[itacty-1]
         idR2 = int(idjoncx[0])
         idTy = int(idjoncx[1])
         # THERMO RIGID
         Th_JONCx[itacty-1] = chipy.RBDY2_GetThermalValue(idR2,idTy)
   
      chipy.WriteDisplayFiles(1,T =('tacts',T_dict))
     
   chipy.WritePostproFiles()

   chipy.checkInteractiveCommand()

##########################
### END OF COMPUTATION ###
##########################

chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()
chipy.Finalize()

