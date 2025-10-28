import sys

import numpy as np

from pylmgc90 import chipy

# ------------------------------------------------------
# Make directories
chipy.checkDirectories()
# ------------------------------------------------------
# Defined time discretisation and integrator
dt     = 0.01
Tmax   = 0.2
dt_min = 0.01
dt_max = 0.01
freq_write  =1
freq_display=1
chipy.TimeEvolution_SetTimeStep(dt)
# Initialize theta integrator for all physical model
Theta = 0.5
chipy.Integrator_InitTheta(Theta)
# Initialize theta integrator for thermal model
chipy.Integrator_InitCrankNickolson(Theta)
# ------------------------------------------------------
# Import the model in LMGC90 database
# Fixed the problem dimension 3D
chipy.SetDimension(3,0)
# Load model definition
chipy.ReadDatbox()            # in /DATBOX
#Create conteneur numpy de resultats
nb_node = chipy.therMAILx_GetNbNodes(1)
# ------------------------------------------------------
# Apply a section for all Segment
Se = np.ones(nb_node)*10.0
chipy.therMAILx_SetScalarFieldByNode(1,chipy.therMAILx_GetScalarFieldRank(1,1,'SECTION'),Se)
# ------------------------------------------------------
# Apply a local source at node
nodes = chipy.therMAILx_GetCoor(1)
Source = (np.sqrt((nodes[:,0]-5.0)**2 + (nodes[:,1]-5.0)**2 + (nodes[:,2]-5.0)**2 ) <= 1.0)*1.0
chipy.therMAILx_SetScalarFieldByNode(1,chipy.therMAILx_GetScalarFieldRank(1,1,'SOURCE'),Source)
# ------------------------------------------------------
chipy.OpenDisplayFiles(thergp_field=['gradT', 'fluxT'])
# ------------------------------------------------------
# Solve the physical model for all time
t  = chipy.TimeEvolution_GetTime()
n = 0
while t < Tmax :
   # ------------------------------------------------------
   # Actualize the time
   chipy.IncrementStep()
   # ------------------------------------------------------
   # Compute external load
   chipy.utilities_logMes("  @    Calcul des flux exterieurs")
   chipy.therMAILx_ComputeExternalFlux()
   # Add a local source
   chipy.therMAILx_AddSource(1,chipy.therMAILx_GetScalarFieldRank(1,1,'SOURCE'))
   # ------------------------------------------------------
   # Compute internal load with NewtonRaphson procedure
   chipy.utilities_logMes("  @    Resolution du probleme : ")
   converged = 1
   
   while converged==1:
      
      # ------------------------------------------------------
      # Update the temperature at the Gauss Point
      #chipy.therMAILx_UpdateThermBulk()
      # ------------------------------------------------------
      # Compute capacity
      Cp = np.ones(nb_node)*0.0001
      chipy.therMAILx_SetScalarFieldByNode(1,chipy.therMAILx_GetScalarFieldRank(1,1,'SPHV'),Cp)
      chipy.therMAILx_ComputeCapacity()
      # Compute conductivity
      # Apply a non linear specific capacity on bodie number 1
      Cd = np.ones(nb_node)*1.0
      chipy.therMAILx_SetScalarFieldByNode(1,chipy.therMAILx_GetScalarFieldRank(1,1,'COCO'),Cd)
      chipy.therMAILx_ComputeConductivity([])
      # Assemble internal load
      chipy.therMAILx_AssembThermRHS()
      # Assemble tangent matrix
      chipy.therMAILx_AssembThermKT()
      # Solve the linear system
      chipy.therMAILx_ComputeThermDof()
      chipy.therMAILx_ComputeThermFields()
      # ------------------------------------------------------
      # Checking the convergence of the solution
      converged = 0
   # ------------------------------------------------------
   # Get the results for Temperature
   # Get the nodal temperature for non linear physical problem on bodie number 1
   # Tbeg_ : Temperature at the beginning time step
   # T____ : Temperature at this time step
   T_=chipy.therMAILx_GetBodyVector('T____',1)
   # ------------------------------------------------------
   # Update Time
   chipy.UpdateStep()
   # ------------------------------------------------------
   # Compute the time value and compute the next time step
   t = chipy.TimeEvolution_GetTime()
   # sortie ascii
   chipy.WriteOut(freq_write)
   # sortie gmv tous les 10 pas
   #~ chipy.overall_WriteOutDisplayFile(freq_display)
   #~ chipy.display_3D_WriteOutDisplayFile(freq_display)
   chipy.WriteDisplayFiles(freq_display)
   n += 1
chipy.CloseDisplayFiles()
chipy.Finalize()

