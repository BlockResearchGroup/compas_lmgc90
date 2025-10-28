import sys

import numpy as np

from pylmgc90 import chipy

# ------------------------------------------------------
# Make directories
chipy.checkDirectories()

# ------------------------------------------------------
# Defined time discretisation and integrator
dt     = 0.01
Tmax   = 2.0
dt_min = 0.01
dt_max = 0.01
freq_display=1
freq_write  =1
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
#
chipy.OpenDisplayFiles(thergp_field=['gradT', 'fluxT'])
#Create conteneur numpy de resultats
nb_node = chipy.therMAILx_GetNbNodes(1)
# ------------------------------------------------------
# Apply a section for all Segment
Se = np.ones(nb_node)*100.0
chipy.therMAILx_SetScalarFieldByNode(1,chipy.therMAILx_GetScalarFieldRank(1,1,'SECTION'),Se)
# ------------------------------------------------------
# Solve the physical model for all time
Sol = np.zeros((202,nb_node),float)
X_ = chipy.therMAILx_GetBodyVector('Coor0',1)[:,0]
# ------------------------------------------------------
# Solve the physical model for all time
t  = chipy.TimeEvolution_GetTime()
n = 0
while t < Tmax :
   # ------------------------------------------------------
   # Actualize the time
   chipy.utilities_logMes("  @    Actualisation du pas de temps")
   chipy.IncrementStep()
   # ------------------------------------------------------
   # Compute external load
   chipy.utilities_logMes("  @    Calcul des flux exterieurs")
   chipy.therMAILx_ComputeExternalFlux()
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
      chipy.utilities_logMes("  @    Calcul matrice de capacite")
      chipy.therMAILx_ComputeCapacity()
      # Compute conductivity
      chipy.utilities_logMes("  @    Calcul matrice de conduction")
      chipy.therMAILx_ComputeConductivity([])
      # Assemble internal load
      chipy.utilities_logMes("  @    Assemblage du systeme lineaire")
      chipy.therMAILx_AssembThermRHS()
      # Assemble tangent matrix
      chipy.therMAILx_AssembThermKT()
      # Solve the linear system
      chipy.utilities_logMes("  @    Resolution du systeme lineaire")
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
   Sol[n,:] = T_[:,0]
   # ------------------------------------------------------
   # Update Time
   chipy.utilities_logMes("  @    Update pas de temps")
   chipy.UpdateStep()

   # ------------------------------------------------------
   # Compute the time value and compute the next time step
   t = chipy.TimeEvolution_GetTime()
   # sortie ascii
   #chipy.WriteOut(freq_write)
   chipy.TimeEvolution_WriteOutDof(freq_write)
   chipy.therMAILx_WriteOutDof()
   chipy.TimeEvolution_WriteOutGPV(freq_write)
   chipy.MAILx_WriteOutGPV()

   # sortie vtk
   chipy.utilities_logMes("  @    Creation des fichiers de visualisation aux noeuds")
   chipy.WriteDisplayFiles()
   n += 1
chipy.CloseDisplayFiles()
chipy.Finalize()
# ------------------------------------------------------
# Solution analytique du probleme de thermique lineaire 1D
#import numpy as np
#from matplotlib.pyplot import *
#import pylab as pl
try:
  from matplotlib import pyplot as plt
except:
  print('matplotlib not imported, skip visu')
  sys.exit()

# ------------------------------------------------------
# Declaration des Parametres
rho   = 1.0
Cp    = 0.000005
D     = 0.002
l     = 100.0
T_imp = 100.0
dx    = 5.0
t_max = Tmax
k_max = 5000
taux  = (D*(np.pi**2) )/(rho*Cp*(l**2))
# ------------------------------------------------------
# Evaluation de la solution theorique en serie de Fourier en k

K = np.arange(1,k_max+1,1)
x = np.arange(0.0,l+dx,dx)
t = np.arange(0.0,t_max,dt)
Bk = np.zeros((len(K)),float)

X,T = np.meshgrid(x,t)
Sol_theo = np.zeros(X.shape,float)
i = 0
for k in K:
	Bk[i] = -((2*T_imp)/(k*np.pi))*(-1)**(k+1) #(2*Timp)
	Sol_theo += Bk[i]*np.sin((k*np.pi/l)*X)*np.exp(-(T*(k**2))*(taux))
	i += 1
	
Sol_theo += (X/l)*T_imp
# ------------------------------------------------------

if '--novisu' not in sys.argv:

  plt.figure()
  ax1 = plt.subplot(2,2,1)
  plt.plot(X_,Sol[10,:],'or')
  plt.plot(x,Sol_theo[10,:],'-+b')
  plt.xlabel('Longeur (m)')
  plt.ylabel('Temperature (K)')
  plt.title('Solution numerique et theorique a dt')
  plt.legend(('LMGC90','Analytique'))
  plt.grid('on')
  ax2 = plt.subplot(2,2,2)
  plt.plot(X_,Sol[50,:],'or')
  plt.plot(x,Sol_theo[50,:],'-+b')
  plt.xlabel('Longeur (m)')
  plt.ylabel('Temperature (K)')
  plt.title('Solution numerique et theorique a t_final/3')
  plt.legend(('LMGC90','Analytique'))
  plt.grid('on')
  ax3 = plt.subplot(2,2,3)
  plt.plot(X_,Sol[100,:],'or')
  plt.plot(x,Sol_theo[100,:],'-+b')
  plt.xlabel('Longeur (m)')
  plt.ylabel('Temperature (K)')
  plt.title('Solution numerique et theorique a 2xt_final/3')
  plt.legend(('LMGC90','Analytique'))
  plt.grid('on')
  ax4 = plt.subplot(2,2,4)
  plt.plot(X_,Sol[150,:],'or')
  plt.plot(x,Sol_theo[150,:],'-+b')
  plt.xlabel('Longeur (m)')
  plt.ylabel('Temperature (K)')
  plt.title('Solution numerique et theorique a t_final')
  plt.legend(('LMGC90','Analytique'))
  plt.grid('on')
  plt.show()
