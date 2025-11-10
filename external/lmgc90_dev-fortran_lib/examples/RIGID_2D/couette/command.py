import math
import numpy as np

# importing chipy module
from pylmgc90 import chipy

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()

# logMes
chipy.utilities_DisableLogMes()


# driving radius expansion
# the given pressure
P=1e5
# max radial velocity amplitude
Vdmax=1e-1
# pseudo mass
M=1e4
# needed by algorithm
Vd=0.
drdila = 0.
Pres=0.

#
# defining some variables
#

# space dimension
dim = 2

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1

# time evolution parameters
dt = 1e-2
nb_steps = 3000

# theta integrator parameter
theta = 0.5

# interaction parameters
Rloc_tol = 5.e-2

# nlgs parameters
tol = 1e-6
relax = 1.0
norm = 'Quad '
gs_it1 = 150
gs_it2 = 30
solver_type='Stored_Delassus_Loops         '

# write parameter
freq_write   = 10

# display parameters
freq_display = 10

chipy.nlgs_SetWithQuickScramble()

#
# read and load
#

# Set space dimension
chipy.SetDimension(dim,mhyp)
#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
#
chipy.ReadDatbox(deformable=False)

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

#
# simulation part ...
#

nb_DISKx = chipy.DISKx_GetNbDISKx()

# ... calls a simulation time loop
# since constant compute elementary mass once
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

# to store results
evol_pres=np.zeros((4,nb_steps))

for k in range(0,nb_steps):
  #
  chipy.utilities_logMes('INCREMENT STEP')

  chipy.utilities_EnableLogMes()
  chipy.IncrementStep()
  chipy.utilities_DisableLogMes()
  chipy.utilities_logMes('COMPUTE Fext')
  chipy.ComputeFext()
  chipy.utilities_logMes('COMPUTE Fint')
  chipy.ComputeBulk()
  chipy.utilities_logMes('COMPUTE Free Vlocy')
  chipy.ComputeFreeVelocity()

  Vd=dt*(Pres-P)/M
  # on seuil la vitesse
  if Vd < 0.:
    Vd = max(-Vdmax,Vd)
  elif Vd > 0.:  
    Vd = min(Vdmax,Vd)
    
  chipy.xKSID_SetVdilation(1,Vd)

  chipy.utilities_logMes('SELECT PROX TACTORS')
  chipy.SelectProxTactors()

  chipy.utilities_logMes('RESOLUTION' )
  chipy.RecupRloc(Rloc_tol)

  chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
  chipy.UpdateTactBehav()

  chipy.StockRloc()

  chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
  chipy.ComputeDof()

  chipy.utilities_logMes('UPDATE DOF, FIELDS')
  chipy.UpdateStep()

  # on reduit le rayon de la particule
  drdila += dt*Vd
  chipy.xKSID_SetXdilation(1,drdila)

  chipy.utilities_logMes('WRITE OUT')
  chipy.WriteOut(freq_write)

  chipy.utilities_logMes('VISU & POSTPRO')
  chipy.WriteDisplayFiles(freq_display)
  chipy.WritePostproFiles()

  # computing pressure on hollow cylinder
  Pres=0.
  R=chipy.xKSID_GetContactorRadius(1)
  if chipy.inter_handler_2D_getNb(chipy.DKKDx_ID):
    inters = chipy.inter_handler_2D_getAll( chipy.DKKDx_ID )
    for inter in inters:
      Pres+=inter[8]  
    Pres/=2*math.pi*R

  evol_pres[0,k]=chipy.TimeEvolution_GetTime()
  evol_pres[1,k]=Vd
  evol_pres[2,k]=R
  evol_pres[3,k]=Pres    
  
#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()

import matplotlib.pyplot as plt
plt.subplot(311)
plt.plot(evol_pres[0],evol_pres[3])
plt.xlabel('time')
plt.ylabel('pressure')
plt.subplot(312)
plt.plot(evol_pres[0],evol_pres[2])
plt.xlabel('time')
plt.ylabel('radius')
plt.subplot(313)
plt.plot(evol_pres[0],evol_pres[1])
plt.xlabel('time')
plt.ylabel('velocity')
plt.show()

import pickle
with open("pres.p", "wb" ) as f:
    pickle.dump(evol_pres, f )

