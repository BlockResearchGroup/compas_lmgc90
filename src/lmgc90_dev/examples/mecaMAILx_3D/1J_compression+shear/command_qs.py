import numpy as np
# importing chipy module
from pylmgc90 import chipy

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()

# logMes
# chipy.utilities_DisableLogMes()

#
# defining some variables
#

# space dimension
dim = 3

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 0

# time evolution parameters
dt = 1.
nb_steps = 25

# write parameter
freq_write   = 1

# display parameters
freq_display = 1

#
# read and load
#

# Set space dimension
chipy.SetDimension(dim,mhyp)
#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitQS()
#
chipy.ReadDatbox()

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

#
# simulation part ...
#

# ... calls a simulation time loop
# since constant compute elementary mass matrices once
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

# since constant compute elementary stiffness matrices once
chipy.utilities_logMes('COMPUTE STIFFNESS')
chipy.ComputeBulk()

# since constant compute iteration matrix once
chipy.utilities_logMes('ASSEMB KT')
chipy.AssembleMechanicalLHS()

f=open('gpi.txt','w')

for k in range(1, nb_steps + 1, 1):
  #
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()
  #
  chipy.utilities_logMes('COMPUTE Fext')
  chipy.ComputeFext()
  #
  chipy.utilities_logMes('COMPUTE Fint')
  chipy.ComputeBulk()
  #
  chipy.utilities_logMes('ASSEMB RHS')
  chipy.AssembleMechanicalRHS()
  #
  chipy.utilities_logMes('COMPUTE Free Vlocy')
  chipy.ComputeFreeVelocity()
  #
  chipy.utilities_logMes('COMPUTE DOF, FIELDS')
  chipy.ComputeDof()

  #
  chipy.utilities_logMes('UPDATE DOF, FIELDS')
  chipy.UpdateStep()
  #
  chipy.utilities_logMes('WRITE OUT')
  chipy.WriteOut(freq_write)
  #
  chipy.utilities_logMes('VISU & POSTPRO')
  chipy.WriteDisplayFiles(freq_display)
  chipy.WritePostproFiles()

  # first body,element,gp
  gpi=chipy.mecaMAILx_GetGpInternals(1,1,1)
  e  =chipy.mecaMAILx_GetGpStrain(1,1,1)
  s  =chipy.mecaMAILx_GetGpStress(1,1,1)
  dep=chipy.mecaMAILx_GetBodyVector('X____',1)

  xx=np.append([chipy.TimeEvolution_GetTime()],dep[4,:]-dep[0,:],axis=0)
  xx=np.append(xx,e[:],axis=0)  
  xx=np.append(xx,s[:],axis=0)  
  xx=np.append(xx,gpi,axis=0)
  np.savetxt(f,xx[np.newaxis])    
  
#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

f.close()

# this is the end
chipy.Finalize()
