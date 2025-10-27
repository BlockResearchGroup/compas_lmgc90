import pickle

import numpy as np

# importing chipy module
from pylmgc90 import chipy
from pylmgc90 import post

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
dt = 1e-4
nb_steps = 1000

# theta integrator parameter
theta = 0.5

# interaction parameters
Rloc_tol = 5.e-2
chipy.PRPRx_ShrinkPolyrFaces(5e-2)
chipy.PRPRx_UseCpF2fExplicitDetection(1e-3)
chipy.PRPRx_LowSizeArrayPolyr(10)

# nlgs parameters
chipy.nlgs_3D_DiagonalResolution()
tol = 1.666e-4
relax = 0.2
# you can consider using relax = 1./4. to have a symmetrical solution 
norm = 'QM/16'
gs_it1 = 200
gs_it2 = 10
solver_type='Stored_Delassus_Loops         '

# write parameter
freq_write   = 50

# display parameters
freq_display = 50

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
chipy.OpenDisplayFiles(write_f2f=3)
chipy.OpenPostproFiles()
post.OpenCentralKernelFiles()

#
# simulation part ...
#

# ... calls a simulation time loop
# since constant compute elementary mass once
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

for k in range(0,nb_steps):
  #
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()

  chipy.utilities_logMes('COMPUTE Fext')
  chipy.ComputeFext()
  chipy.utilities_logMes('COMPUTE Fint')
  chipy.ComputeBulk()
  chipy.utilities_logMes('COMPUTE Free Vlocy')
  chipy.ComputeFreeVelocity()

  chipy.utilities_logMes('SELECT PROX TACTORS')
  chipy.SelectProxTactors()

  chipy.utilities_logMes('RESOLUTION' )
  chipy.RecupRloc(Rloc_tol)

  chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
  # chipy.UpdateTactBehav()

  chipy.StockRloc()

  chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
  chipy.ComputeDof()

  chipy.utilities_logMes('UPDATE DOF, FIELDS')
  chipy.UpdateStep()

  chipy.utilities_logMes('WRITE OUT')
  chipy.WriteOut(freq_write)

  chipy.utilities_logMes('VISU & POSTPRO')
  chipy.WriteDisplayFiles(freq_display)
  chipy.WritePostproFiles()

#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# save last inters and f2f to pkl
with open('f2f_inters.pkl', 'wb') as f:
  f2f    = chipy.PRPRx_GetF2f2Inters()
  inters = chipy.getInteractions()
  pickle.dump( (f2f, inters,), f )

# this is the end
chipy.Finalize()
