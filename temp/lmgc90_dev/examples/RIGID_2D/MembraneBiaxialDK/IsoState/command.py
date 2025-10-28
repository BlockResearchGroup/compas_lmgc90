from __future__ import print_function

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
dim = 2

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1

# time evolution parameters
dt = 2e-3
nb_steps = 5000

# theta integrator parameter
theta = 0.5

# interaction parameters
Rloc_tol = 5.e-2

# nlgs parameters
tol = 1e-5
relax = 1.0
norm = 'Quad '
gs_it1 = 300
gs_it2 = 20
solver_type='Stored_Delassus_Loops         '

# write parameter
freq_write   = 100

# display parameters
freq_display = 100

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

# ... calls a simulation time loop
# since constant compute elementary mass once
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

chipy.RBDY2_BiaxialLoading(4,0.,3,0.,1,0.,2,0.)

for k in range(0,nb_steps):
  #
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()

  chipy.utilities_logMes('COMPUTE Fext')
  chipy.ComputeFext()

  chipy.RBDY2_BiaxialLoading(4,10000.,3,10000.,1,10000.,2,10000.)

  chipy.utilities_logMes('COMPUTE Fint')
  chipy.ComputeBulk()
  chipy.utilities_logMes('COMPUTE Free Vlocy')
  chipy.ComputeFreeVelocity()

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

  chipy.utilities_logMes('WRITE OUT')
  chipy.WriteOut(freq_write)

  chipy.utilities_logMes('VISU & POSTPRO')
  chipy.WriteDisplayFiles(freq_display)
  chipy.WritePostproFiles()

  # are we in equilibrium
  if (chipy.RBDY2_CheckEquilibriumState()): 
    print('--Equilibrated sample--')
    break

#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()
