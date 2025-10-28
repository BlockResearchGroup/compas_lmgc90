# importing chipy module
from pylmgc90 import chipy

# Initializing
chipy.Initialize()

chipy.tact_behav_SetCZMwithInitialFriction(2.)
# checking/creating mandatory subfolders
chipy.checkDirectories()


# logMes
# utilities_DisableLogMes()

#
# defining some variables
#

# space dimension
dim = 2

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1

# time evolution parameters
dt = 1e-4
nb_steps = 50000 #97000

# theta integrator parameter
theta = 0.5

# interaction parameters
freq_detect = 1
Rloc_tol = 5.e-2

# nlgs parameters
tol    = 1e-4
relax  = 1.0
norm   = 'Quad '
gs_it1 = 5
gs_it2 = 10
stype  ='Stored_Delassus_Loops         '



# write parameter
freq_write   = 1000

# display parameters
freq_display = 1000

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
# since constant compute elementary mass once
chipy.ComputeMass()

for k in range(0,nb_steps):
  #
  chipy.IncrementStep()

  chipy.ComputeFext()
  chipy.ComputeBulk()
  chipy.ComputeFreeVelocity()

  chipy.SelectProxTactors(freq_detect)

  chipy.RecupRloc(Rloc_tol)

  chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
  chipy.UpdateTactBehav()

  chipy.StockRloc()

  chipy.ComputeDof()

  chipy.UpdateStep()

  chipy.WriteOut(freq_write)

  #chipy.WriteDisplayFiles(freq_display)
  chipy.WritePostproFiles()

#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()
