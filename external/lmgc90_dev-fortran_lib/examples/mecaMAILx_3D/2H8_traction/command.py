import numpy as np

# importing chipy module
from pylmgc90 import chipy

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()

# logMes
chipy.utilities_DisableLogMes()

#
# defining some variables
#

# space dimension
dim = 3

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 0

# time evolution parameters
dt = 1.e-6
nb_steps = int(0.1/dt)

# theta integrator parameter
theta = 0.5

# interaction parameters
Rloc_tol = 5.e-2

chipy.CSASp_SkipAutoContact()

# nlgs parameters
tol = 1e-4
relax = 1.0
norm = 'Quad '
gs_it1 = 50
gs_it2 = 5
solver_type='Stored_Delassus_Loops         '

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
chipy.ReadDatbox()

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

####
# Registering access to some interaction internals
chipy.registerInterInternals(['beta', '#taille_ele', 'saut_de_un'])
# sizing results... nb_tact_laws is the number of interactions !
nb_inters = chipy.tact_behav_GetNbTactBehav()
data2draw = np.zeros( [nb_inters, 4, nb_steps], dtype=float)

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

chipy.mecaMAILx_SetPreconAllBodies()
chipy.CSxxx_PushPreconNodes()
chipy.ASpxx_PushPreconNodes()
chipy.mecaMAILx_ComputePreconW()

# since constant compute iteration matrix once
chipy.utilities_logMes('ASSEMB KT')
chipy.AssembleMechanicalLHS()

for k in range(1, nb_steps + 1, 1):
  if k%(nb_steps//100) == 0 :
    print( str(k//(nb_steps//100))+"% done" )
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
  chipy.utilities_logMes('SELECT PROX TACTORS')
  chipy.SelectProxTactors()
  #
  chipy.utilities_logMes('RESOLUTION' )
  chipy.RecupRloc()
  #
  chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
  chipy.UpdateTactBehav()
  #
  chipy.StockRloc()
  #
  chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
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

  nstep=chipy.TimeEvolution_GetStep()
    
  inters = chipy.getInteractions()

  data2draw[:,0,nstep-1] = chipy.getInternalArray('saut_de_un', inters)
  data2draw[:,1,nstep-1] = -inters['rl'][:,1]
  data2draw[:,2,nstep-1] = chipy.getInternalArray('beta', inters)
  data2draw[:,3,nstep-1] = chipy.getInternalArray('#taille_ele', inters)

#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# tricky last inters is still available... using it to redo
# the dictionnary with the type of contact law as keys
law2draw = { }
for ilaw in range( 1, chipy.tact_behav_GetNbTactBehav()+1):
    law_type, law_name, law_params = chipy.tact_behav_GetTactBehav(ilaw)
    # here are all laws ; normally only one for each type
    idx = inters['behav']==law_name.encode()
    law2draw[law_type] = [ ilaw, law_params, data2draw[idx,:,:] ]

# this is the end
chipy.Finalize()

# Save contact law type and associated values
import pickle
with open("RnGapBeta.p", "wb" ) as f:
    pickle.dump(law2draw, f )

