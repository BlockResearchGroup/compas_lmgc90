
from pylmgc90 import chipy
####

chipy.checkDirectories()

# desactivation des messages de log
chipy.utilities_DisableLogMes()

### parameters setting ###
chipy.SetDimension(2,1)
dt = 1.e-4
theta = 0.9
nb_steps = 2500

#   * detection frequency

# write parameter
freq_write   = 10

#   * visualization frequency
freq_display = 10
#         123456789012345678901234567890
#   * nlgs solver parameters
stype  = 'Stored_Delassus_Loops         '
norm   = 'Quad '
tol    = 1.e-3
relax  = 1.0
gs_it1 = 100
gs_it2 = 10

#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
#

### lecture du modele ###
chipy.utilities_logMes('READ DATBOX')
chipy.ReadDatbox()
chipy.utilities_logMes('SET NB CLxxx BY EDGE')
chipy.CLxxx_SetNbNodesByCLxxx(2)

### post2D ##
chipy.OpenDisplayFiles()

### postpro ###
chipy.OpenPostproFiles()

chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

# since constant compute elementary stiffness matrices once
chipy.utilities_logMes('COMPUTE STIFFNESS')
chipy.ComputeBulk()

# precondensation
chipy.mecaMAILx_SetPreconAllBodies()
chipy.CLxxx_PushPreconNodes()
chipy.ALpxx_PushPreconNodes()
chipy.mecaMAILx_ComputePreconW()

# since constant compute iteration matrix once
chipy.utilities_logMes('ASSEMB KT')
chipy.AssembleMechanicalLHS()

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
  chipy.utilities_logMes('SELECT PROX TACTORS')
  chipy.SelectProxTactors()
  #
  chipy.utilities_logMes('RESOLUTION' )
  chipy.RecupRloc()
  #
  chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
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

#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()
