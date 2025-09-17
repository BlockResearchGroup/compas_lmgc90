import os

from pylmgc90 import chipy

chipy.Initialize()

chipy.utilities_DisableLogMes()

chipy.SetDimension(3,0)

#working directory for outputs
wd = os.path.join( os.getcwd(), 'standard' )

####
dt = 1.e-3
theta = 0.505
nb_steps = 10
freq_write=1

#
freq_display = 1

#
freq_detect = 1

#         123456789012345678901234567890
stype  = 'Stored_Delassus_Loops         '
quad   = 'QM/16'
tol    = 1e-4
relax  = 1.0
gs_it1 = 51
gs_it2 = 501

###
chipy.mecaMAILx_SparseStorage()

###
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

###
chipy.utilities_logMes('READ BODIES')
chipy.ReadBodies()

chipy.utilities_logMes('READ BEHAVIOURS')
chipy.ReadBehaviours()

chipy.utilities_logMes('READ MODELS')
chipy.ReadModels()

chipy.utilities_logMes('LOADS BEHAVIOUR AND MODELS')
chipy.LoadBehaviours()
chipy.LoadModels()

# should be made after changing directory because
# of ASpxx.vtu and CSpxx.vtu
chipy.utilities_logMes('LOADS CONTACTORS')
chipy.LoadTactors()

#
chipy.utilities_logMes('READ INI DOF')
chipy.ReadIniDof()

chipy.utilities_logMes('READ INI GPV')
chipy.ReadIniGPV()

chipy.utilities_logMes('READ INI Vloc Rloc')
chipy.ReadIniVlocRloc()

chipy.utilities_logMes('READ DRIVEN DOF')
chipy.ReadDrivenDof()

# changing working directory for output
if not os.path.isdir(wd):
  os.mkdir(wd)
chipy.overall_SetWorkingDirectory(wd)
chipy.checkDirectories(checkDATBOX=False)


###
chipy.utilities_logMes('WRITE BODIES')
chipy.WriteBodies()

chipy.utilities_logMes('WRITE MODELS')
chipy.models_WriteModels()

chipy.utilities_logMes('WRITE BEHAVIOURS')
chipy.WriteBehaviours()

chipy.utilities_logMes('WRITE DRIVEN DOF')
chipy.WriteDrivenDof()

###
chipy.OpenDisplayFiles()

chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

chipy.utilities_logMes('COMPUTE STIFFNESS')
chipy.ComputeBulk()
chipy.AssembleMechanicalLHS()

# precondensation
# mecaMAILx_SetPreconAllBodies()
# CSxxx_PushPreconNodes()
# ASpxx_PushPreconNodes()
# mecaMAILx_ComputePreconW()

for k in range(1,nb_steps+1,1):
   #
   chipy.utilities_logMes('INCREMENT STEP')
   chipy.IncrementStep()

   chipy.utilities_logMes('DISPLAY TIMES')
   chipy.TimeEvolution_DisplayStep()

   chipy.utilities_logMes('COMPUTE Fext')
   chipy.ComputeFext()

   chipy.utilities_logMes('COMPUTE Fint')
   chipy.ComputeBulk()

   chipy.utilities_logMes('ASSEMBLING')
   chipy.AssembleMechanicalRHS()

   chipy.utilities_logMes('COMPUTE Free Vlocy')
   chipy.ComputeFreeVelocity()

   chipy.utilities_logMes('SELECT PROX TACTORS')
   chipy.SelectProxTactors(freq_detect)

   chipy.utilities_logMes('RESOLUTION' )
   chipy.RecupRloc()
   chipy.nlgs_3D_ExSolver(stype, quad, tol, relax, gs_it1, gs_it2)
   chipy.StockRloc()

   chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
   chipy.ComputeDof()

   chipy.utilities_logMes('UPDATE DOF, FIELDS')
   chipy.UpdateStep()

   chipy.utilities_logMes('WRITE out')
   chipy.WriteOut(freq_write)
   ###
   chipy.WriteDisplayFiles(freq_display)

###
chipy.CloseDisplayFiles()
chipy.Finalize()
