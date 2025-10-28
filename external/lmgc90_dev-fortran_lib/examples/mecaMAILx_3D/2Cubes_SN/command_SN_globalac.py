import os

from pylmgc90 import chipy

chipy.Initialize()

chipy.utilities_DisableLogMes()

chipy.SetDimension(3,0)

#working directory for outputs
wd = os.path.join( os.getcwd(), 'SN_globalac' )

####
# info gestion du temps
dt = 1.e-3
theta = 0.505
nb_steps = 10

freq_write=1

# bavardage de certaines fonctions
echo = 0

# info generation fichier visu
freq_display = 1

# info contact
freq_detect = 1

#             123456789012345678901234567890
solver      ='globalac                      '
itermax     = 500
tol         = 1.e-6
freq_err    = 1
relax       = 1. 
verbose     = 1     # 0: no, 1: yes
output      = 0     # 0 off, 1 C file, 2 dat, 3 FClib
freq_output = 0     #

chipy.SiconosNumerics_SetParameters(solver, tol, freq_err, itermax, relax,\
                                    verbose, output, freq_output)

chipy.mecaMAILx_SparseStorage()
chipy.mecaMAILx_UnspecifiedShape()

###
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

### lecture du modele ###
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
   chipy.mecaMAILx_PrepGlobalSolver()
   chipy.RBDY3_ComputeFreeVelocity()

   chipy.utilities_logMes('SELECT PROX TACTORS')
   chipy.SelectProxTactors(freq_detect)

   chipy.utilities_logMes('RESOLUTION' )
   chipy.RecupRloc()
   chipy.SiconosNumerics_ExSolver()
   chipy.StockRloc()

   chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
   chipy.mecaMAILx_PostGlobalSolver()
   chipy.mecaMAILx_ComputeField()
   chipy.RBDY3_ComputeDof()

   chipy.utilities_logMes('UPDATE DOF, FIELDS')
   chipy.UpdateStep()

   chipy.utilities_logMes('WRITE out')
   chipy.WriteOut(freq_write)
   ###
   chipy.WriteDisplayFiles(freq_display)

###
chipy.CloseDisplayFiles()
chipy.Finalize()
