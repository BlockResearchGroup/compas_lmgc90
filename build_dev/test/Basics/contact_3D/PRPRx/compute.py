
from pylmgc90 import chipy

def init_lmgc90(dt, theta, dim, mhyp, deformable, path=None):

  if path is not None:
    chipy.overall_SetWorkingDirectory(path)

  chipy.Initialize()
  
  chipy.checkDirectories()
  #chipy.utilities_DisableLogMes()

  chipy.SetDimension(dim,mhyp)

  chipy.TimeEvolution_SetTimeStep(dt)
  chipy.Integrator_InitTheta(theta)

  chipy.POLYR_SkipAutomaticReorientation()
  chipy.ReadDatbox(deformable)
  
  #TimeEvolution_WriteLastDof()
  #mecaMAILx_WriteLastDof()
  if dim==2 :
    chipy.CLxxx_SetNbNodesByCLxxx(1)
  
  chipy.utilities_logMes('DISPLAY & WRITE')
  chipy.OpenDisplayFiles(write_f2f=True)
  #chipy.OpenDisplayFiles(write_f2f=True)
  #chipy.OpenPostproFiles()
  
  # since constant compute elementary mass matrices once
  chipy.utilities_logMes('COMPUTE MASS')
  chipy.ComputeMass()
  
  # since constant compute elementary stiffness matrices once
  chipy.utilities_logMes('COMPUTE STIFFNESS')
  chipy.ComputeBulk()
  
  # since constant compute iteration matrix once
  chipy.AssembleMechanicalLHS()
  
def compute_lmgc90_one_step(solver_param, freq_display, freq_write):
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()
  #
  #utilities_logMes('DISPLAY TIMES')
  #TimeEvolution_DisplayStep()
  #
  chipy.utilities_logMes('COMPUTE Fext')
  chipy.ComputeFext()
  #
  chipy.utilities_logMes('COMPUTE Fint')
  chipy.ComputeBulk()
  #
  chipy.utilities_logMes('ASSEMBLAGE')
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
  chipy.ExSolver(**solver_param)
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
  chipy.utilities_logMes('WRITE OUT DOF')
  chipy.WriteOutDof(freq_write)
  #
  chipy.utilities_logMes('WRITE OUT GPV')
  chipy.WriteOutGPV(freq_write)
  #
  chipy.utilities_logMes('WRITE OUT Rloc')
  chipy.WriteOutVlocRloc(freq_write)
  #
  #chipy.WriteHDF5(freq_write)
  #
  chipy.utilities_logMes('VISU & POSTPRO')
  chipy.WriteDisplayFiles(freq_display)
  #chipy.WritePostproFiles()


def finalize_lmgc90():
   #
   # close display & postpro
   #
   chipy.CloseDisplayFiles()
   #chipy.ClosePostproFiles()
   
   # this is the end
   chipy.Finalize()

