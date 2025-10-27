from pylmgc90 import chipy

def init_lmgc90(dt, theta, dim, mhyp, deformable, output_file=None, wd=None):

  chipy.Initialize()

  if wd is not None:
    chipy.overall_SetWorkingDirectory(wd)

  chipy.SetDimension(dim,mhyp)
  
  chipy.checkDirectories()
  #chipy.utilities_DisableLogMes()

  #chipy.mecaMAILx_SparseStorage()
  #chipy.therMAILx_SparseStorage()

  chipy.TimeEvolution_SetTimeStep(dt)
  chipy.Integrator_InitTheta(theta)

  chipy.ReadDatbox(deformable)
 
  #TimeEvolution_WriteLastDof()
  #mecaMAILx_WriteLastDof()
  chipy.utilities_logMes('DISPLAY & WRITE')
  if output_file:
      chipy.InitHDF5(output_file)
  chipy.OpenDisplayFiles()
  #chipy.OpenPostproFiles()
  
  if dim == 2:
      chipy.CLxxx_SetNbNodesByCLxxx(1)
      chipy.PT2Dx_SetDisplayRadius(5.e-3)
  else:
      chipy.PRPRx_UseCpCundallDetection(40)
      chipy.PT3Dx_SetDisplayRadius(5.e-3)
  
  # since constant compute elementary mass matrices once
  chipy.utilities_logMes('COMPUTE MASS')
  chipy.ComputeMass()
  
def compute_lmgc90_one_step(solver_param, freq_write, freq_display):
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
  chipy.AssembleMechanicalLHS()
  chipy.AssembleMechanicalRHS()
  chipy.AssembleThermalLHS()
  chipy.AssembleThermalRHS()
  chipy.AssemblePoroLHS()
  chipy.AssemblePoroRHS()
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
  chipy.utilities_logMes('WRITE OUT')
  chipy.WriteOut(freq_write)
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
   
   chipy.WriteLastDof()
   chipy.WriteLastGPV()
   chipy.WriteLastVlocRloc()
   # this is the end
   chipy.Finalize()

