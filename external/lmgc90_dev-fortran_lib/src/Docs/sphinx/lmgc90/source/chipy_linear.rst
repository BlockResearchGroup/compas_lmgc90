
Time loop for linear deformable bodies
======================================

Here is a typical time loop for the modelling of a linear deformable
sample (assumes *deformable = 1*) ::

  # since constant compute elementary mass matrices once 
  chipy.utilities_logMes('COMPUTE MASS')
  chipy.ComputeMass()

  # since constant compute elementary stiffness matrices once 
  chipy.utilities_logMes('COMPUTE STIFFNESS')  
  chipy.ComputeBulk()

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

    chipy.checkInteractiveCommand()

