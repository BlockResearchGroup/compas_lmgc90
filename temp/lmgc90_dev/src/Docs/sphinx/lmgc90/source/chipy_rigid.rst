
Time loop for rigid bodies
==========================

Here is a typical time loop for the modelling of a rigid sample (assumes *deformable = 0*) ::

  # since constant compute elementary mass once 
  chipy.utilities_logMes('COMPUTE MASS')
  chipy.ComputeMass()

  for k in range(nb_steps):
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

    chipy.checkInteractiveCommand()

