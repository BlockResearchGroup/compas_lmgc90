
Time loop for non-linear deformable bodies 
==========================================

Additional parameters are necessary at the beginning of the script ::

  t_final = 0.1E0
  dt_min = dt
  dt_max = dt

  NR_max_iter = 20
  NR_adapt = 9999999
  NR_tol = 1.e-3

Here is a typical time loop (assumes *deformable=1*) ::

  # Newton loop parameters:
  chipy.NewtonRaphson_SetFinalTime(t_final)
  chipy.NewtonRaphson_SetMinTimeStep(dt_min)
  chipy.NewtonRaphson_SetMaxTimeStep(dt_max)
  chipy.NewtonRaphson_SetMaxIter(NR_max_iter)
  chipy.NewtonRaphson_SetIncPatience(NR_adapt)

  chipy.utilities_logMes('COMPUTE MASS')
  chipy.ComputeMass()

  while chipy.TimeEvolution_GetTime() < t_final :
    #
    chipy.utilities_logMes('INCREMENT STEP')
    chipy.IncrementStep()

    chipy.utilities_logMes('COMPUTE Fext')
    chipy.ComputeFext()

    # Newton loop
    chipy.NewtonRaphson_Initialize(NR_tol)
    is_converged = 1
    k=0
    #looping until something changes in CheckConvergence
    while is_converged == 1 : 
      k+=1            
      chipy.utilities_logMes('COMPUTE BULK')
      chipy.ComputeBulk()
     
      chipy.utilities_logMes('ASSEMB RHS/KT')
      chipy.AssembleMechanicalRHS()
      chipy.AssembleMechanicalLHS()

      chipy.utilities_logMes('COMPUTE Free Vlocy')
      chipy.ComputeFreeVelocity()
      #
      chipy.utilities_logMes('SELECT PROX TACTORS')
      chipy.SelectProxTactors()
      #
      ### Signorini Coulomb
      chipy.RecupRloc()
      chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
      chipy.StockRloc()
      ###
      chipy.utilities_logMes('COMPUTE DOF')
      chipy.ComputeDof()
      #
      if k > 1:
        NR_norm = chipy.mecaMAILx_ComputeResidueNorm()
        is_converged = chipy.NewtonRaphson_CheckConvergence(NR_norm)

    ### end while NR

    chipy.utilities_logMes('COMPUTE TIME STEP')
    #istate = 1 => redo step
    #istate = 2 => stop

    istate = chipy.NewtonRaphson_ComputeTimeStep()

    if not istate == 1 :

      chipy.utilities_logMes('UPDATE TACT BEHAV')
      chipy.UpdateTactBehav()
      chipy.StockRloc()

      chipy.utilities_logMes('UPDATE DOF')
      chipy.UpdateStep()
      #
      ### write results ###
      #
      chipy.WriteOut(freq_write)

      chipy.WriteDisplayFiles(freq=freq_display)
      chipy.WritePostproFiles()

      chipy.checkInteractiveCommand()

      if istate == 2 :
        # istate => Stop
        break

  ### end while time loop ###


