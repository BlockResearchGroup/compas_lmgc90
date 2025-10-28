import collections

from . import lmgc90
from . import macro

def initialize(dim, dt, theta, mhyp=0, h5_file=None, deformable=False, logmes=False, restart=None):
    """
    Initialize a (linear) LMGC90 simulation with some default parameters.

    :param dim: (integer) dimension of the simulation (2 or 3)
    :param dt: (real) time step of the simulation
    :param theta: (real) value of the theta integrator ( value in [0.,1.])
    :param mhyp: (integer) modeling hypothesis use for deformable bodies:
                 * 0 = 3D (default value)
                 * 1 = plain strain
                 * 2 = plain stress
                 * 3 = axi-symmetry
    :param h5_file: (string optional) HDF5 file in which to save the computation.
                    If not set, only text files in the OUTBOX directory will be available.
    :param deformable: (boolean) with deformable bodies (default to false)
    :param logmes: (boolean optional) set to True to activate LMGC90 log messaging.
    :param restart: (one or two integers) restart parameters to put in ReadIni and OpenDisplayFiles
    """

    lmgc90.utilities_setStopMode(False)

    macro.Initialize()
    macro.checkDirectories()

    if not logmes:
        lmgc90.utilities_DisableLogMes()

    # Set space dimension
    mhyp = 1 if dim==2 and not deformable else mhyp
    macro.SetDimension(dim,mhyp)
    #
    lmgc90.utilities_logMes('INIT TIME STEPPING')
    macro.TimeEvolution_SetTimeStep(dt)
    macro.Integrator_InitTheta(theta)
    #
    lmgc90.utilities_logMes('READ DATBOX')
    macro.ReadDatbox(deformable)

    if restart is None:
      r_disp = 1
    else:
      if isinstance(restart, collections.abc.Sequence):
        r_file = restart[0]
        r_disp = restart[1]
      else:
        r_file = restart
        r_disp = restart+1
      macro.ReadIni(record=r_file, h5_file=h5_file)

    # open display & postpro
    lmgc90.utilities_logMes('DISPLAY & WRITE')
    macro.OpenDisplayFiles(r_disp)
    macro.OpenPostproFiles()
    
    # if HDF5 is available
    if h5_file is not None:
        macro.InitHDF5(h5_file)

    # since constant compute elementary mass once
    lmgc90.utilities_logMes('COMPUTE MASS')
    macro.ComputeMass()

    if deformable :
        lmgc90.utilities_logMes('COMPUTE STIFFNESS')
        macro.ComputeBulk()
        lmgc90.utilities_logMes('ASSEMB KT')
        macro.AssembleMechanicalLHS()


def initialize_non_linear(dim, dt, theta, mhyp, t_final, dt_min, dt_max,
                          NR_max_iter, NR_adapt, h5_file=None, logmes=False):
    """
    Initialize an LMGC90 simulation involving non linear deformable bodies

    :param dim: (integer) dimension of the simulation (2 or 3)
    :param dt: (real) time step of the simulation
    :param theta: (real) value of the theta integrator ( value in [0.,1.])
    :param mhyp: (integer) modeling hypothesis use for deformable bodies:
                 * 0 = 3D
                 * 1 = plain strain
                 * 2 = plain stress
                 * 3 = axi-symmetry
    :param t_final: (real) desired final time of the simulation
    :param dt_min: (real) mininum time step allowed in the adaptative process
    :param dt_max: (real) maxinum time step allowed in the adaptative process
    :param NR_max_iter: (integer) maxinum number of iteration allowed in the Newton-Raphson loop
    :param NR_adapt: (integer) number of consecutive iterations with same status allowing to decide
                     if the time step should increase of decreased.
    :param h5_file: (optional) HDF5 file in which to save the computation.
                    If not set, only text files in the OUTBOX directory will be available.
    :param logmes: (optional) set to True to activate LMGC90 log messaging.
    """

    macro.Initialize()
    macro.checkDirectories()
    
    # Newton loop parameters:
    macro.NewtonRaphson_SetFinalTime(t_final)
    macro.NewtonRaphson_SetMinTimeStep(dt_min)
    macro.NewtonRaphson_SetMaxTimeStep(dt_max)
    macro.NewtonRaphson_SetMaxIter(NR_max_iter)
    macro.NewtonRaphson_SetIncPatience(NR_adapt)

    if not logmes:
        lmgc90.utilities_DisableLogMes()
    
    # Set space dimension
    macro.SetDimension(dim,mhyp)
    #
    lmgc90.utilities_logMes('INIT TIME STEPPING')
    macro.TimeEvolution_SetTimeStep(dt)
    macro.Integrator_InitTheta(theta)
    #
    lmgc90.utilities_logMes('READ BEHAVIOURS')
    macro.ReadBehaviours()
    macro.ReadModels()
    #
    lmgc90.utilities_logMes('READ BODIES')
    macro.ReadBodies()
    #
    lmgc90.utilities_logMes('LOAD BEHAVIOURS')
    macro.LoadBehaviours()
    macro.LoadModels()
    #
    lmgc90.utilities_logMes('READ DRIVEN DOF')
    macro.ReadDrivenDof()
    #
    lmgc90.utilities_logMes('LOAD TACTORS')
    macro.LoadTactors()
    #
    lmgc90.utilities_logMes('READ INI')
    macro.ReadIni()

    # paranoid writes
    lmgc90.utilities_logMes('WRITE BODIES')
    macro.WriteBodies()
    lmgc90.utilities_logMes('WRITE BEHAVIOURS')
    macro.WriteBehaviours()
    lmgc90.utilities_logMes('WRITE DRIVEN DOF')
    macro.WriteDrivenDof()

    # open display & postpro
    lmgc90.utilities_logMes('DISPLAY & WRITE')
    macro.OpenDisplayFiles()
    macro.OpenPostproFiles()
    
    # if HDF5 is available
    if h5_file is not None:
        macro.InitHDF5(h5_file)

    # since constant compute elementary mass once
    lmgc90.utilities_logMes('COMPUTE MASS')
    macro.ComputeMass()

def one_step(stype, norm, tol, relax, gs_it1, gs_it2,
             f_write, f_display                      ):
    """
    Compute one step of a computation with rigids or linear deformable bodies.

    :param stype: type of contact solver to use can only be:
                  * 'Stored_Delassus_Loops         '
                  * 'Exchange Local Global         '
    :param norm: type of norm to use in contact solver to check convergence, can be:
                 * 'Quad '
                 * 'Maxm '
                 * 'QM/16'
    :param tol: (real) desired tolerance to decided if contact solver has converged.
    :param relax: (real) relaxation
    :param gs_it1: (integer) maximum number of converge check of the contact solver before stopping (outer loop).
    :param gs_it2: (integer) number of contact solver iteration to run before checking convergence (inner loop).
    :param f_write: (integer) frequency at which to save into file(s).
    :param f_display: (integer) frequency at which to save display files (if 0, no file generated).
    """

    lmgc90.utilities_logMes('INCREMENT STEP')
    macro.IncrementStep()

    lmgc90.utilities_logMes('COMPUTE Fext')
    macro.ComputeFext()
    lmgc90.utilities_logMes('COMPUTE Fint')
    macro.ComputeBulk()

    lmgc90.utilities_logMes('ASSEMB RHS')
    macro.AssembleMechanicalRHS()

    lmgc90.utilities_logMes('COMPUTE Free Vlocy')
    macro.ComputeFreeVelocity()

    lmgc90.utilities_logMes('SELECT PROX TACTORS')
    macro.SelectProxTactors()

    lmgc90.utilities_logMes('RESOLUTION' )
    macro.RecupRloc()

    macro.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
    macro.UpdateTactBehav()

    macro.StockRloc()

    lmgc90.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
    macro.ComputeDof()

    lmgc90.utilities_logMes('UPDATE DOF, FIELDS')
    macro.UpdateStep()

    lmgc90.utilities_logMes('WRITE OUT')
    macro.WriteOut(f_write)

    if f_display > 0:
        lmgc90.utilities_logMes('VISU & POSTPRO')
        macro.WriteDisplayFiles(f_display)

    macro.WritePostproFiles()

    macro.checkInteractiveCommand()


def one_step_non_linear(NR_tol, stype, norm, tol, relax, gs_it1, gs_it2,
                        f_write, f_display                              ):
    """
    Compute one step of a computation for non linear deformable bodies.

    :param NR_tol: tolerance to check convergence in the Newton-Raphson resolution loop
    :param stype: type of contact solver to use can only be:
                  * 'Stored_Delassus_Loops         '
                  * 'Exchange Local Global         '
    :param norm: type of norm to use in contact solver to check convergence, can be:
                 * 'Quad '
                 * 'Maxm '
                 * 'QM/16'
    :param tol: (real) desired tolerance to decided if contact solver has converged.
    :param relax: (real) relaxation
    :param gs_it1: (integer) maximum number of converge check of the contact solver before stopping (outer loop).
    :param gs_it2: (integer) number of contact solver iteration to run before checking convergence (inner loop).
    :param f_write: (integer) frequency at which to save into file(s).
    :param f_display: (integer) frequency at which to save display files (if 0, no file generated).
    :return: return True or False depending if the while loop must be broken.
    """

    lmgc90.utilities_logMes('INCREMENT STEP')
    macro.IncrementStep()
  
    lmgc90.utilities_logMes('COMPUTE Fext')
    macro.ComputeFext()
  
    # Newton loop
    macro.NewtonRaphson_Initialize(NR_tol)
    is_converged = 1
    k=0
    #looping until something changes in CheckConvergence
    while is_converged == 1 :
        k+=1
        lmgc90.utilities_logMes('COMPUTE BULK')
        macro.ComputeBulk()
  
        lmgc90.utilities_logMes('ASSEMB RHS/KT')
        macro.AssembleMechanicalRHS()
        macro.AssembleMechanicalLHS()
  
        lmgc90.utilities_logMes('COMPUTE Free Vlocy')
        macro.ComputeFreeVelocity()
        #
        lmgc90.utilities_logMes('SELECT PROX TACTORS')
        macro.SelectProxTactors()
        #
        ### Signorini Coulomb
        macro.RecupRloc()
        macro.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
        macro.StockRloc()
        ###
        lmgc90.utilities_logMes('COMPUTE DOF')
        macro.ComputeDof()
        #
        if k > 1:
          NR_norm = macro.mecaMAILx_ComputeResidueNorm()
          is_converged = macro.NewtonRaphson_CheckConvergence(NR_norm)
  
    ### end while NR
  
    lmgc90.utilities_logMes('COMPUTE TIME STEP')
    #istate = 1 => redo step
    #istate = 2 => stop
  
    istate = macro.NewtonRaphson_ComputeTimeStep()
  
    if not istate == 1 :
  
        lmgc90.utilities_logMes('UPDATE TACT BEHAV')
        macro.UpdateTactBehav()
        macro.StockRloc()
  
        lmgc90.utilities_logMes('UPDATE DOF')
        macro.UpdateStep()
        #
        ### write results ###
        #
        macro.WriteOut(f_write)
  
        if f_display > 0:
            macro.WriteDisplayFiles(f_display)
        macro.WritePostproFiles()
  
        macro.checkInteractiveCommand()
  
        if istate == 2 :
          # istate => Stop
          return True

    ### end while time loop ###
    return False


def finalize(cleanup=True):
    """
    Finalize an LMGC90 computation.

    :param cleanup: boolean stating if the memory of LMGC90 must be purged
                    (True by default)
    """

    macro.CloseDisplayFiles()
    macro.ClosePostproFiles()
    
    # this is the end
    if cleanup:
        macro.Finalize()

