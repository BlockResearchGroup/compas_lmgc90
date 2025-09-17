from pylmgc90 import chipy

def init(dt, hfile, restart=0, restart_postpro=0, logmes=True):

    chipy.Initialize()
    chipy.checkDirectories()

    if not logmes:
        chipy.utilities_DisableLogMes()

    dim = 3
    mhyp = 0

    theta = 0.51

    # interaction parameters
    chipy.PRPRx_UseCpCundallDetection(300)
    
    chipy.mecaMAILx_BandStorage()

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
    if restart>0:
        chipy.io_hdf5_read(hfile, restart)
    
    #
    # open display & postpro
    #
    chipy.utilities_logMes('DISPLAY & WRITE')
    chipy.OpenDisplayFiles(restart)
    chipy.OpenPostproFiles(restart_postpro)

    chipy.InitHDF5(hfile)


def compute(nb_steps, freq_write, freq_display):
    #
    # simulation part ...
    #

    Rloc_tol = 5.e-2

    # nlgs parameters
    tol = 1e-4
    relax = 1.0
    norm = 'QM/16'
    gs_it1 = 200
    gs_it2 = 10
    solver_type='Stored_Delassus_Loops         '

    # ... calls a simulation time loop
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



def finalize():
    #
    # close display & postpro
    #
    chipy.CloseDisplayFiles()
    chipy.ClosePostproFiles()
    
    # this is the end
    chipy.Finalize()
