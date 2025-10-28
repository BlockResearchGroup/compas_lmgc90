from pylmgc90 import chipy

# space dimension
dim = 3

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 0

# time evolution parameters
dt   = 4e-3
step = 3

# theta integrator parameter
theta = 0.501

# interaction parameters
Rloc_tol = 5.e-3

h5_files = ('v0_1.h5', 'v0_2.h5')

for h5_file in h5_files:

    chipy.Initialize()
    
    chipy.checkDirectories()
    #chipy.utilities_DisableLogMes()
    
    chipy.SetDimension(dim,mhyp)
    
    chipy.TimeEvolution_SetTimeStep(dt)
    chipy.Integrator_InitTheta(theta)
    
    chipy.utilities_logMes('READ DATBOX')
    chipy.ReadDatbox(deformable=True)
    #
    chipy.utilities_logMes('READ INI')
    chipy.ReadIni(step, h5_file)
    #
    chipy.PRPRx_UseCpCundallDetection(40)
    
    # since constant compute elementary mass matrices once
    chipy.utilities_logMes('COMPUTE MASS')
    chipy.ComputeMass()
    
    # since constant compute elementary stiffness matrices once
    chipy.utilities_logMes('COMPUTE STIFFNESS')
    chipy.ComputeBulk()
    
    # since constant compute iteration matrix once
    chipy.AssembleMechanicalLHS()
    
    # computation step
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
    
    inter_names = { chipy.parameters_getInteractionId(name):name for name in chipy.parameters_getInteractionNames() }
    inter_ids   =  (chipy.CDCDx_ID, chipy.CDPLx_ID,
                    chipy.CSASp_ID, chipy.PRASp_ID,
                    chipy.CSPRx_ID, chipy.PRPLx_ID,
                    chipy.PTPT3_ID, chipy.SPCDx_ID,
                    chipy.SPDCx_ID, chipy.SPPLx_ID,
                    chipy.SPSPx_ID, chipy.PRPRx_ID,)
    for inter_id in inter_ids:
        if h5_file == 'v0_1.h5' and (inter_id == chipy.CSASp_ID or inter_id == chipy.CSPRx_ID) :
            continue
        nb_inter = chipy.inter_handler_3D_tgetNb(inter_id)
        nb_recup = chipy.inter_handler_3D_getNbRecup(inter_id)
        msg = f"[{h5_file}:{inter_names[inter_id]}] found {nb_inter}, recup {nb_recup}"
        assert nb_inter == nb_recup, msg
    
    # this is the end
    chipy.Finalize()
 
