import os
from pylmgc90 import chipy, pre

# space dimension
dim = 2

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1

# time evolution parameters
dt   = 1e-3
step = 10

# theta integrator parameter
theta = 0.501

# interaction parameters
Rloc_tol = 5.e-3

# nlgs parameters
solver_param = { 'conv'  : 1e-4   ,
                 'relax' : 1.0    ,
                 'norm'  : 'Quad ',
                 'gsit1' : 50     ,
                 'gsit2' : 1000   ,
                 'type'  : 'Stored_Delassus_Loops         '
               }


h5_files = ('v0_1.h5', 'v0_2.h5',)

gen_path = 'pre'
if not os.path.isdir(gen_path):
    os.mkdir(gen_path)

for h5_file in h5_files:

    containers = pre.readDatbox(dim, "DATBOX", step, h5_file)
    pre.writeDatbox(dim, *containers, datbox_path=gen_path+"/DATBOX")
    
    chipy.Initialize()
    
    chipy.overall_SetWorkingDirectory(gen_path)
    chipy.checkDirectories()
    #chipy.utilities_DisableLogMes()
    
    chipy.SetDimension(dim,mhyp)
    
    chipy.TimeEvolution_SetTimeStep(dt)
    chipy.Integrator_InitTheta(theta)
    
    chipy.utilities_logMes('READ BEHAVIOURS')
    chipy.ReadBehaviours()
    chipy.ReadModels()
    
    chipy.utilities_logMes('WRITE BEHAVIOURS')
    chipy.WriteBehaviours()
    chipy.models_WriteModels()
    
    chipy.utilities_logMes('READ BODIES')
    chipy.ReadBodies()
    #
    #
    chipy.utilities_logMes('LOAD BEHAVIOURS')
    chipy.LoadBehaviours()
    chipy.LoadModels()
    #
    chipy.utilities_logMes('LOAD TACTORS')
    chipy.LoadTactors()
    chipy.ReadDrivenDof()
    #
    chipy.utilities_logMes('READ INI')
    chipy.ReadIni()
    
    #
    chipy.CLxxx_SetNbNodesByCLxxx(1)
    
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
    inter_ids   =  (chipy.CLALp_ID, chipy.CLJCx_ID,
                    chipy.DKALp_ID, chipy.DKDKx_ID,
                    chipy.DKJCx_ID, chipy.DKKDx_ID,
                    chipy.DKPLx_ID, chipy.PLALp_ID,
                    chipy.PLJCx_ID, chipy.PLPLx_ID,
                    chipy.PTPT2_ID, chipy.P2P2L_ID,
                   )
    
    for inter_id in inter_ids:
        nb_inter = chipy.inter_handler_2D_tgetNb(inter_id)
        nb_recup = chipy.inter_handler_2D_getNbRecup(inter_id)
        msg = f"[{h5_file}:{inter_names[inter_id]}] found {nb_inter}, recup {nb_recup}"
        assert nb_inter == nb_recup, msg
    
    # this is the end
    chipy.Finalize()
    
