
import filecmp
import shutil
from pathlib import Path

import h5py

from pylmgc90 import pre, chipy

def generate():

    datbox = Path('./DATBOX')
    datbox.mkdir(exist_ok=True)
    
    # containers
    bodies = pre.avatars()
    mats   = pre.materials()
    mods   = pre.models()
    sees   = pre.see_tables()
    tacts  = pre.tact_behavs()
    
    # 2D
    dim = 2
    
    # mod & mat
    mot = pre.model(name='M2D_T',physics='MECAx',element='T3xxx',dimension=2, external_model='MatL_',
                    kinematic='small',material='elas_',anisotropy='iso__',mass_storage='coher')
    mods+=mot
    
    moq = pre.model(name='M2D_Q',physics='MECAx',element='Q4xxx',dimension=2, external_model='MatL_',
                    kinematic='small',material='elas_',anisotropy='iso__',mass_storage='coher')
    mods+=moq
    
    ma = pre.material(name='steel',materialType='ELAS',elas='standard',
                      young=0.1e+15,nu=0.2,anisotropy='isotropic',
                      density=0.25e+4)
    mats.addMaterial(ma)
    
    
    #  meshing
    mt = pre.buildMesh2D('4T3', x0=0.1, y0=1.0, lx=0.8, ly=1., nb_elem_x=1, nb_elem_y=1)
    
    #avatar
    bt = pre.buildMeshedAvatar(mt,mot,ma)
    bt.imposeDrivenDof(group='up',component=2,dofty='force',description='evolution',evolutionFile='Fy.txt')
    with open('./DATBOX/Fy.txt','w') as ofile:
        ofile.write('%12.5e %12.5e\n' % (0.,0.))
        ofile.write('%12.5e %12.5e\n' % (0.01,0.))
        ofile.write('%12.5e %12.5e\n' % (0.1,-1e4))
        ofile.write('%12.5e %12.5e\n' % (100.,-1e4))
        ofile.close()
    
    bt.addContactors(group='down', shape='CLxxx', color='BLUEx')
    bodies+=bt
    
    mq = pre.buildMesh2D('Q4', x0=-0.2, y0=0., lx=1.4, ly=1., nb_elem_x=2, nb_elem_y=2)
    bq= pre.buildMeshedAvatar(mq,moq,ma)
    bq.imposeDrivenDof(group='down',component=[1,2],dofty='vlocy')
    bq.addContactors(group='up', shape='ALpxx', color='BLUEx')
    bodies+=bq
    
    # contact law
    lclal = pre.tact_behav(name='gapc0', law='GAP_SGR_CLB', fric=0.3)
    tacts+= lclal
    
    # visibility table
    vt = pre.see_table(CorpsCandidat   ='MAILx', candidat   ='CLxxx', colorCandidat   ='BLUEx',
                       CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='BLUEx',
                       behav=lclal,  alert=0.01, halo=0.4)
    sees+=vt
    
    
    # post processing commands
    post = pre.postpro_commands()
    post.addCommand(pre.postpro_command(name='SOLVER INFORMATIONS', step=1))
    pre.writePostpro(commands=post, parts=bodies, path=datbox)
    
    # Lets write
    pre.writeDatbox(dim, mats, mods, bodies, tacts, sees, gravy=[0., -9.81, 0.], datbox_path=datbox)

def compute():

    chipy.Initialize()
    chipy.utilities_setStopMode(False)
    
    # checking/creating mandatory subfolders
    chipy.checkDirectories()
    
    # logMes
    #chipy.utilities_DisableLogMes()
    
    dim = 2
    mhyp = 1
    
    dt = 1e-3
    nb_steps = 100
    
    theta = 0.5
    
    deformable = True
    Rloc_tol = 5.e-2
    
    tol = 1e-4
    relax = 1.0
    norm = 'Quad '
    gs_it1 = 50
    gs_it2 = 10
    solver_type='Stored_Delassus_Loops         '
    
    freq_write   = 10
    freq_display = 10
    hfile = 'lmgc90.h5'
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
    chipy.utilities_logMes('READ DATBOX')
    chipy.ReadDatbox(deformable)
    
    #
    # open display & postpro
    #
    
    chipy.utilities_logMes('DISPLAY & WRITE')
    chipy.OpenDisplayFiles()
    chipy.OpenPostproFiles()
    
    # if HDF5 is available
    chipy.InitHDF5(hfile)
    
    #
    # simulation part ...
    #
    
    chipy.ComputeMass()
    chipy.ComputeBulk()
    chipy.AssembleMechanicalLHS()
    
    for k in range(1, nb_steps + 1, 1):
        chipy.IncrementStep()
        chipy.ComputeFext()
        chipy.ComputeBulk()
        chipy.AssembleMechanicalRHS()
        chipy.ComputeFreeVelocity()
        chipy.SelectProxTactors()
        chipy.RecupRloc()
        chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
        chipy.UpdateTactBehav()
        chipy.StockRloc()
        chipy.ComputeDof()
        chipy.UpdateStep()
        #
        chipy.utilities_logMes('WRITE OUT')
        chipy.WriteOut(freq_write)
        chipy.WriteOutDof(freq_write)
        chipy.WriteOutVlocRloc(freq_write)
        chipy.WriteOutGPV(freq_write)
        #
        chipy.utilities_logMes('VISU & POSTPRO')
        chipy.WriteDisplayFiles(freq_display)
        chipy.WritePostproFiles()
    
        chipy.checkInteractiveCommand()
    #
    # close display & postpro
    #
    chipy.CloseDisplayFiles()
    chipy.ClosePostproFiles()
    
    # this is the end
    chipy.Finalize()


def post():

    outbox = Path('OUTBOX')
    ref_outbox = Path('ref_OUTBOX')
    if ref_outbox.is_dir():
      shutil.rmtree(ref_outbox)
    outbox.rename('ref_OUTBOX')

    dim = 2
    mhyp = 1
    
    dt = 1e-3
    theta = 0.5
    
    deformable = True
    hfile = 'lmgc90.h5'

    chipy.Initialize()
    chipy.utilities_setStopMode(False)
    chipy.checkDirectories()
    
    chipy.SetDimension(dim,mhyp)
    #
    chipy.utilities_logMes('INIT TIME STEPPING')
    chipy.TimeEvolution_SetTimeStep(dt)
    chipy.Integrator_InitTheta(theta)
    #
    chipy.utilities_logMes('READ DATBOX')
    chipy.ReadDatbox(deformable)

    chipy.ComputeMass()
    chipy.ComputeBulk()
    chipy.AssembleMechanicalLHS()

    with h5py.File(hfile, 'r') as hf:
      nb_record = int( hf['Simulation/nb_record'][()] )

    for i_record in range(1, nb_record+1):
      chipy.ReadIni(i_record, hfile)   
      chipy.WriteOutDof()
      chipy.WriteOutGPV()
      chipy.WriteOutVlocRloc()

def check():
    for i in range(1,11):
        gpv = Path(f"GPV.OUT.{i}")
        assert filecmp.cmp( 'OUTBOX'/gpv, 'ref_OUTBOX'/gpv, shallow=False ), f"error comparing {gpv} file"

if __name__ == "__main__":
    generate()
    compute()
    post()
    check()
