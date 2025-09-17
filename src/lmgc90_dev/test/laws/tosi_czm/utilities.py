
import sys
import itertools
from pathlib import Path
from copy import deepcopy

import numpy as np
from matplotlib import pyplot as plt
import h5py

from pylmgc90 import pre, chipy

MECAx_SETS = {}

def generate(dim=2, incre=False, loading='Normal', path='.'):
    global MECAx_SETS

    # ecriture des fichiers
    datbox = Path(path)
    datbox = datbox/'DATBOX'
    datbox.mkdir(exist_ok=True, parents=True)
    
    # size
    x0 = y0 = z0 = 0.
    lx = ly = lz = 1e-4
    nx = ny = nz = 1

    # law params:
    law_params = { 'name'  : 'tosi_'   , 'law'   : 'TOSI_CZM',
                   'dyfr'  : 0.1       , 'stfr'  : 0.1       ,
                   'cn'    : 1.e17     , 'ct'    : 1.e17     , 
                   'f0'    : 0.04      , 'fc'    : 0.9       ,
                   'n'     : 6.        , 'kcoal' : 1e5       ,
                   'R'     : 8.314     , 's0'    : 5.0e6     ,
                   'Gc1'   : 1.        , 'Gc2'   : 1.        ,
                   'K'     : 29130.0   , 'hdef'  : 20e-6     ,
                   'n_mol' : 60.       , 'Q'     : 482000.0  ,
                   }
    if incre:
      law_params['law'] = 'TOSI_CZM_INCRE'

    # see table:
    col = 'tosi_'
    alert = 1e-3
    halo  = 1e-3

    # loads:
    nb_half_cycles = 3
    dt    = 1.e-3
    t0    = 0.
    load  = [(0., 0.,),]
    for i in range(1, nb_half_cycles+1) :
      load.append( ( (2*i-1)*dt , (-1)**(i-1)*2e-6 ,) )
      load.append( ( (2*i  )*dt , (-1)**(i-1)*2e-6 ,) )
    lfile = 'Vload.txt'

    bodies = pre.avatars()
    mats   = pre.materials()
    mods   = pre.models()
    sees   = pre.see_tables()
    tacts  = pre.tact_behavs()
    posts  = pre.postpro_commands()
    
    steel = pre.material(name='Steel', materialType='ELAS', elas='standard',
                         young=210e15, nu=0.3, anisotropy='isotropic', density=7800.)  
    mats.addMaterial(steel)

    if dim == 2:
        mdl = pre.model(name='M2DNL', physics='MECAx', element='Q4xxx', dimension=dim, external_model='MatL_',
                        kinematic='large', material='neoh_', anisotropy='iso__', mass_storage='lump_',formulation='TotaL')

        mesh_block = pre.buildMesh2D(mesh_type='Q4', x0=x0, y0=y0, lx=lx, ly=ly, nb_elem_x=nx, nb_elem_y=ny)

    else:

        mdl = pre.model(name='M3DNL', physics='MECAx', element='H8xxx', dimension=dim, external_model='MatL_',
                        kinematic='large', material='neoh_', anisotropy='iso__', mass_storage='lump_',formulation='TotaL')

        mesh_block = pre.buildMeshH8(x0=x0, y0=y0, z0=z0, lx=lx, ly=ly, lz=lz, nb_elem_x=nx, nb_elem_y=ny, nb_elem_z=nz)

    mods.addModel(mdl)

    cube1 = pre.buildMeshedAvatar(mesh=mesh_block, model=mdl, material=steel)
    cube2 = deepcopy(cube1)

    if dim == 2:

        cd = 'CLxxx'
        an = 'ALpxx'

        cube1.addContactors(group='up'  , shape='ALpxx', color=col)
        cube2.addContactors(group='down', shape='CLxxx', color=col, weights=[0.5])
        cube2.translate(dy=ly)

    else :

        cd = 'CSxxx'
        an = 'ASpxx'

        cube1.addContactors(group='up'  , shape='ASpxx', color=col)
        cube2.addContactors(group='down', shape='CSpxx', color=col, quadrature=0)
        cube2.translate(dz=lz)


    with open(datbox/lfile,'w') as ofile :
      for t, v in load:
        ofile.write(f"{t:12.5e} {v:12.5e}\n")


    if dim == 2:

      #v_load = 2 if loading == 'Normal' else 1
      (v_load,v_null) = (2,1) if loading == 'Normal' else (1,2)

      cube1.imposeDrivenDof(group='down', component=[1, 2], dofty='vlocy')
      cube2.imposeDrivenDof(group='up'  , component=v_null, dofty='vlocy')
      cube2.imposeDrivenDof(group='up'  , component=v_load, dofty='vlocy',
                            description='evolution', evolutionFile=lfile)

    else:

      (v_load, v_null) = (3, (1,2,) )  if loading == 'Normal' else (1, (2,3,) )

      cube1.imposeDrivenDof(group='down', component=[1,2,3], dofty='vlocy')
      #cube1.imposeDrivenDof(group='rear', component=1, dofty='vlocy')
      #cube2.imposeDrivenDof(group='rear', component=1, dofty='vlocy')
      #cube1.imposeDrivenDof(group='left', component=2, dofty='vlocy')
      #cube2.imposeDrivenDof(group='left', component=2, dofty='vlocy')

      cube2.imposeDrivenDof(group='up'  , component=v_load, dofty='vlocy',
                            description='evolution', evolutionFile=lfile)
      for v_n in v_null:
        cube2.imposeDrivenDof(group='up', component=v_n, dofty='vlocy')

    bodies += cube1
    bodies += cube2

    tlaw   = pre.tact_behav( **law_params )
    tacts += tlaw

    see = pre.see_table(CorpsCandidat   ='MAILx', candidat   =cd, colorCandidat   =col,
                        CorpsAntagoniste='MAILx', antagoniste=an, colorAntagoniste=col,
                        behav=tlaw, alert=alert, halo=halo)
    
    sees += see

    MECAx_SETS = { ('cube1', 'down',) : 1, 
                   ('cube1', 'up'  ,) : 2, 
                   ('cube2', 'down',) : 3, 
                   ('cube2', 'up'  ,) : 4, 
                 }
    mecax_sets = [ [(cube1, 'down',)], 
                   [(cube1, 'up'  ,)], 
                   [(cube2, 'down',)], 
                   [(cube2, 'up'  ,)], 
                 ]

    posts.addCommand(pre.postpro_command(name = 'NEW MECAx SETS'     , mecax_sets=mecax_sets))
    posts.addCommand(pre.postpro_command(name = 'Dep EVOLUTION'      , step = 1 )            )
    posts.addCommand(pre.postpro_command(name = 'Fint EVOLUTION'     , step = 1 )            )
    posts.addCommand(pre.postpro_command(name = 'SOLVER INFORMATIONS', step = 1 )            )
      
    pre.writeDatbox(dim, mats, mods, bodies, tacts, sees, gravy=[0.,0.,0.], post=posts, datbox_path=datbox)

    try:
      pre.visuAvatars(bodies)
    except:
      pass


def compute(dim, path):

    chipy.Initialize()

    chipy.overall_SetWorkingDirectory( path )
    chipy.checkDirectories()

    # logMes
    chipy.utilities_DisableLogMes()

    # modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
    mhyp = 0 if dim == 3 else 1

    # time evolution parameters
    t_scale = 1 if dim == 2 else 0.125
    dt = 2e-6 / t_scale
    nbsteps = int(5250 * t_scale)
    t_final = nbsteps*dt
    dt_min = dt
    dt_max = dt

    NR_max_iter = 20
    NR_adapt = 9999999
    NR_tol = 1.e-5

    # theta integrator parameter
    theta = 0.55
    
    # interaction parameters
    Rloc_tol = 5.e-2
    
    # nlgs parameters
    tol    = 1e-5
    relax  = 1.0
    norm   = 'Quad '
    gs_it1 = 1
    gs_it2 = 2
    stype  = 'Stored_Delassus_Loops         '
    
    # write parameter
    hfile     = 'lmgc90.h5'
    f_write   = 1#0#0
    
    # display parameters
    f_display = 500

    #
    # read and load
    #
    
    # Newton loop parameters:
    chipy.NewtonRaphson_SetFinalTime(t_final)
    chipy.NewtonRaphson_SetMinTimeStep(dt_min)
    chipy.NewtonRaphson_SetMaxTimeStep(dt_max)
    chipy.NewtonRaphson_SetMaxIter(NR_max_iter)
    chipy.NewtonRaphson_SetIncPatience(NR_adapt)
    
    # Set space dimension
    chipy.SetDimension(dim,mhyp)
    #
    chipy.utilities_logMes('INIT TIME STEPPING')
    chipy.TimeEvolution_SetTimeStep(dt)
    chipy.Integrator_InitTheta(theta)
    
    #
    chipy.ReadDatbox()

    if dim == 2:
        chipy.CLxxx_SetNbNodesByCLxxx(1)
    else:
        chipy.CSASp_SkipAutoContact()

    chipy.registerInterInternals( ['beta', 'energy'] )
    chipy.addRegistersToDisplay(True)
    #
    # open display & postpro
    #
    chipy.utilities_logMes('DISPLAY & WRITE')
    chipy.OpenDisplayFiles()
    chipy.OpenPostproFiles()
    
    chipy.InitHDF5( hfile )
    #
    # simulation part ...
    #
    chipy.utilities_logMes('COMPUTE MASS')
    chipy.ComputeMass()
    
    while chipy.TimeEvolution_GetTime() < t_final :
      #
        chipy.computation.one_step_non_linear(NR_tol, stype, norm, tol, relax,
                                              gs_it1, gs_it2, f_write, f_display)
    ### end while time loop ###
    
    chipy.ClosePostproFiles()
    chipy.Finalize()


def post_plot(dim=2, incre=False, loading='Normal', path='.'):
    global MECAx_SETS

    wd = Path(path)
    postpro = wd/'POSTPRO'

    fields = ['Dep', 'Fint']

    data = {}
    for f, s in itertools.product( fields, MECAx_SETS.keys() ):
        i = MECAx_SETS[s]
        fname = f"{f}_{i:07d}.DAT"
        data[(f,s)] = np.loadtxt( postpro/fname )


    idx = dim if loading == 'Normal' else 1
    u_jump =   data[('Dep' ,('cube2','down',),)][:,idx] \
             - data[('Dep' ,('cube1','up'  ,),)][:,idx] 
    force  = - data[('Fint',('cube2','down',),)][:,idx]

    # from hdf5 of inside computation ?
    with h5py.File( wd/'lmgc90.h5', 'r' ) as hf:
        nb_record = int( hf['Simulation/nb_record'][()] )
        int_idx   = hf['Help/VlocRloc/rdata/internals/bound'][0] - 1 #Fortran to C

        # because only one law !
        claw = hf['Help/parameters/inter_law/comment'][()].decode()
        claw = { n:i for i,n in enumerate(claw[1:].split()) }

        # index of internal starting with 0
        d = 'n' if loading == 'Normal' else 's' if dim == 3 else 't'
        u_idx  = int_idx + claw[f'saut_de_u{d}']
        f1_idx = int_idx + claw[f'energy1']
        f2_idx = int_idx + claw[f'energy2']
        b_idx  = int_idx + claw[f'beta']

        e_u = np.zeros( [4,nb_record], dtype=float )
        # should I assert that nb_inters == 1 ?
        for i in range(nb_record):
          if 'VlocRloc' not in hf[f"Evolution/ID_{i+1}"].keys() :
            break
          e_u[0, i] = hf[f"Evolution/ID_{i+1}/VlocRloc/rdata"][0, u_idx]
          e_u[1, i] = hf[f"Evolution/ID_{i+1}/VlocRloc/rdata"][0,f1_idx]
          e_u[2, i] = hf[f"Evolution/ID_{i+1}/VlocRloc/rdata"][0,f2_idx]
          e_u[3, i] = hf[f"Evolution/ID_{i+1}/VlocRloc/rdata"][0, b_idx]

    fig, (ax_F, ax_E, ax_P) = plt.subplots(1, 3)
    fig.suptitle(path)

    ax_F.set_title( f"{loading} force" )
    ax_F.set_xlabel(f"$[u_{loading[0]}]$" )
    ax_F.set_ylabel(f"$R_{loading[0]}$" )
    ax_F.plot( u_jump, force, 'b')

    ax_E.set_title( f"Cohesize Energie" )
    ax_E.set_xlabel(f"[$u_{loading[0]}$] (m)" )
    ax_E.set_ylabel(f"E ($J.m^{-2}$)" )
    ax_E.plot( e_u[0], e_u[1], 'b')
    ax_E.plot( e_u[0], e_u[2], 'r')

    ax_P.set_title( f"Beta" )
    ax_P.set_xlabel(f"[$u_{loading[0]}$] (m)" )
    ax_P.set_ylabel(f"$1-f$" )
    ax_P.plot( e_u[0], e_u[3], 'b')

    if '--plot' in sys.argv:
        plt.show()
    else:
        # save figure
        fname = str(wd/str(wd))+'.png'
        fig.tight_layout()
        dpi = fig.get_dpi()
        ds  = fig.get_size_inches()
        fig.set_size_inches( (ds[0]*1.5, ds[1]) )
        plt.savefig(fname)

    fname = str(wd)+'_ref.txt'
    if '--save-ref' in sys.argv:
        np.savetxt(fname, e_u)

    if '--no-check' in sys.argv:
        pass
    else:
        e_ref = np.loadtxt(fname)
        msg = f'[ERROR] differences in postpro value of {wd} example with value {np.amax(np.abs(e_ref-e_u))}'
        assert np.allclose( e_ref, e_u ), msg
    
        

