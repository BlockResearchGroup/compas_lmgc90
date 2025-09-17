
import sys
import itertools
from pathlib import Path

import numpy as np
import h5py

from pylmgc90 import pre, chipy

try :
    from matplotlib import pyplot as plt
    WITH_PLOT = True
except:
    WITH_PLOT = False


STIFFNESS = 1.e8

def generate(dim=2, variant='', path='.'):
    global STIFFNESS

    # ecriture des fichiers
    datbox = Path(path)
    datbox = datbox/'DATBOX'
    datbox.mkdir(exist_ok=True, parents=True)
    
    # size
    r = 1.2
    alert = 1e-2

    # law params:
    law_params = { 'name'      : 'elasR'  , 'law'  : 'ELASTIC_REPELL_CLB',
                   'stiffness' : STIFFNESS, 'fric' : 0.3                 ,
                 }
    if variant:
      law_params['law'] += '_'+variant

    bodies = pre.avatars()
    mats   = pre.materials()
    mods   = pre.models()
    sees   = pre.see_tables()
    tacts  = pre.tact_behavs()
    posts  = pre.postpro_commands()
    
    steel = pre.material(name='Steel', materialType='RIGID', density=78000.)
    mats.addMaterial(steel)

    mod = pre.model(name='rigid', physics='MECAx', element=f'Rxx{dim}D', dimension=dim)

    mods.addModel(mod)

    if dim == 2:
      s1 = pre.rigidDisk(model=mod, material=steel, r=r, center=[0., 0], color='DISKx')
      s2 = pre.rigidDisk(model=mod, material=steel, r=r, center=[0., 0], color='DISKx')

      s2.translate(dy=2*r)
      components = [1,2,3]
      gravy = [0., -9.81, 0.]
    else:
      s1 = pre.rigidSphere(model=mod, material=steel, r=r, center=[0., 0, 0], color='SPHER')
      s2 = pre.rigidSphere(model=mod, material=steel, r=r, center=[0., 0, 0], color='SPHER')

      s2.translate(dz=2*r)
      components = [1,2,3,4,5,6]
      gravy = [0., 0, -9.81]


    s1.imposeDrivenDof(component=components, dofty='vlocy')

    bodies += s1
    bodies += s2

    tlaw   = pre.tact_behav( **law_params )
    tacts += tlaw

    body = f"RBDY{dim}"
    tact = 'DISKx' if dim == 2 else 'SPHER'
    see = pre.see_table(CorpsCandidat   =body, candidat   =tact, colorCandidat   =tact,
                        CorpsAntagoniste=body, antagoniste=tact, colorAntagoniste=tact,
                        behav=tlaw, alert=alert)
    
    sees += see
    posts.addCommand(pre.postpro_command(name = 'SOLVER INFORMATIONS', step = 1 )            )
      
    pre.writeDatbox(dim, mats, mods, bodies, tacts, sees, gravy=gravy, post=posts, datbox_path=datbox)

    try:
      pre.visuAvatars(bodies)
    except:
      pass


def compute(dim, path):

    chipy.Initialize()

    chipy.overall_SetWorkingDirectory( path )
    chipy.checkDirectories()

    # logMes
    #chipy.utilities_DisableLogMes()

    # modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
    mhyp = 0 if dim == 3 else 1

    # time evolution parameters
    dt = 1e-3
    nbsteps = 10#0

    # theta integrator parameter
    theta = 0.5
    
    # nlgs parameters
    tol    = 1e-5
    relax  = 1.0
    norm   = 'Quad '
    gs_it1 = 1
    gs_it2 = 2
    stype  = 'Stored_Delassus_Loops         '
    
    # write parameter
    hfile     = 'lmgc90.h5'
    f_write   = 1
    
    # display parameters
    f_display = 1

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
    chipy.ReadDatbox(False)

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
    
    if str(path).endswith('adapt'):
      if dim == 2 :
        chipy.nlgs_IsInitialized()
      else :
        chipy.nlgs_3D_IsInitialized()

    for i_step in range(nbsteps):
        chipy.IncrementStep()
        chipy.ComputeFext()
        chipy.ComputeBulk()
        chipy.AssembleMechanicalRHS()
        chipy.ComputeFreeVelocity()
        chipy.SelectProxTactors()
        chipy.RecupRloc()

        if str(path).endswith('adapt'):
          new_stiff = 0.5*STIFFNESS
          if dim == 2 :
            chipy.inter_handler_2D_tsetInternal( chipy.DKDKx_ID, 1, 1, new_stiff)
          else :
            chipy.inter_handler_3D_tsetInternal( chipy.SPSPx_ID, 1, 1, new_stiff)

        chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
        chipy.UpdateTactBehav()
        chipy.StockRloc()
        chipy.ComputeDof()
        chipy.UpdateStep()

        chipy.WriteOut(f_write)
        if f_display > 0:
            chipy.WriteDisplayFiles(f_display)

        chipy.WritePostproFiles()
        chipy.checkInteractiveCommand()

    ### end while time loop ###
    
    chipy.ClosePostproFiles()
    chipy.Finalize()


def post_plot(dim=2, variant='', path='.'):

    global STIFFNESS

    wd = Path(path)


    # from hdf5 of inside computation ?
    with h5py.File( wd/'lmgc90.h5', 'r' ) as hf:
        nb_record = int( hf['Simulation/nb_record'][()] )
        gap_idx  = hf['Help/VlocRloc/rdata/gapTT/bound'][0] - 1 #Fortran to C
        rlb_idx = hf['Help/VlocRloc/rdata/rl/bound'][0] - 1     #Fortran to C
        rle_idx = hf['Help/VlocRloc/rdata/rl/bound'][1]         #end excluded

        g_r = np.zeros( [2,nb_record], dtype=float )
        # should I assert that nb_inters == 1 ?
        for i in range(nb_record):
          if 'VlocRloc' not in hf[f"Evolution/ID_{i+1}"].keys() :
            break
          g_r[0, i] = hf[f"Evolution/ID_{i+1}/VlocRloc/rdata"][0,gap_idx]
          g_r[1, i] = np.linalg.norm(hf[f"Evolution/ID_{i+1}/VlocRloc/rdata"][0,rlb_idx:rle_idx])

    if WITH_PLOT:
        fig, ax_F = plt.subplots(1, 1)
        fig.suptitle(path)

        ax_F.set_title( f"" )
        ax_F.set_xlabel(f"$gap$" )
        ax_F.set_ylabel(f"$R$" )
        ax_F.plot( g_r[0], g_r[1], 'b')

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

    r_ref = -STIFFNESS* g_r[0,:]
    if str(path).endswith('adapt'):
      r_ref *= 0.5
    msg = f'[ERROR] differences in computed values of {wd} example with value {np.amax(np.abs(r_ref-g_r[1,:]))}'
    assert np.allclose( r_ref, g_r[1,:], atol=1e-8 ), msg

