# -*- coding:Utf-8 -*-

import os, sys
from pathlib import Path

import numpy as np

from pylmgc90 import pre, chipy

def generate(dim, nb, gap, r, path):
    """
    Generate disk/spheres with point contactor
    at the center on a regular grid and.

    Params
    ------

    dim (int)   : space dimension
    nb  (float) : number of particles along each axis
    gap (float) : space between particles
    r   (float) : radius of particles
    path (Path) : path object where to write DATBOX
    """

    elem  = 'Rxx'+str(dim)+'D'
    rbdyx = 'RBDY'+str(dim)
    ptxdx = 'PT'+str(dim)+'Dx'
    if dim == 2:
      part = 'DISKx'
    else:
      part = 'SPHER'

    bodies = pre.avatars()
    mats  = pre.materials()
    mods  = pre.models()
    svs   = pre.see_tables()
    tacts = pre.tact_behavs()

    tdur= pre.material(name='TDURx',materialType='RIGID',density=1000.)
    mats.addMaterial(tdur)

    # create a model of rigid
    mod = pre.model(name='rigid', physics='MECAx', element=elem, dimension=dim)
    mods.addModel(mod)

    # create particles with points
    nb_part = nb**dim
    radii = [r]*nb_part

    if dim == 2:
      coor    = pre.squareLattice2D(nb, nb, 2.*r+gap)
      addBody = pre.rigidDisk
    else:
      coor    = pre.cubicLattice3D(nb, nb, nb, 2.*r+gap)
      addBody = pre.rigidSphere

    for c in coor:

        body  = addBody( r=r, center=c, material=tdur,
                         model=mod, color='BLUEx')
        body.addContactors(ptxdx,'BLUEx')
        bodies.addAvatar(body)

    #inter laws
    bl_part = pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.1)
    tacts += bl_part
    bl_ptxd = pre.tact_behav(name='vgtr0', law='ELASTIC_ROD',
                             stiffness=1.e8, prestrain=0.0)
    tacts += bl_ptxd

    alert_part = 1.e-2 * r
    alert_ptxd = 2.01*r+gap

    st_part = pre.see_table(CorpsCandidat   =rbdyx, candidat   =part, colorCandidat   ='BLUEx',
                            CorpsAntagoniste=rbdyx, antagoniste=part, colorAntagoniste='BLUEx',
                            behav=bl_part, alert=alert_part)
    svs += st_part
    st_ptxd = pre.see_table(CorpsCandidat   =rbdyx, candidat   =ptxdx, colorCandidat   ='BLUEx',
                            CorpsAntagoniste=rbdyx, antagoniste=ptxdx, colorAntagoniste='BLUEx',
                            behav=bl_ptxd, alert=alert_ptxd)
    svs += st_ptxd

    #ecriture fichiers datbox
    datbox = path/'DATBOX'
    datbox.mkdir(parents=True, exist_ok=True)

    post = pre.postpro_commands()

    pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post, gravy=[0., 0., 0.], datbox_path=datbox)

    #visualisation
    if 'with-visu' in sys.argv:
      pre.visuAvatars(bodies)


def compute_init(dim, dt, theta, r, path):

    chipy.overall_SetWorkingDirectory(str(path))
    chipy.checkDirectories()

    chipy.utilities_DisableLogMes()

    ### computation's parameters definition ###

    if dim == 3:
      chipy.nlgs_3D_DiagonalResolution()

    chipy.SetDimension(dim)
    chipy.Initialize()

    chipy.utilities_logMes('INIT TIME STEPPING')
    chipy.TimeEvolution_SetTimeStep(dt)
    chipy.Integrator_InitTheta(theta)

    if dim == 3:
      chipy.RBDY3_NewRotationScheme()
      chipy.PT3Dx_SetDisplayRadius(r)
    else:
      chipy.PT2Dx_SetDisplayRadius(r)

    ### model reading ###
    chipy.ReadDatbox(deformable=False)

    chipy.OpenDisplayFiles()
    chipy.InitHDF5('lmgc90.h5')

    ### compute masses ###
    chipy.ComputeMass()


def compute_one_step(stype, norm, tol, relax, gs_it1, gs_it2, freq_write, freq_display, recup=True):

        chipy.IncrementStep()
        #
        chipy.ComputeFext()
        chipy.ComputeBulk()
        chipy.ComputeFreeVelocity()
        #
        chipy.SelectProxTactors()

        if recup :
          chipy.RecupRloc()
        chipy.ExSolver(stype, norm, tol, relax, gs_it1, gs_it2)
        chipy.StockRloc()
        #
        chipy.ComputeDof()
        chipy.UpdateStep()

        chipy.WriteOut(freq_write)

        # False is important because it tests the interaction display file
        # writing configuration !
        chipy.WriteDisplayFiles(freq_display)

def compute_finalize():

    chipy.WriteLastDof()
    chipy.WriteLastVlocRloc()
    chipy.CloseDisplayFiles()
    chipy.Finalize()

if __name__ == "__main__":

    assert( len(sys.argv) == 2 ),  'test script needs a dimension as input argument'
    assert( sys.argv[1]=='2' or sys.argv[1]=='3' ), 'can only test dimension 2 or 3'

    dim = int(sys.argv[1])
    path = Path(f"{dim}D")

    nb  = 2
    r   = 0.1
    gap = 0.05

    dt       = 0.1
    theta    = 0.5

    compute_params = {'freq_write'  : 1      ,
                      'freq_display': 1      ,
                      'tol'         : 1.e-4  ,
                      'relax'       : 1.0    ,
                      'norm'        : 'Quad ',
                      'gs_it1'      : 50     ,
                      'gs_it2'      : 500    ,
                      'stype'       : 'Stored_Delassus_Loops         ',
                      'recup'       : True
                     }
    generate(dim, nb, gap, r, path)

    compute_init(dim, dt, theta, r, path)

    # use normal detection
    compute_one_step(**compute_params)

    # check the exact number of interaction
    # (that is number of edges of a regular grid)
    # and that Rn is 0. (no prestrain):
    nx = nb
    ny = nb
    if dim == 3:
      nz = nb
    else :
      nz = 1
    nb_inter = ( nx*(ny-1) + (ny-1)*nx ) * nz + ( nx*ny * (nz-1) )

    if dim == 3:
      allInterGetter = 'chipy.inter_handler_3D_getAll( chipy.PTPT3_ID )'
      interIdBodies  = 'chipy.inter_handler_3D_tgetIdBodies( chipy.PTPT3_ID, 1 )'
      rn_index = 13
      loadNetwork       = chipy.PTPT3_LoadNetwork
      selectProxTactors = chipy.PTPT3_SelectProxTactors
      useNonuc0         = chipy.PTPT3_UseCurrentNonuc0
    else:
      allInterGetter = 'chipy.inter_handler_2D_getAll( chipy.PTPT2_ID )'
      interIdBodies  = 'chipy.inter_handler_2D_tgetIdBodies( chipy.PTPT2_ID, 1 )'
      rn_index = 7
      loadNetwork       = chipy.PTPT2_LoadNetwork
      selectProxTactors = chipy.PTPT2_SelectProxTactors
      useNonuc0         = chipy.PTPT2_UseCurrentNonuc0

    allInter = eval(allInterGetter)
    assert( allInter.shape[0] == nb_inter )
    assert( np.all( allInter[:,rn_index] == 0. ) )


    # generate a new network with only one interaction
    # and a reference distance shorter than 0 detection
    cd_id = 1
    if dim == 3:
      an_id = nx*(ny+1)+2
      with open(path/'DATBOX/PTPT3_NETWORK.DAT','w') as f:
        # careful, file format change depending of UseParam or not
        # in our case input must be : icdtac, iantac, xper, yper, iloop, nonuc0
        line = '{:05d} {:05d} 0 0 0 {:f}'.format( cd_id, an_id, gap )
        f.write(line)
    else:
      an_id = nx+2
      with open(path/'DATBOX/PTPT2_NETWORK.DAT','w') as f:
        # careful, file format change depending of UseParam or not
        # in our case input must be : isee, icdtac, iantac, nonuc0
        line = '{:05d} {:05d} {:05d} {:f}'.format( 2, cd_id, an_id, gap )
        f.write(line)

    # activate network file loading
    # and compute a new step
    # ... should not change anything
    # since rough detection is already done
    loadNetwork()
    compute_one_step(**compute_params)

    # check
    allInter = eval(allInterGetter)
    assert( allInter.shape[0] == nb_inter )

    # reset PTPT detection and run a new step
    # then check that network file is taken into account
    selectProxTactors(1)
    compute_one_step(**compute_params)

    # checks
    allInter = eval(allInterGetter)
    assert( allInter.shape[0] == 1 )

    idBodies = eval( interIdBodies )
    assert( idBodies[0]==cd_id and idBodies[1]==an_id )


    # reset PTPT detection and check
    # that network file is taken into account
    # including the nonuc0 part, but since
    # the contact already exist, RecupRloc
    # must be skipped to not recup the internal value !
    useNonuc0(1)
    selectProxTactors(1)
    compute_params['recup']=False
    compute_one_step(**compute_params)
    allInter = eval(allInterGetter)
    assert( allInter[0,rn_index] < 0. )

    compute_finalize()

