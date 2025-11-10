from pathlib import Path
import copy

import numpy as np

np_version = tuple( int(n) for n in np.__version__.split(".") )
if np_version < (1, 16) :
    def s2u( array, dtype ):
        nbf = len(array.dtype)
        out = np.empty( [array.size, nbf], dtype)
        for i, n in enumerate( array.dtype.names ):
            out[:,i] = array[n]
        return out
else :
    from numpy.lib.recfunctions import structured_to_unstructured as s2u

from pylmgc90 import pre


wd = Path('2D')
datbox = wd/'DATBOX'
datbox.mkdir(parents=True, exist_ok=True)

# containers
bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
tacts  = pre.tact_behavs()
sees   = pre.see_tables()
post   = pre.postpro_commands()

dim = 2

# models
modr = pre.model(name='M2D_R', physics='MECAx', element='Rxx2D', dimension=dim)
modl = pre.model(name='M2D_L', physics='MECAx', element='T3xxx', dimension=dim,
                 external_model='MatL_', kinematic='small', material='elas_',
                 anisotropy='iso__', mass_storage='lump_')
mods.addModel(modr, modl)

# materials
steel = pre.material(name='steel', materialType='ELAS', elas='standard',
                     anisotropy='isotropic', density=7850.,
                     young=2.05e11, nu=0.3)  
pdur  = pre.material(name='pdurx', materialType='RIGID', density=1.)
mats.addMaterial(steel, pdur)

# standard block mesh of T3
mesh_block = pre.readMesh('gmsh/2d.msh',dim)

# generating left and right base blocks
baseL = pre.buildMeshedAvatar(mesh_block, modl, steel)
baseL.imposeDrivenDof(group='bottom left' , component=[1, 2], dofty='vlocy')
baseL.imposeDrivenDof(group='bottom right', component=[1, 2], dofty='vlocy')

baseL.addContactors(group='upper left' , shape='ALpxx', color='BASEx', reverse=True)
baseL.addContactors(group='upper right', shape='ALpxx', color='BASEx', reverse=True)

baseR = copy.deepcopy( baseL )
baseR.translate(dx=2.)

bodies += baseL
bodies += baseR

# generating 3 brick blocks
brick1 = pre.buildMeshedAvatar(mesh_block, modl, steel)

# reducing size
ratio = 0.5
for n in brick1.nodes:
  n.coor *= ratio

brick1.addContactors(group='bottom left' , shape='CLxxx', color='BRICK', reverse=True)#, weights=[0.1,0.9])
brick1.addContactors(group='bottom right', shape='CLxxx', color='BRICK', reverse=True)#, weights=[0.1,0.9])
brick1.addContactors(group='upper left'  , shape='ALpxx', color='BRICK', reverse=True)
brick1.addContactors(group='upper right' , shape='ALpxx', color='BRICK', reverse=True)

brick1.translate(dx=0.7, dy=1.)

brick2 = copy.deepcopy(brick1)
brick2.translate(dx=1.)

brick3 = copy.deepcopy(brick2)
brick3.translate(dx=1.)

bodies += brick1
bodies += brick2
bodies += brick3

# add cluster of disk and polygon
r = 0.1
vertices = np.array( [[0.,0.],[2*r,0.],[2*r,2*r],[0.,2*r]] )
d1 = pre.rigidPolygon(center=[0., 0.], nb_vertices=4, vertices=vertices,
                      generation_type='full', model=modr, material=pdur, color='JUNKx')
d1.addContactors(shape='DISKx', byrd=r, shift=[1.5*r,0.], color='JUNKx')
d1.computeRigidProperties()
d1.translate(dx=1.7-r,dy=1.5)

d2 = copy.deepcopy(d1)
d2.rotate(center=d2.getNodeCoor(), psi=np.pi)
d2.translate(dx=1.)

bodies += d1
bodies += d2
#pre.visuAvatars(bodies)

gapc0 = pre.tact_behav(name='gapc0', law='GAP_SGR_CLB', fric=0.5)
gapc1 = pre.tact_behav(name='gapc1', law='GAP_SGR_CLB', fric=0.1)
tacts.addBehav(gapc0)
tacts.addBehav(gapc1)

see1 = pre.see_table(CorpsCandidat   ='MAILx', candidat   ='CLxxx', colorCandidat   ='BRICK',
                     CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='BASEx',
                     alert=0.01, halo=0.25, behav=gapc0)
see2 = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='JUNKx',
                     CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='BRICK',
                     alert=0.01, halo=0.25, behav=gapc1)
see3 = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='POLYG', colorCandidat   ='JUNKx',
                     CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='BRICK',
                     alert=0.01, halo=0.25, behav=gapc1)
sees.addSeeTable(see1)
sees.addSeeTable(see2)
sees.addSeeTable(see3)

pre.writeDatbox(dim, mats, mods, bodies, tacts, sees, post=post, datbox_path=datbox)



from pylmgc90 import chipy
from pylmgc90.chipy import computation

dt    = 1e-3
theta = 0.501
mhyp  = 1

chipy.overall_SetWorkingDirectory(str(wd))
computation.initialize(dim, dt, theta, mhyp, deformable=True, logmes=True)
chipy.CLALp_Trim()

stype = 'Stored_Delassus_Loops         '
norm  = 'Quad '
tol   = 1e-4
relax = 1.0
gs_it1= 1000
gs_it2= 50

f_write = 1
f_disp  = 1

chipy.WriteDisplayFiles(1)
# compute a single step
computation.one_step(stype, norm, tol, relax, gs_it1, gs_it2,
                     f_write, f_disp)

# change gravity to make things slide
new_g = -9.81 * np.sqrt(2.)
chipy.bulk_behav_SetGravity([new_g, new_g, 0.])

inters_new = chipy.getInteractions(this=True)

# columns indexing
iid = ['cdbdy', 'anbdy', 'icdbdy', 'ianbdy', 'cdtac', 'antac', 'icdtac', 'iantac', 'icdsci', 'iansci']
lid = ['icdbdy', 'ianbdy', 'icdtac', 'iantac', 'icdsci', 'iansci']

# initial state comparison
ref_idata = np.array([(b'MAILx', b'MAILx', 3, 1, b'CLxxx', b'ALpxx',  1,  1, 0, 4),
                      (b'MAILx', b'MAILx', 3, 1, b'CLxxx', b'ALpxx',  2,  1, 0, 3),
                      (b'MAILx', b'MAILx', 3, 1, b'CLxxx', b'ALpxx',  3,  1, 0, 4),
                      (b'MAILx', b'MAILx', 3, 1, b'CLxxx', b'ALpxx',  4,  2, 0, 1),
                      (b'MAILx', b'MAILx', 3, 1, b'CLxxx', b'ALpxx',  5,  2, 0, 1),
                      (b'MAILx', b'MAILx', 3, 1, b'CLxxx', b'ALpxx',  6,  2, 0, 2),
                      (b'MAILx', b'MAILx', 3, 1, b'CLxxx', b'ALpxx',  7,  2, 0, 1),
                      (b'MAILx', b'MAILx', 3, 1, b'CLxxx', b'ALpxx',  8,  2, 0, 2),
                      (b'MAILx', b'MAILx', 3, 1, b'CLxxx', b'ALpxx',  9,  2, 0, 3),
                      (b'MAILx', b'MAILx', 3, 1, b'CLxxx', b'ALpxx', 10,  2, 0, 3),
                      (b'MAILx', b'MAILx', 4, 1, b'CLxxx', b'ALpxx',  1,  2, 0, 4),
                      (b'MAILx', b'MAILx', 4, 1, b'CLxxx', b'ALpxx',  2,  2, 0, 3),
                      (b'MAILx', b'MAILx', 4, 1, b'CLxxx', b'ALpxx',  3,  2, 0, 4),
                      (b'MAILx', b'MAILx', 4, 2, b'CLxxx', b'ALpxx',  4,  1, 0, 1),
                      (b'MAILx', b'MAILx', 4, 2, b'CLxxx', b'ALpxx',  5,  1, 0, 1),
                      (b'MAILx', b'MAILx', 4, 2, b'CLxxx', b'ALpxx',  6,  1, 0, 2),
                      (b'MAILx', b'MAILx', 4, 2, b'CLxxx', b'ALpxx',  7,  1, 0, 1),
                      (b'MAILx', b'MAILx', 4, 2, b'CLxxx', b'ALpxx',  8,  1, 0, 2),
                      (b'MAILx', b'MAILx', 4, 2, b'CLxxx', b'ALpxx',  9,  1, 0, 3),
                      (b'MAILx', b'MAILx', 4, 2, b'CLxxx', b'ALpxx', 10,  1, 0, 3),
                      (b'MAILx', b'MAILx', 5, 2, b'CLxxx', b'ALpxx',  1,  1, 0, 4),
                      (b'MAILx', b'MAILx', 5, 2, b'CLxxx', b'ALpxx',  2,  1, 0, 3),
                      (b'MAILx', b'MAILx', 5, 2, b'CLxxx', b'ALpxx',  3,  1, 0, 4),
                      (b'MAILx', b'MAILx', 5, 2, b'CLxxx', b'ALpxx',  4,  2, 0, 1),
                      (b'MAILx', b'MAILx', 5, 2, b'CLxxx', b'ALpxx',  5,  2, 0, 1),
                      (b'MAILx', b'MAILx', 5, 2, b'CLxxx', b'ALpxx',  6,  2, 0, 2),
                      (b'MAILx', b'MAILx', 5, 2, b'CLxxx', b'ALpxx',  7,  2, 0, 1),
                      (b'MAILx', b'MAILx', 5, 2, b'CLxxx', b'ALpxx',  8,  2, 0, 2),
                      (b'MAILx', b'MAILx', 5, 2, b'CLxxx', b'ALpxx',  9,  2, 0, 3),
                      (b'MAILx', b'MAILx', 5, 2, b'CLxxx', b'ALpxx', 10,  2, 0, 3),
                      (b'RBDY2', b'MAILx', 1, 4, b'DISKx', b'ALpxx',  2, 11, 0, 1),
                      (b'RBDY2', b'MAILx', 1, 4, b'DISKx', b'ALpxx',  2, 11, 0, 2),
                      (b'RBDY2', b'MAILx', 2, 4, b'DISKx', b'ALpxx',  2, 12, 0, 3),
                      (b'RBDY2', b'MAILx', 2, 4, b'DISKx', b'ALpxx',  2, 12, 0, 4),
                      (b'RBDY2', b'MAILx', 1, 3, b'POLYG', b'ALpxx',  1, 12, 1, 4),
                      (b'RBDY2', b'MAILx', 1, 4, b'POLYG', b'ALpxx',  1, 11, 2, 1),
                      (b'RBDY2', b'MAILx', 2, 4, b'POLYG', b'ALpxx',  1, 12, 3, 4),
                      (b'RBDY2', b'MAILx', 2, 5, b'POLYG', b'ALpxx',  1, 11, 4, 1),
                     ], dtype=inters_new[iid].dtype )

assert np.all( inters_new[iid] == ref_idata ), "error when comparing idata to ref"

# difference when clalp change of cd/an contactor
diff_clal_1 = np.array([[ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0, -1,  0,  3],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0, -1,  0,  1,  0,  3],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0, -1,  0,  3],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0,  0,  0, -1],
                       ], dtype=int)
diff_clal_2 = np.array([[ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0, -1,  0,  3],
                        [ 0,  0,  0, -1,  0,  3],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0, -1,  0,  1,  0,  3],
                        [ 0, -1,  0,  1,  0,  3],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0, -1,  0,  3],
                        [ 0,  0,  0, -1,  0,  3],
                        [ 0,  0,  0,  0,  0, -1],
                        [ 0,  0,  0,  0,  0, -1],
                       ], dtype=int)


# line index of inters_old/new changing
id_plal   = iter( [34] )
diff_plal = np.array([ 0, -1,  0, 1,  0, 3], dtype=int)

# line index of inters_new to remove to compare with inters_old
remove_dkal = iter( [33, 36, 31, 36, 33] )
# line index of inters_old to remove to compare with inters_new
adding_dkal = iter( [32, 30,  ] )


# running the computation with oriented gravity
for i in range(2,251):
    computation.one_step(stype, norm, tol, relax, gs_it1, gs_it2,
                         f_write, f_disp)

    # reading inters_new from hdf5 file (verlet interactions data)
    inters_old = inters_new
    inters_new = chipy.getInteractions(this=True)

    # counting number of recuped contacts
    nb_recup = [ chipy.inter_handler_2D_getNbRecup(ctc_id) for ctc_id in [chipy.DKALp_ID, chipy.PLALp_ID, chipy.CLALp_ID] ]

    # when CL change of ALp
    if   i == 242:
        # to generate this:
        # tt1 = s2u( inters_old[lid], dtype=int )
        # tt2 = s2u( inters_new[lid], dtype=int )
        # np.where( np.any( tt2-tt1 != 0, axis=1 ) )
        ids = [ 1,  2,  4,  6,  7,  9, 11, 12, 14, 16, 17, 19, 21, 22, 24, 26, 27, 29 ]
        t1 = s2u( inters_old[lid][ids], dtype=int )
        t2 = s2u( inters_new[lid][ids], dtype=int )
        assert np.all( (t2-t1) == diff_clal_2 ), "error when comparing idata at step {}".format(i)
        assert( sum(nb_recup)+6 == inters_new.size ), "wrong number of recup at step {}".format(i)
    # when PL changes of ALp
    elif i in [192]:
        ids = next(id_plal)
        t1 = s2u( inters_old[lid][ids], dtype=int )
        t2 = s2u( inters_new[lid][ids], dtype=int )
        assert np.all( (t2-t1) == diff_plal ), "error when comparing idata at step {}".format(i)
        assert( sum(nb_recup)+1 == inters_new.size ), "wrong number of recup at step {}".format(i)
    # when CL changes of ALp
    elif i == 149:
        ids = [0, 3, 5, 8, 10, 13, 15, 18, 20, 23, 25, 28]
        t1 = s2u( inters_old[lid][ids], dtype=int )
        t2 = s2u( inters_new[lid][ids], dtype=int )
        assert np.all( (t2-t1) == diff_clal_1 ), "error when comparing idata at step {}".format(i)
        assert( sum(nb_recup)+3 == inters_new.size ), "wrong number of recup at step {}".format(i)
    # when DK see new ALp
    elif i in [143, 196]:
        ids = next(adding_dkal)
        assert np.all( inters_old[iid] == np.delete(inters_new[iid],ids ) ), "error when comparing idata at step {}".format(i)
        assert( sum(nb_recup) == inters_old.size ), "wrong number of recup at step {}".format(i)
    # when some CL change of subcontactor of ALp... no check on subcontactor change :s
    elif i in [97, 106, 234]:
        assert np.all( inters_old[iid[:-2]] == inters_new[iid[:-2]] ), "error when comparing idata at step {}".format(i)
        assert( sum(nb_recup) == inters_new.size ), "wrong number of recup at step {}".format(i)
    # when DK/PL lost some ALp
    elif i in [90, 102, 162, 201, 238]:
        ids = next(remove_dkal)
        assert np.all( np.delete(inters_old[iid],ids ) == inters_new[iid] ), "error when comparing idata at step {}".format(i)
        assert( sum(nb_recup) == inters_new.size ), "wrong number of recup at step {}".format(i)
    else:
        assert np.all( inters_old[iid] == inters_new[iid] ), "error when comparing idata at step {}".format(i)
        assert( sum(nb_recup) == inters_new.size ), "wrong number of recup at step {}".format(i)

computation.finalize()
