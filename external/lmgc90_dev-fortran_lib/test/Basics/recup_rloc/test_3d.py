from pathlib import Path
import copy
import itertools

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

wd = Path('3D')
datbox = wd/'DATBOX'
datbox.mkdir(parents=True, exist_ok=True)

# containers
bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
tacts  = pre.tact_behavs()
sees   = pre.see_tables()
post   = pre.postpro_commands()

dim = 3

# models
modr = pre.model(name='M3D_R', physics='MECAx', element='Rxx3D', dimension=dim)
modT = pre.model(name='M3D_T', physics='MECAx', element='TE4xx', dimension=dim,
                 external_model='MatL_', kinematic='small', material='elas_',
                 anisotropy='iso__', mass_storage='lump_')
modH = pre.model(name='M3D_H', physics='MECAx', element='H8xxx', dimension=dim,
                 external_model='MatL_', kinematic='small', material='elas_',
                 anisotropy='iso__', mass_storage='lump_')
mods.addModel(modr, modT,  modH)

# materials
steel = pre.material(name='steel', materialType='ELAS', elas='standard',
                     anisotropy='isotropic', density=7850.,
                     young=2.05e11, nu=0.3)  
pdur  = pre.material(name='pdurx', materialType='RIGID', density=1.)
mats.addMaterial(steel, pdur)

# standard block mesh of TE4
mesh_T4 = pre.readMesh('gmsh/3d.msh',dim)
# standard block mesh of H8
mesh_H8 = pre.buildMeshH8(0.,2.,0.,2.,2.,1.0, 4, 4, 2)

# base left
baseL = pre.buildMeshedAvatar(mesh=mesh_T4, model=modT, material=steel)
baseL.addContactors(group='upper rear'  , shape='ASpxx', color='BASEx')
baseL.addContactors(group='upper front' , shape='ASpxx', color='BASEx')
baseL.addContactors(group='bottom rear' , shape='CSpxx', color='BASEx', quadrature=1, reverse=True)
baseL.addContactors(group='bottom front', shape='CSpxx', color='BASEx', quadrature=1, reverse=True)

# base right
baseR = pre.buildMeshedAvatar(mesh=mesh_H8, model=modH, material=steel)
baseR.addContactors(group='up'  , shape='ASpxx', color='BASEx')
baseR.addContactors(group='down', shape='CSpxx', color='BASEx', quadrature=1)

baseL.imposeDrivenDof(group='bottom rear' , component=[1, 2, 3], dofty='vlocy')
baseL.imposeDrivenDof(group='bottom front', component=[1, 2, 3], dofty='vlocy')
baseR.imposeDrivenDof(group='down'        , component=[1, 2, 3], dofty='vlocy')

bodies += baseL
bodies += baseR

# generating 2 brick blocks
brick1 = pre.buildMeshedAvatar(mesh_T4, modT, steel)
brick2 = pre.buildMeshedAvatar(mesh_H8, modH, steel)

# reducing size
ratio = 0.5
for n in itertools.chain(brick1.nodes, brick2.nodes):
  n.coor *= ratio

brick1.addContactors(group='upper rear'  , shape='ASpxx', color='BASEx')
brick1.addContactors(group='upper front' , shape='ASpxx', color='BASEx')
brick1.addContactors(group='bottom rear' , shape='CSpxx', color='BRICK', reverse=True)
brick1.addContactors(group='bottom front', shape='CSpxx', color='BRICK', quadrature=0, reverse=True)

brick1.translate(dx=0.48, dy=0.95, dz=1.)

brick2.addContactors(group='up'  , shape='ASpxx', color='BASEx')
brick2.addContactors(group='down', shape='CSpxx', color='BRICK', quadrature=1)

brick2.translate(dx=0.48, dy=0.95, dz=1.)

bodies += brick1
bodies += brick2

# add cluster of disk and polygon
r = 0.125
d1 = pre.rigidPolyhedron(modr, pdur, radius=r, nb_vertices=8, color='JUNKx')
#d1.addContactors(shape='SPHER', byrd=r*0.8, color='JUNKx')
#d1.computeRigidProperties()
d1.translate(dx=1.-r*2.1,dy=2.-r*1.1, dz=1.5+r*0.62)

d2 = copy.deepcopy(d1)
d2.translate(dx=2*r*0.62)
#
bodies += d1
bodies += d2
#pre.visuAvatars(bodies, True)

gapc0 = pre.tact_behav(name='gapc0', law='GAP_SGR_CLB', fric=0.5)
gapc1 = pre.tact_behav(name='gapc1', law='GAP_SGR_CLB', fric=0.1)
tacts.addBehav(gapc0)
tacts.addBehav(gapc1)

see1 = pre.see_table(CorpsCandidat   ='MAILx', candidat   ='CSxxx', colorCandidat   ='BRICK',
                     CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='BASEx',
                     alert=0.25, halo=0.5, behav=gapc0)
see2 = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='POLYR', colorCandidat   ='JUNKx',
                     CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='BASEx',
                     alert=0.01, halo=0.25, behav=gapc1)
sees.addSeeTable(see1)
sees.addSeeTable(see2)

pre.writeDatbox(dim, mats, mods, bodies, tacts, sees, post=post, datbox_path=datbox)



from pylmgc90 import chipy
from pylmgc90.chipy import computation

dt    = 1e-3
theta = 0.501
mhyp  = 0

chipy.overall_SetWorkingDirectory(str(wd))
computation.initialize(dim, dt, theta, mhyp, deformable=True, logmes=True)
chipy.CSASp_Trim()

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
new_g = 9.81 / np.sqrt(3.)
chipy.bulk_behav_SetGravity([new_g, new_g, -new_g])

#rdata = chipy.inter_handler_2D_getAll(chipy.CLALp_ID)
#idata = chipy.inter_handler_2D_getAllIdata(chipy.CLALp_ID)

inters_new = chipy.getInteractions(this=True)

# columns indexing
iid = ['cdbdy', 'anbdy', 'icdbdy', 'ianbdy', 'cdtac', 'antac', 'icdtac', 'iantac', 'icdsci', 'iansci']
lid = ['icdbdy', 'ianbdy', 'icdtac', 'iantac', 'icdsci', 'iansci']

# initial state comparison
ref_idata = np.array([(b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 113, 1,  1,  4),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 114, 1,  2,  4),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 115, 1,  3,  1),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 116, 1,  4,  1),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 117, 1,  5,  5),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 118, 1,  6,  4),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 119, 1,  7,  6),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 120, 1,  8,  2),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 121, 2,  1,  3),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 122, 2,  2,  3),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 123, 2,  3,  3),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 124, 2,  4,  5),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 125, 2,  5,  5),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 126, 2,  6,  1),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 127, 2,  7,  3),
                      (b'MAILx', b'MAILx', 3, 1, b'CSxxx', b'ASpxx', 128, 2,  8,  1),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 129, 1,  1, 19),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 130, 1,  2, 20),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 131, 1,  3, 19),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 132, 1,  4, 19),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 133, 1,  5, 20),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 134, 1,  6, 20),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 135, 1,  7, 20),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 136, 1,  8, 20),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 137, 1,  9, 21),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 138, 1, 10, 22),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 139, 1, 11, 21),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 140, 1, 12, 21),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 141, 1, 13, 22),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 142, 1, 14, 22),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 143, 1, 15, 22),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 144, 1, 16, 22),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 145, 1, 17, 19),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 146, 1, 18, 19),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 147, 1, 19, 19),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 148, 1, 20, 19),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 149, 1, 21, 19),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 150, 1, 22, 20),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 151, 1, 23, 19),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 152, 1, 24, 19),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 153, 1, 25, 21),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 154, 1, 26, 21),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 155, 1, 27, 21),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 156, 1, 28, 21),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 157, 1, 29, 21),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 158, 1, 30, 22),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 159, 1, 31, 21),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 160, 1, 32, 21),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 161, 1, 33, 27),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 162, 1, 34, 28),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 163, 1, 35, 27),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 164, 1, 36, 27),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 165, 1, 37, 28),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 166, 1, 38, 28),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 167, 1, 39, 28),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 168, 1, 40, 28),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 169, 1, 41, 29),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 170, 1, 42, 30),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 171, 1, 43, 29),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 172, 1, 44, 29),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 173, 1, 45, 30),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 174, 1, 46, 30),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 175, 1, 47, 30),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 176, 1, 48, 30),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 177, 1, 49, 27),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 178, 1, 50, 27),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 179, 1, 51, 27),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 180, 1, 52, 27),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 181, 1, 53, 27),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 182, 1, 54, 28),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 183, 1, 55, 27),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 184, 1, 56, 27),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 185, 1, 57, 29),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 186, 1, 58, 29),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 187, 1, 59, 29),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 188, 1, 60, 29),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 189, 1, 61, 29),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 190, 1, 62, 30),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 191, 1, 63, 29),
                      (b'MAILx', b'MAILx', 4, 2, b'CSxxx', b'ASpxx', 192, 1, 64, 29),
                     ], dtype=inters_new[iid].dtype )

#assert np.all( inters_new[iid] == ref_idata ), f"error when comparing idata to ref"

diff_ids = [ [2,3,5,6],
             [18, 19, 26, 27, 34, 35, 42, 43, 50, 51, 58, 59, 66, 67, 74, 75],
             [17, 19, 21, 23, 25, 27, 29, 31, 49, 51, 53, 55, 57, 59, 61, 63],
             [13],
           ]
diff_val = [ np.array([-3, -3, -2,  6], dtype=int),
              1,
              9,
             -1,
           ]
diff_csas = zip(diff_ids, diff_val)

for i in range(2,251):
    computation.one_step(stype, norm, tol, relax, gs_it1, gs_it2,
                         f_write, f_disp)

    # reading inters_new from hdf5 file (verlet interactions data)
    inters_old = inters_new
    inters_new = chipy.getInteractions(this=True)

    # counting number of recuped contacts
    nb_recup = [ chipy.inter_handler_3D_getNbRecup(ctc_id) for ctc_id in [chipy.CSASp_ID] ]

#    if   i == 167:
#        ids = [4,5,7]
#        assert np.all( np.delete(inters_old[iid], ids) == np.delete(inters_new[iid], ids) ), "error when comparing idata at step {}".format(i)
#        assert( sum(nb_recup) == inters_new.size-2 ), "wrong number of recup at step {}".format(i)
#    elif i in [140, 202, 239, 240]:
#        ids, diff = next(diff_csas)
#        assert np.all( inters_old['iansci'][ids]-inters_new['iansci'][ids] == diff ), "error when comparing CS change of subcontactor of ASp at step {}".format(i)
#        assert np.all( inters_old[iid[:-1]][ids] == inters_new[iid[:-1]][ids] ), "error when comparing CS change of subcontactor of ASp at step {}".format(i)
#        assert np.all( np.delete(inters_old[iid], ids) == np.delete(inters_new[iid], ids) ), "error when comparing idata at step {}".format(i)
#        assert( sum(nb_recup) == inters_new.size ), "wrong number of recup at step {}".format(i)
#    # when some CS change of ASp
#    elif i == 107:
#        ids = [1,5,6]
#        diff = np.array([[ 0,  0,  0, -1,  0,  1],
#                         [ 0,  0,  0, -1,  0,  1],
#                         [ 0,  0,  0, -1,  0, -1],
#                        ])
#        t1 = s2u( inters_old[lid][ids], dtype=int )
#        t2 = s2u( inters_new[lid][ids], dtype=int )
#        assert np.all( t1-t2 == diff ), "error when comparing CS change of ASp at step {}".format(i)
#        assert np.all( np.delete(inters_old[iid], ids) == np.delete(inters_new[iid], ids) ), "error when comparing idata at step {}".format(i)
#        assert( sum(nb_recup) == inters_new.size-len(ids) ), "wrong number of recup at step {}".format(i)
#    else:
#        assert np.all( inters_old[iid] == inters_new[iid] ), "error when comparing idata at step {}".format(i)
#        assert( sum(nb_recup) == inters_new.size ), "wrong number of recup at step {}".format(i)

computation.finalize()
