import pathlib
import copy

import numpy as np

from pylmgc90 import pre

datbox = pathlib.Path('./DATBOX')
datbox.mkdir(exist_ok=True)

dim = 3

bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
sees   = pre.see_tables()
tacts  = pre.tact_behavs()

density = 1000.

# create materials
matr = pre.material(name='MATER',materialType='RIGID',density=density)
mats.addMaterial(matr)

# create a model of rigid
modr = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(modr)

nb_c  = 1
nb_m  = 2
lsize = 0.2
radius = lsize / nb_m

marbles = pre.cubicLattice3D(nb_m, nb_m, nb_c, 2*radius, -lsize, -lsize, 1.) 
for coor in marbles:
  marble  = pre.rigidSphere(radius, coor, modr, matr)
  bodies += marble

bodies2 = copy.deepcopy(bodies)
bodies2.translate(dx=2.,dz=-1.+(2*nb_m*radius))
bodies += bodies2

mesh_slope = pre.readMesh('slope.msh', dim)

slope = pre.surfacicMeshToRigid3D(mesh_slope, modr, matr, color='BLUEx', is_open=True)
slope.imposeDrivenDof(dofty='vlocy', component=[1,2,3,4,5,6])
bodies += slope

#overriding Inertia
print('For slope, inertia parameters currently are : ')
avrd   = slope.bulks[0].avrd
volume = avrd**3 * np.pi * 4 / 3
print(avrd, volume, slope.bulks[0].inertia, slope.bulks[0].axis)

print('For slope, inertia parameters overwritten with dummy values: ')
volume  = 1.2
inertia = np.array( [1., 1., 2] )
frame   = np.eye( 3 )
frame[:,[0,1]] = frame[:,[1,0]]
print(volume, inertia, frame )

slope.bulks[0].setInertia( inertia )
slope.bulks[0].setFrame( frame )
slope.bulks[0].setVolume( volume )

# reverse pyramid
epols = pre.surfacicMeshToRigid3D(mesh_slope, modr, matr, color='BLUEx', is_open=True, reverse=True)

epols.imposeDrivenDof(dofty='vlocy', component=[1,2,3,4,5,6])
epols.rotate(theta=np.pi)
epols.translate(dx=2)
bodies += epols

# moving center of inertia under the whole surface
# IMPORTANT things should be written twice...
# moving center of inertia under the whole surface !
epols.contactors[0].shift[2] = -1.

iqsc0 = pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.5)
tacts.addBehav(iqsc0)

see1 = pre.see_table(CorpsCandidat='RBDY3'   , candidat='SPHER'   , colorCandidat='BLUEx'   , behav=iqsc0,
                     CorpsAntagoniste='RBDY3', antagoniste='SPHER', colorAntagoniste='BLUEx', alert=0.01*radius)
see2 = pre.see_table(CorpsCandidat='RBDY3'   , candidat='SPHER'   , colorCandidat='BLUEx'   , behav=iqsc0,
                     CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLUEx', alert=2*radius)
sees += see1
sees += see2

#pre.visuAvatars(bodies)
pre.writeDatbox(dim, mats, mods, bodies, tacts, sees)

slope_ibdyty = slope.number

from pylmgc90 import chipy
from pylmgc90.chipy import computation

dim      = 3
nb_steps = 1
dt       = 1e-2
theta    = 0.5

solver_param = { 'tol'   : 1e-4   ,
                 'relax' : 1.0    ,
                 'norm'  : 'Quad ',
                 'gs_it1': 10     ,
                 'gs_it2': 10     ,
                 'stype' : 'Stored_Delassus_Loops         '
               }

chipy.Initialize()

chipy.checkDirectories()
#chipy.utilities_DisableLogMes()

chipy.SetDimension(dim,0)

chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)

chipy.POLYR_SkipAutomaticReorientation()
chipy.ReadDatbox(deformable=False)

chipy.OpenDisplayFiles()

chipy.ComputeMass()
chipy.SetDomainBoundary(Zmin=-1.)

assert np.allclose( chipy.RBDY3_GetBodyMatrix('IFref',slope_ibdyty), frame)    , '[ERROR] Wrong frame in LMGC90'
assert np.allclose( chipy.RBDY3_GetBodyInertia(slope_ibdyty), density*inertia ), '[ERROR] Wrong inertia in LMGC90'
assert np.allclose( chipy.RBDY3_GetMass(slope_ibdyty), density*volume)         , '[ERROR] Wrong mass in LMGC90'

for k in range(nb_steps):
  computation.one_step(f_write=1, f_display=1, **solver_param)

inters = chipy.getInteractions()

chipy.Finalize()


check_dtype = ['inter', 'icdan', 'cdbdy', 'icdbdy', 'cdtac', 'icdtac', 'anbdy', 'ianbdy', 'antac', 'iantac', 'icdsci', 'iansci']

ref = np.array([(b'SPSPx', 1, b'RBDY3', 5, b'SPHER', 1, b'RBDY3',  7, b'SPHER', 1, 0, 0),
                (b'SPSPx', 2, b'RBDY3', 5, b'SPHER', 1, b'RBDY3',  6, b'SPHER', 1, 0, 0),
                (b'SPSPx', 3, b'RBDY3', 7, b'SPHER', 1, b'RBDY3',  8, b'SPHER', 1, 0, 0),
                (b'SPSPx', 4, b'RBDY3', 6, b'SPHER', 1, b'RBDY3',  8, b'SPHER', 1, 0, 0),
                (b'SPSPx', 5, b'RBDY3', 1, b'SPHER', 1, b'RBDY3',  2, b'SPHER', 1, 0, 0),
                (b'SPSPx', 6, b'RBDY3', 1, b'SPHER', 1, b'RBDY3',  3, b'SPHER', 1, 0, 0),
                (b'SPSPx', 7, b'RBDY3', 2, b'SPHER', 1, b'RBDY3',  4, b'SPHER', 1, 0, 0),
                (b'SPSPx', 8, b'RBDY3', 3, b'SPHER', 1, b'RBDY3',  4, b'SPHER', 1, 0, 0),
                (b'SPPRx', 1, b'RBDY3', 1, b'SPHER', 1, b'RBDY3',  9, b'POLYR', 1, 0, 0),
                (b'SPPRx', 2, b'RBDY3', 2, b'SPHER', 1, b'RBDY3',  9, b'POLYR', 1, 0, 0),
                (b'SPPRx', 3, b'RBDY3', 3, b'SPHER', 1, b'RBDY3',  9, b'POLYR', 1, 0, 0),
                (b'SPPRx', 4, b'RBDY3', 4, b'SPHER', 1, b'RBDY3',  9, b'POLYR', 1, 0, 0),
                (b'SPPRx', 5, b'RBDY3', 5, b'SPHER', 1, b'RBDY3', 10, b'POLYR', 1, 0, 0),
                (b'SPPRx', 6, b'RBDY3', 6, b'SPHER', 1, b'RBDY3', 10, b'POLYR', 1, 0, 0),
                (b'SPPRx', 7, b'RBDY3', 7, b'SPHER', 1, b'RBDY3', 10, b'POLYR', 1, 0, 0),
                (b'SPPRx', 8, b'RBDY3', 8, b'SPHER', 1, b'RBDY3', 10, b'POLYR', 1, 0, 0)],
                dtype={'names':['inter','icdan','cdbdy','icdbdy','cdtac','icdtac','anbdy','ianbdy','antac','iantac','icdsci','iansci'], 'formats':['S5','<i4','S5','<i4','S5','<i4','S5','<i4','S5','<i4','<i4','<i4'], 'offsets':[0,5,9,14,18,23,27,32,36,41,45,49], 'itemsize':371})

nor = np.array([[ 0.        , -1.        , -0.        ],
                [-1.        ,  0.        , -0.        ],
                [-1.        ,  0.        , -0.        ],
                [ 0.        , -1.        , -0.        ],
                [-1.        ,  0.        , -0.        ],
                [ 0.        , -1.        , -0.        ],
                [ 0.        , -1.        , -0.        ],
                [-1.        ,  0.        , -0.        ],
                [-1.        , -0.57735027,  0.        ],
                [-1.        , -0.57735027,  0.        ],
                [-1.        ,  0.57735027,  0.        ],
                [-1.        ,  0.57735027,  0.        ],
                [-0.70710678,  0.57735027,  0.40824829],
                [-0.70710678,  0.57735027, -0.40824829],
                [-0.70710678, -0.57735027, -0.40824829],
                [-0.70710678, -0.57735027,  0.40824829]])

assert np.all( ref==inters[check_dtype] ), 'differences in contact id with ref'
assert np.allclose( nor, inters['uc'][:,1] ), 'differences on normal with ref'


