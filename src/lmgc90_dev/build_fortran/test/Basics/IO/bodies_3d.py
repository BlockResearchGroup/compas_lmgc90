import sys
import filecmp
import pathlib

# import des modules
import numpy as np

from pylmgc90 import pre, chipy

import utils

pre.setStopMode('exception')

datbox = pathlib.Path('DATBOX')
datbox.mkdir(exist_ok=True)

bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
tacts  = pre.tact_behavs()
svs    = pre.see_tables()

dim = 3

mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mat = pre.material(name='TDURx', materialType='RIGID', density=1000.)
mats.addMaterial(mat)

planx = pre.rigidPlan(axe1=1., axe2=1., axe3=0.2, center=[0., 0., -0.2], model=mod, material=mat, color='PLANx')
planx.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies.addAvatar(planx)
   
spher1 = pre.rigidSphere(r=0.11, center=[0.,0.,0.11], model=mod, material=mat, color='SPHER')
spher2 = pre.rigidSphere(r=0.1, center=[-0.01,0.,0.315], model=mod, material=mat, color='SPHER')
cylnd1 = pre.rigidCylinder(r=0.11, h=0.25, center=[-0.22 ,0.,0.11 ], model=mod, material=mat, color='CYLND')
cylnd2 = pre.rigidCylinder(r=0.1 , h=0.25, center=[-0.199,0.,0.315], model=mod, material=mat, color='CYLND')
alpha  = np.pi/2.
frame  = [[1., 0., 0.,], [0., np.cos(alpha), -np.sin(alpha)], [0., np.sin(alpha), np.cos(alpha)]]
cylnd2.addContactors(shape='CYLND', byrd=0.1, High=0.125, shift=[0., 0.125, 0.125], frame=frame, color='CYLND')
cylnd1.rotate(description='axis',alpha=np.pi/2.,axis=[1.,0.,0.],center=[-0.22 ,0.,0.11])
cylnd2.rotate(description='axis',alpha=np.pi/2.,axis=[1.,0.,0.],center=[-0.199,0.,0.315])

vertices = np.array([0.25,-0.25,0. , 0.25,0.25,0. , 0.75,0.25,0. , 0.75,-0.25,0., \
                        0.25,-0.25,0.1, 0.25,0.25,0.1, 0.75,0.25,0.1, 0.75,-0.25,0.1])
vertices.shape=[8,3]
polyr1 = pre.rigidPolyhedron(model=mod, material=mat, generation_type='vertices', vertices=vertices, color='POLYR')


bodies.addAvatar(spher1)
bodies.addAvatar(spher2)
bodies.addAvatar(cylnd1)
bodies.addAvatar(cylnd2)
bodies.addAvatar(polyr1)

mat = pre.material(name='TTDUR', materialType='RIGID', density=10000.)
mats.addMaterial(mat)
dnlyc = pre.rigidCylinder(r=0.1, h=0.125, center=[-0.65,0.,0.4], model=mod, material=mat, color='DNLYC', is_Hollow=True)
dnlyc.addContactors(shape='PT3Dx',color='PT3Dx',shift=[0.,-0.05,0.055])
dnlyc.rotate(description='axis',alpha=np.pi/3.,axis=[1.,0.,0.],center=[-0.65,0.,0.4])
dnlyc.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
spher3 = pre.rigidSphere(r=0.05, center=[-0.65,-0.05,0.4], model=mod, material=mat, color='SPHER')
spher3.addContactors(shape='PT3Dx',color='PT3Dx',shift=[0.,0.,0.05])
spher3.rotate(description='axis',alpha=np.pi/3.,axis=[1.,0.,0.],center=[-0.65,0.,0.4])

bodies.addAvatar(dnlyc)
bodies.addAvatar(spher3)

polyr3 = pre.rigidPolyhedron(model=mod, material=mat, generation_type='regular', radius=0.05, nb_vertices=4, color='PYRAM')

mod = pre.model(name='M3DH8', physics='MECAx', element='H8xxx', dimension=3, external_model='no___',
                kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')
mods.addModel(mod)

mat = pre.material(name='stone', materialType='ELAS', density=2750., elas='standard', anisotropy='isotropic',
                   young=7.e10, nu=0.2)  
mats.addMaterial(mat)

mesh = pre.buildMeshH8(0.3,-0.2,0.1,0.4,0.4,0.2,4,4,2)
mesh1 = pre.buildMeshedAvatar(mesh=mesh,model=mod,material=mat)
mesh1.addContactors(group='up',shape='ASpxx',color='ASpxx')
mesh1.addContactors(group='down',shape='CSpxx',color='CSpxx', quadrature=1)

from copy import deepcopy
mesh2 = deepcopy(mesh1)

#mesh1.translate(dz=0.001)
mesh2.translate(dz=0.2)

polyr2 = deepcopy(polyr1)
polyr2.translate(dy=0.48,dz=0.1)

bodies.addAvatar(mesh1)
bodies.addAvatar(mesh2)
bodies.addAvatar(polyr2)

polyr3.translate(dx=0.5,dy=0.,dz=0.52)
bodies.addAvatar(polyr3)

# definition d'une loi de contact frottant, avec pre-gap
iqsc0=pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.5)
gapc0=pre.tact_behav(name='gapc0', law='GAP_SGR_CLB', fric=0.5)
elas0=pre.tact_behav(name='elas0', law='ELASTIC_WIRE', stiffness=5.e3, prestrain=0.)

tacts.addBehav(iqsc0)
tacts.addBehav(gapc0)
tacts.addBehav(elas0)

sv1 = pre.see_table(CorpsCandidat='RBDY3', candidat='CYLND', colorCandidat='CYLND', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='CYLND', colorAntagoniste='CYLND', alert=0.1)

sv2 = pre.see_table(CorpsCandidat='RBDY3', candidat='CYLND', colorCandidat='CYLND', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='PLANx', alert=0.1)

sv3 = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='POLYR', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='PLANx', alert=0.1)

sv4 = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='POLYR', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='POLYR', alert=0.1)

sv5 = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='SPHER', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='CYLND', colorAntagoniste='CYLND', alert=0.1)

sv6 = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='SPHER', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='PLANx', alert=0.1)

sv7 = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='SPHER', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='SPHER', colorAntagoniste='SPHER', alert=0.1)

sv8 = pre.see_table(CorpsCandidat='MAILx', candidat='CSxxx', colorCandidat='CSpxx', behav=gapc0,
                    CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='ASpxx', alert=0.05, halo=1.)

sv9 = pre.see_table(CorpsCandidat='MAILx', candidat='CSxxx', colorCandidat='CSpxx', behav=gapc0,
                    CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='POLYR', alert=0.05)

sva = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='PYRAM', behav=gapc0,
                   CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='ASpxx', alert=0.01, halo=0.1)

svb = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='SPHER', behav=iqsc0,
                    CorpsAntagoniste='RBDY3', antagoniste='DNLYC', colorAntagoniste='DNLYC', alert=0.1)

svc = pre.see_table(CorpsCandidat='RBDY3', candidat='PT3Dx', colorCandidat='PT3Dx', behav=elas0,
                    CorpsAntagoniste='RBDY3', antagoniste='PT3Dx', colorAntagoniste='PT3Dx', alert=0.1)


svs.addSeeTable(sv1)
svs.addSeeTable(sv2)
svs.addSeeTable(sv3)
svs.addSeeTable(sv4)
svs.addSeeTable(sv5)
svs.addSeeTable(sv6)
svs.addSeeTable(sv7)
svs.addSeeTable(sv8)
svs.addSeeTable(sv9)
svs.addSeeTable(sva)
svs.addSeeTable(svb)
svs.addSeeTable(svc)


try:
  pre.visuAvatars(bodies)
except:
  pass

# ecriture des fichiers de donnees pour LMGC90
pre.writeDatbox( dim, mats, mods, bodies, tacts, svs )


# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 0

# time evolution parameters
dt = 4e-3
nb_steps = 40

# theta integrator parameter
theta = 0.501

# deformable  yes=1, no=0
deformable = 1

# interaction parameters
Rloc_tol = 5.e-3

# nlgs parameters
solver_param = { 'conv'  : 1e-4   ,
                 'relax' : 1.0    ,
                 'norm'  : 'Quad ',
                 'gsit1' : 10     ,
                 'gsit2' : 10     ,
                 'stype' : 'Stored_Delassus_Loops         '
               }

# write parameter
freq_write = 1
# display parameters
freq_display = 1

if '--with-hdf5' in sys.argv:
    h5_file = 'bodies_3.h5'
else:
    h5_file = None

#initialize
utils.init_lmgc90(dt, theta, dim, mhyp, deformable, h5_file)
chipy.Integrator_InitCrankNickolson(theta)
for k in range(nb_steps):
  utils.compute_lmgc90_one_step(solver_param, freq_write, freq_display)
utils.finalize_lmgc90()



# try to read last step:
mats2, mods2, bodies2, tacts2, sees2, inters2 = pre.readDatbox(dim, 'DATBOX')
inters2 = pre.readState(bodies2, 'OUTBOX', -1)

# writing read data in new directory
pre.writeDatbox( dim, mats2, mods2, bodies2, tacts2, sees2, inters2, datbox_path='DATBOX2' )

for f in ('BULK_BEHAV', 'MODELS', 'TACT_BEHAV', 'DRV_DOF', 'BODIES'):
    f1 = datbox/(f+'.DAT')
    f2 = pathlib.Path('DATBOX2/'+f+'.DAT')
    assert filecmp.cmp(f1,f2,shallow=False), "{} is not read/write correctly".format(f)

# leftover to diff: all INI files for small differences


# check hdf5
if h5_file is not None:
    inters2 = pre.readState(bodies2, 'OUTBOX', nb_steps, h5_file, tacts2)
    pre.writeDatbox( dim, mats2, mods2, bodies2, tacts2, sees2, inters2, datbox_path='DATBOX3' )

# checking update reference config
# using list of list... so disgusting
ref_coor = [ [ b.getNodeCoor(n.number).tolist() for n in b.nodes ] for b in bodies2 ]
bodies2.updateReferenceConfig()
disp_ok = True
for b in bodies2:
  for n in b.nodes:
    disp_ok = np.all( n.dof.disp == 0. )
    if not disp_ok:
      break
  if not disp_ok:
    break
assert disp_ok, 'After updating reference configuration, there are node with a disp not null'
new_coor = [ [ b.getNodeCoor(n.number).tolist() for n in b.nodes ] for b in bodies2 ]
assert new_coor == ref_coor, 'Different configuration before and after UpdateReferenceConfig'
pre.visuAvatars(bodies2)
pre.writeDatbox( dim, mats2, mods2, bodies2, tacts2, sees2, inters2, datbox_path='DATBOX4' )
