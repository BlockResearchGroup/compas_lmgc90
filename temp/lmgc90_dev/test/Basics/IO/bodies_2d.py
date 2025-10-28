import sys
import filecmp
import pathlib
from copy import deepcopy

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
sees   = pre.see_tables()

dim = 2

# models creation
rmod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
#mod1 = pre.model(name='M2DPQ', physics='MECAx', element='Q4xxx', dimension=dim, external_model='yes__',
#                 kinematic='large', formulation='TotaL', material='J2iso', anisotropy='iso__',
#                 mass_storage='coher')
mod1 = pre.model(name='M2DPQ', physics='MECAx', element='Q4P0x', dimension=dim, external_model='no___',
                 kinematic='large', formulation='UpdtL', material='J2iso', anisotropy='iso__',
                 mass_storage='coher')
mod2 = pre.model(name='M2DET', physics='MECAx', element='T3xxx', dimension=dim, external_model='no___',
                 kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')
mod3 = pre.model(name='M2DEQ', physics='MECAx', element='Q4P0x', dimension=dim, external_model='no___',
                 kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')

mods.addModel(rmod)
mods.addModel(mod1)
mods.addModel(mod2)
mods.addModel(mod3)

# materials creation
rmat1 = pre.material(name='TDURx', materialType='RIGID', density=1000.)
rmat2 = pre.material(name='PDURx', materialType='RIGID', density=100. )
lmat1 = pre.material(name='steel', materialType='ELAS_PLAS', density=8930., elas='standard', anisotropy='isotropic',
                     young=1.17e11, nu=0.35, critere='Von-Mises', isoh='linear', iso_hard=4.e8, isoh_coeff=1e8,
                     cinh='none', visc='none')
lmat2 = pre.material(name='stone', materialType='ELAS', density=2750., elas='standard', anisotropy='isotropic',
                     young=7.e10, nu=0.2)

mats.addMaterial(rmat1)
mats.addMaterial(rmat2)
mats.addMaterial(lmat1)
mats.addMaterial(lmat2)


# adding two diskx in a xksid:
rk = 1.2
xksid = pre.rigidDisk(r=rk, center=[rk,rk], model=rmod, material=rmat1, color='xKSID', is_Hollow=True)
rd = 0.2
disk1 = pre.rigidDisk(r=rd, center=[2*rk-3.1*rd,rk], model=rmod, material=rmat2, color='DK4KD')
disk2 = pre.rigidDisk(r=rd, center=[2*rk-1.1*rd,rk], model=rmod, material=rmat2, color='DK4KD')


# creating a cluster of disk, polyg and a pt2dx at the base of the polyg
# duplicate it, return it and put the two of them, next to each other on a joncx 
vertices = np.array([-rd,-2.*rd, rd,-2.*rd, rd,rd, -rd,rd]).reshape([4,2])
clust1 = pre.rigidPolygon(center=[0.,0.], nb_vertices=4, vertices=vertices,
                          model=rmod, material=rmat2, color='CLUST', generation_type='full')
clust1.addContactors(shape='PT2Dx', color='CLUST', shift=[0.,-1.5*rd])
clust1.addContactors(shape='DISKx', color='CLUST', shift=[0., 1.5*rd], byrd=rd)
clust1.computeRigidProperties()
clust1.translate(dy=2.*rd)
clust2 = deepcopy(clust1)
clust2.rotate( psi=np.pi+1e-1, center=[0.,2.*rd] )
clust2.translate(dx=-3.*rd)

joncx = pre.rigidJonc(axe1=1.25*rk, axe2=rk*0.1, center=[-rk, -rk*0.1], model=rmod, material=rmat1, color='JONCx')


# adding a polygon and and a disk above first cluster
trian = pre.rigidPolygon(center=[-3.*rd,5.*rd+1e-6], model=rmod, material=rmat2, color='TRIAN',
                         generation_type='regular', nb_vertices=3, radius=rd)
disk3 = pre.rigidDisk(r=rd, center=[-3*rd,7.*rd+1e-6], model=rmod, material=rmat2, color='DISKx')


# adding a mesh on the joncx and a second mesh above it
mesh  = pre.buildMesh2D('Q4', x0=-2.5*rk, y0=0.0, lx=rk, ly=rk/2., nb_elem_x=10, nb_elem_y=5)
mesh1 = pre.buildMeshedAvatar(mesh=mesh, model=mod1, material=lmat1)
mesh1.addContactors(group='up', shape='ALpxx', color='ALpxx')
mesh1.addContactors(group='down', shape='CLxxx', color='CLxxx')
mesh1.addContactors(group='left', shape='PT2DL', color='PT2DL', weights=[0.5])

mesh  = pre.buildMesh2D('4T3', x0=-2.*rk, y0=rk/2., lx=rk, ly=rk/2., nb_elem_x=10, nb_elem_y=5)
mesh2 = pre.buildMeshedAvatar(mesh=mesh, model=mod2, material=lmat2)
mesh2.addContactors(group='up', shape='ALpxx', color='ALpxx')
mesh2.addContactors(group='down', shape='CLxxx', color='CLxxx')

# adding a disk and a polygon on the upper mesh
disk4 = pre.rigidDisk(r=rd, center=[-1.5*rk,rk+rd], model=rmod, material=rmat2, color='DISKx')
penta = pre.rigidPolygon(center=[-1.5*rk+2*rd,rk+rd], model=rmod, material=rmat2, color='PENTA',
                         generation_type='regular', nb_vertices=5, radius=rd)

# add a mesh with PT2DL contactors to attach to left side of first mesh
mesh  = pre.buildMesh2D('Q4', x0=-3.*rk, y0=-rk/2., lx=rk/2., ly=rk, nb_elem_x=5, nb_elem_y=10)
mesh3 = pre.buildMeshedAvatar(mesh=mesh, model=mod3, material=lmat2)
mesh3.addContactors(group='right', shape='PT2DL', color='PT2DL', weights=[0.5])

# DISKL and DKDKL is missing

#boundary condition and initial values
disk1.imposeInitValue(component=1, value=1.2)
xksid.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
joncx.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
mesh3.imposeDrivenDof(group='left', component=1, dofty='force', ct=0.1)

for b in (xksid, disk1, disk2, clust1, clust2, joncx, trian, disk3, mesh1, mesh2, disk4, penta, mesh3):
     bodies.addAvatar(b)
 
# generating visibility tables for all these
iqsc0 = pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.5)
gapc0 = pre.tact_behav(name='gapc0', law='GAP_SGR_CLB', fric=0.7)
elas0 = pre.tact_behav(name='elas0', law='ELASTIC_WIRE', stiffness=5.e3, prestrain=-0.2)
cpldo = pre.tact_behav(name='cpldo', law='COUPLED_DOF')

for t in (iqsc0, gapc0, elas0, cpldo):
    tacts.addBehav(t)

# dkdk and dkkd
sv1 = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='DK4KD',
                    CorpsAntagoniste='RBDY2', antagoniste='DISKx', colorAntagoniste='DK4KD',
                    behav=iqsc0, alert=0.05)
sv2 = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='DK4KD',
                    CorpsAntagoniste='RBDY2', antagoniste='xKSID', colorAntagoniste='xKSID',
                    behav=iqsc0, alert=0.05)
# clust/joncx, clust/clust, trian/clust, disk/trian
sv3 = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='CLUST',
                    CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='JONCx',
                    behav=iqsc0, alert=0.05)
sv4 = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='POLYG', colorCandidat   ='CLUST',
                    CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='JONCx',
                    behav=iqsc0, alert=0.05)
sv5 = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='PT2Dx', colorCandidat   ='CLUST',
                    CorpsAntagoniste='RBDY2', antagoniste='PT2Dx', colorAntagoniste='CLUST',
                    behav=elas0, alert=4.*rd)
sv6 = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='POLYG', colorCandidat   ='TRIAN',
                    CorpsAntagoniste='RBDY2', antagoniste='POLYG', colorAntagoniste='CLUST',
                    behav=iqsc0, alert=0.05)
sv7 = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='DISKx',
                    CorpsAntagoniste='RBDY2', antagoniste='POLYG', colorAntagoniste='TRIAN',
                    behav=iqsc0, alert=0.05)
# mesh/jonc, mesh/mesh, disk/mesh, penta/mesh
sv8 = pre.see_table(CorpsCandidat   ='MAILx', candidat   ='CLxxx', colorCandidat   ='CLxxx',
                    CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='JONCx',
                    behav=gapc0, alert=0.05)
sv9 = pre.see_table(CorpsCandidat   ='MAILx', candidat   ='CLxxx', colorCandidat   ='CLxxx',
                    CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='ALpxx',
                    behav=gapc0, alert=0.05)
sv10= pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='DISKx',
                    CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='ALpxx',
                    behav=gapc0, alert=0.05)
sv11= pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='POLYG', colorCandidat   ='PENTA',
                    CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='ALpxx',
                    behav=gapc0, alert=0.05)
# mesh/mesh with PTPT
sv12= pre.see_table(CorpsCandidat   ='MAILx', candidat   ='PT2DL', colorCandidat   ='PT2DL',
                    CorpsAntagoniste='MAILx', antagoniste='PT2DL', colorAntagoniste='PT2DL',
                    behav=cpldo, alert=0.05)


for s in range(1,13):
    see = eval('sv'+str(s))
    sees.addSeeTable(see)


# adding a THERM example :
modT = pre.model(name='DIFFU', physics='THERx', element='S2xth', dimension=dim,
                 external_model='no___', capacity_storage='lump_', formulation = 'class',
                 external_fields =['SECTION'])
mods.addModel(modT)
matT = pre.material(name='steeT', materialType='THERMO_ELAS', density=1.0,
                    elas='standard', young=0.0, nu=0.0, T_ref_meca=0.0, 
                    anisotropy='isotropic', dilatation = 0.0,
                    conductivity=0.002, specific_capacity=0.000005)
mats.addMaterial(matT)

mesh  = pre.readMesh('barre1D.msh', dim=dim, keep_elements=[1])
meshT = pre.buildMeshedAvatar(mesh=mesh, model=modT, material=matT)
meshT.groups.addGroup( pre.group.group('L') )
meshT.groups.addGroup( pre.group.group('R') )
meshT.groups['L'].addNode( meshT.nodes[ 1] )
meshT.groups['R'].addNode( meshT.nodes[21] )
meshT.imposeDrivenDof(group='L',component = 1, dofty='temp', ct =  0.0, rampi = 1.0)
meshT.imposeDrivenDof(group='R',component = 1, dofty='temp', ct =100.0, rampi = 1.0)
#meshT.imposeInitValue(group='all', component = 1, value = 0.0)
meshT.translate(dy=-rk)

bodies += meshT

pre.visuAvatars(bodies)

# file writing
pre.writeDatbox( dim, mats, mods, bodies, tacts, sees )


# Run a computation with this case !

# space dimension
dim = 2
# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1
# time evolution parameters
dt = 1e-4
nb_steps = 10
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
                 'gsit1' : 50     ,
                 'gsit2' : 1000   ,
                 'stype' : 'Stored_Delassus_Loops         '
               }
# write parameter
freq_write   = 1
# display parameters
freq_display = 1

if '--with-hdf5' in sys.argv:
    h5_file = 'bodies_2.h5'
else:
    h5_file = None

#initialize
chipy.Integrator_InitCrankNickolson(theta)
utils.init_lmgc90(dt, theta, dim, mhyp, deformable, h5_file)
for k in range(nb_steps):
  utils.compute_lmgc90_one_step(solver_param, freq_write, freq_display)
utils.finalize_lmgc90()



# try to read last step:
mats2, mods2, bodies2, tacts2, sees2, inters2 = pre.readDatbox(dim, 'DATBOX')
inters2 = pre.readState(bodies2, 'OUTBOX', -1)

# writing read data in new directory
pre.writeDatbox( dim, mats2, mods2, bodies2, tacts2, sees2, inters2, datbox_path='DATBOX2' )

for f in ('BULK_BEHAV', 'MODELS', 'TACT_BEHAV', 'DRV_DOF'):#, 'BODIES'):
    f1 = datbox/(f+'.DAT')
    f2 = pathlib.Path('DATBOX2/'+f+'.DAT')
    assert filecmp.cmp(f1,f2,shallow=False), "{} is not read/write".format(f)

# leftover to diff: all INI files and BODIES.DAT for small differences

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
