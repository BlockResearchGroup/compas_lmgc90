import copy

from pathlib import Path

from pylmgc90 import pre

datbox = Path('./DATBOX')
datbox.mkdir(exist_ok=True)

# definition des conteneurs:
bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
sees   = pre.see_tables()
tacts  = pre.tact_behavs()

dim = 3

# elastic hexaedron model
m3Dl = pre.model(name='M3DH8', physics='MECAx', element='H8xxx', dimension=3, 
                 external_model='no___', kinematic='small', material='elas_', 
                 anisotropy='iso__', mass_storage='lump_')
mods.addModel(m3Dl)

modR = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(modR)

# a elastic material
stone = pre.material(name='stone', materialType='ELAS', density=2750., elas='standard',
                     anisotropy='isotropic', young=7.e10, nu=0.2)  
mats.addMaterial(stone)

mat = pre.material(name='TDURx', materialType='RIGID', density=1000.)
mats.addMaterial(mat)

c1 = {'x0':0.0, 'y0':0. , 'z0':0., 'lx':2., 'ly':2., 'lz':2., 'nb_elem_x':2, 'nb_elem_y':2, 'nb_elem_z':2}
c2 = {'x0':0.5, 'y0':0.5, 'z0':2., 'lx':1., 'ly':1., 'lz':1., 'nb_elem_x':1, 'nb_elem_y':1, 'nb_elem_z':1}
c3 = {'x0':3.0, 'y0':0.5, 'z0':0., 'lx':1., 'ly':1., 'lz':1., 'nb_elem_x':1, 'nb_elem_y':1, 'nb_elem_z':1}
c4 = {'x0':3.0, 'y0':0.5, 'z0':1., 'lx':1., 'ly':1., 'lz':1., 'nb_elem_x':1, 'nb_elem_y':1, 'nb_elem_z':1}

all_meshes = [ (c1, 'up'  , {'shape':'ASpxx',},),
               (c2, 'down', {'shape':'CSpxx',},),
               (c3, 'up'  , {'shape':'ASpxx',},),
               (c4, 'down', {'shape':'CSpxx', 'quadrature':0},),
             ]

# meshes to avatar
for c, g, s in all_meshes:
  m = pre.buildMeshH8(**c)
  cube = pre.buildMeshedAvatar(mesh=m, model=m3Dl, material=stone)
  cube.addContactors(group=g, color='BLEUx', **s)
  cube.imposeDrivenDof(group='up',component=[1, 2, 3], dofty='vlocy')
  cube.imposeDrivenDof(group='down',component=[1, 2, 3], dofty='vlocy')
  bodies += cube

# rigid bodies
s1 = pre.rigidSphere(0.5, [0.5,3.5,0.5], modR, mat)
s1.imposeDrivenDof(component=[2,3,4,5,6], dofty='vlocy')
s1.imposeDrivenDof(component=1, dofty='vlocy', ct=5.e+3)
s2 = pre.rigidSphere(0.5, [1.6,3.5,0.5], modR, mat)
s2.imposeDrivenDof(component=[1,2,3,4,5,6], dofty='vlocy')
bodies += s1
bodies += s2

# gestion des interactions :
lspsp = pre.tact_behav('iqsc0','IQS_CLB',fric=0.3)
tacts+= lspsp
lcsas = pre.tact_behav('gapc0','GAP_SGR_CLB',fric=0.3)
tacts+= lcsas

svcsas = pre.see_table(CorpsCandidat='MAILx', candidat='CSxxx', colorCandidat='BLEUx',
                       CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='BLEUx',
                       behav=lcsas, alert=0.1, halo=1.0)
sees+= svcsas
svspsp = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='BLUEx',
                       CorpsAntagoniste='RBDY3', antagoniste='SPHER', colorAntagoniste='BLUEx',
                       behav=lspsp, alert=0.2)
sees+= svspsp


post = pre.postpro_commands()

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mats, mods, bodies, tacts, sees, post=post)
