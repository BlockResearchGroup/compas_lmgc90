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

dim = 2

# elastic quadrangle model
m2Dl = pre.model(name='M2DH8', physics='MECAx', element='Q4xxx', dimension=dim, 
                 external_model='no___', kinematic='small', material='elas_', 
                 anisotropy='iso__', mass_storage='lump_')
mods.addModel(m2Dl)

modR = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mods.addModel(modR)

# a elastic material
stone = pre.material(name='stone', materialType='ELAS', density=2750., elas='standard',
                     anisotropy='isotropic', young=7.e10, nu=0.2)  
mats.addMaterial(stone)

mat = pre.material(name='TDURx', materialType='RIGID', density=1000.)
mats.addMaterial(mat)

# three meshes at the bottom 
mesh_block = pre.buildMesh2D(mesh_type='Q4', x0=0., y0=0., lx=3., ly=1., nb_elem_x=3, nb_elem_y=1)
bas = pre.buildMeshedAvatar(mesh=mesh_block, model=m2Dl, material=stone)
bas.addContactors(group='up', shape='ALpxx', color='BLEUx')
bas.imposeDrivenDof(group='up',component=[1, 2], dofty='vlocy')
bas.imposeDrivenDof(group='down',component=[1, 2], dofty='vlocy')
bodies += bas

# two meshes at the top (1st test)
mesh_block = pre.buildMesh2D(mesh_type='Q4', x0=0.5, y0=1.5, lx=2., ly=1., nb_elem_x=2, nb_elem_y=1)
haut_gauche = pre.buildMeshedAvatar(mesh=mesh_block, model=m2Dl, material=stone)
haut_gauche.addContactors(group='down', shape='CLxxx', color='BLEUx', weights=[0.25,0.75])
haut_gauche.imposeDrivenDof(group='up',component=[1, 2], dofty='vlocy')
haut_gauche.imposeDrivenDof(group='down',component=[1, 2], dofty='vlocy')
bodies += haut_gauche

# one mesh at the top right (2nd test)
mesh_block = pre.buildMesh2D(mesh_type='Q4', x0=2.5, y0=1.5, lx=1, ly=1., nb_elem_x=1, nb_elem_y=1)
haut_droite = pre.buildMeshedAvatar(mesh=mesh_block, model=m2Dl, material=stone)
haut_droite.addContactors(group='down', shape='CLxxx', color='BLEUx')
haut_droite.imposeDrivenDof(group='up',component=[1, 2], dofty='vlocy')
haut_droite.imposeDrivenDof(group='down',component=[1, 2], dofty='vlocy')
bodies += haut_droite

# rigid bodies
d1 = pre.rigidDisk(0.5, [0.5,0.5], modR, mat)
d1.imposeDrivenDof(component=[2,3], dofty='vlocy')
d1.imposeDrivenDof(component=1, dofty='vlocy', ct=5.e+3)
d2 = pre.rigidDisk(0.5, [1.6,0.5], modR, mat)
d2.imposeDrivenDof(component=[1,2,3], dofty='vlocy')
bodies += d1
bodies += d2

# gestion des interactions :
lspsp = pre.tact_behav( name='mal1_', law='MAL_CZM', dyfr=0.1, stfr=0.1,
                        cn=1.e12, s1=3.16227e5, G1=0.1,
                        ct=1.e12, s2=3.16227e5, G2=0.1)
tacts+= lspsp

ldkdk = pre.tact_behav('iqsc0','IQS_CLB',fric=0.3)
tacts+= ldkdk

svspsp = pre.see_table(CorpsCandidat='MAILx', candidat='CLxxx', colorCandidat='BLEUx',
                       CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='BLEUx',
                       behav=lspsp, alert=1)
sees+= svspsp

svspsp = pre.see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='BLUEx',
                       CorpsAntagoniste='RBDY2', antagoniste='DISKx', colorAntagoniste='BLUEx',
                       behav=ldkdk, alert=0.2)
sees+= svspsp

post = pre.postpro_commands()

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mats, mods, bodies, tacts, sees, post=post, gravy=[0.,10.,0.])
