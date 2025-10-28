import math

from pylmgc90.pre import *

import os
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# on se place en 2D
dim=3
Lx=1.
Ly=5.
Lz=1.

# creation des conteneurs
#   * pour les corps
bodies = avatars()
#   * pour les materiaux
mats = materials()
#   * pour les modeles
mods = models()
#   * pour les tables de visibilite
svs = see_tables()
#   * pour les lois de contact
tacts = tact_behavs()

# materiau
cuivr = material(name='cuivr', materialType='ELAS', elas='standard',
   young=1.17e11, nu=0.33, anisotropy='isotropic', density=8920.)  
mats.addMaterial(cuivr)

# modele
m2Dl = model(name='M2D_L', physics='MECAx', element='H8xxx', dimension=dim, external_model='MatL_',
     formulation='TotaL', kinematic='large', material='neoh_', anisotropy='iso__', mass_storage='coher')
mods.addModel(m2Dl)

mesh_block=buildMeshH8(x0=0., y0=0., z0=0., lx=Lx, ly=Ly, lz=Lz, nb_elem_x=2, nb_elem_y=10,nb_elem_z=2)

# on contruit un corps maille a partir du maillage du bloc
body=buildMeshedAvatar(mesh=mesh_block,model=m2Dl, material=cuivr)

# predicat: ligne proche
def lp(x):
   n=[1., 0., 0.]

   x0=[0., 0., 0.5]

   y = x - x0

   v=[(y[1]*n[2])-(y[2]*n[1]),(y[2]*n[0])-(y[0]*n[2]),(y[0]*n[1])-(y[1]*n[0])]

   d = ((v[0]**2) + (v[1]**2) + (v[2]**2)) / ((n[0]**2) + (n[1]**2) +(n[2]**2))

   return d <= (1.e-3**2)

# predicat: point proche
def pp(x):

   x0=[0.5, 5., 0.5]

   y = x - x0

   d = ((y[0]**2) + (y[1]**2) + (y[2]**2)) 

   return d <= (1.e-1**2)

body.addGroupUsingPredicate(name='line', predicate=lp, super_group='all')

body.imposeDrivenDof(group='line',component=[1, 2, 3], dofty='vlocy')

body.addGroupUsingPredicate(name='point', predicate=pp, super_group='all')

# ajout du corps a la liste des corps
bodies += body

# Ecriture des fichiers pour LMGC
writeBodies(bodies, chemin='./DATBOX/')
writeDofIni(bodies, chemin='./DATBOX/')
writeDrvDof(bodies, chemin='./DATBOX/')
writeModels(mods,chemin='./DATBOX/')
writeGPVIni(bodies, chemin='./DATBOX/')
writeBulkBehav(mats, chemin='./DATBOX/', dim=dim)
writeTactBehav(tacts,svs,chemin='DATBOX/')
writeVlocRlocIni(chemin='DATBOX/')

post = postpro_commands()

body_set = [(body, 'point')]
body_sets = postpro_command(name='NEW MECAx SETS',mecax_sets=[body_set])
post.addCommand(body_sets)
disp = postpro_command(name='Dep EVOLUTION', step=1)
post.addCommand(disp)

writePostpro(post, bodies, path='./DATBOX/')

try:
  visuAvatars(bodies)
except:
  pass
