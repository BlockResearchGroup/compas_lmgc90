import numpy as np
import math
import sys

from pylmgc90.pre import *

import os
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

dim = 2

ps = avatars()
ms = models()
mx = materials()
svs = see_tables()
tacts = tact_behavs()

# definition des modeles 
mod = model(name='Q4MNL',physics='MECAx',element='Q4xxx', dimension=dim, external_model='MatL_',kinematic='large',
              formulation='TotaL',material='neoh_',anisotropy='iso__',mass_storage='lump_')
ms.addModel(mod)

# Definition des materiaux
mat1 = material(name='dur__',materialType='ELAS',elas='standard',
                 young=68.96e+8,nu=0.32,anisotropy='isotropic',
                 density=20.)
mat2 = material(name='mou__',materialType='ELAS',elas='standard',
                 young=68.96e+7,nu=0.32,anisotropy='isotropic',
                 density=20.)
mx.addMaterial(mat1,mat2)

# definition des parties maillees
# dessus
mesh1=buildMesh2D(mesh_type='Q4', x0=-1, y0=0., lx=2, ly=1, nb_elem_x=4, nb_elem_y=2)

body1=buildMeshedAvatar(mesh=mesh1, model=mod, material=mat1)
body1.addContactors(group='down',shape='ALpxx',color='xxxxx')

def vy(t):
   if t <= 1.:
      return -0.1
   else:
      return  0.

writeEvolution(f=vy, instants=np.linspace(0,2,200) ,path='./DATBOX/', name='vy.dat')

body1.imposeDrivenDof(group='up',component=1,dofty='vlocy')
body1.imposeDrivenDof(group='up',component=2,dofty='vlocy',description='evolution',evolutionFile='vy.dat')

ps.addAvatar(body1)

mesh2=buildMesh2D(mesh_type='Q4', x0=-2, y0=-1, lx=4, ly=1, nb_elem_x=32, nb_elem_y=8)
body2=buildMeshedAvatar(mesh=mesh2, model=mod, material=mat2)

a=0.5*(1-0.577350269189626)
b=0.5*(1+0.577350269189626)
body2.addContactors(group='up',shape='CLxxx',color='xxxxx',weights=[a,b])

#body2.addContactors(group='up',shape='CLxxx',color='xxxxx')

body2.imposeDrivenDof(group='down',component=[1,2],dofty='vlocy')
ps.addAvatar(body2)

# Definition des interactions et des tables de visibilites
#...interaction
b = tact_behav(name='gapc0',law='GAP_SGR_CLB',fric=0.3)
tacts+=b

#.. table de visibilite
sv = see_table(CorpsCandidat=   'MAILx',candidat=   'CLxxx',colorCandidat=   'xxxxx',behav=b,
               CorpsAntagoniste='MAILx',antagoniste='ALpxx',colorAntagoniste='xxxxx',alert=1.,halo=1.)
svs+=sv

# Ecriture des fichiers pour LMGC
writeBodies(ps,chemin='./DATBOX/')
writeModels(ms,chemin='./DATBOX/')
writeDrvDof(ps,chemin='./DATBOX/')
writeDofIni(ps,chemin='./DATBOX/')
writeVlocRlocIni(chemin='./DATBOX/')
writeGPVIni(ps,chemin='./DATBOX/')
writeBulkBehav(mx,chemin='./DATBOX/',gravy=[0., 0., 0.])
writeTactBehav(tacts,svs,chemin='./DATBOX/')

post = postpro_commands()
set1 = [(body1, "down")]
sets = postpro_command(name='NEW MECAx SETS',mecax_sets=[set1])
post.addCommand(sets)
disp = postpro_command(name='Dep EVOLUTION', step=1)
post.addCommand(disp)
f = postpro_command(name='Fint EVOLUTION', step=1)
post.addCommand(f)
writePostpro(post, ps, path='./DATBOX/')

try:
  visuAvatars(ps)
except:
  pass
