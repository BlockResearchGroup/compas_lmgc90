
from pylmgc90.pre import *

import os
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# definition du conteneur de partie ou de pieces, des modeles et des materiaux
ps = avatars()
ms = models()
mx = materials()
svs = see_tables()
tacts = tact_behavs()

dim=3

# modele 
mod = model(name='TET__',physics='MECAx',element='TE4xx',external_model='MatL_',kinematic='small',
            material='elas_',anisotropy='iso__',mass_storage='coher', dimension=dim)
ms.addModel(mod)

# materiau
acier = material(name='acier',materialType='ELAS',elas='standard',
                 young=2e+12,nu=0.3,anisotropy='isotropic',
                 density=0.)
mx.addMaterial(acier)


# lecture maillage
#mesh = readMesh(name='../../gmsh/cube.msh',dim=dim)
#mesh = readMesh(name='../../gmsh/cube_fin.msh',dim=dim)
mesh = readMesh(name='../../gmsh/cube_tres_fin.msh',dim=dim)
mesh.rankRenumbering()


# modele mecaMAILx
cube = buildMeshedAvatar(mesh=mesh, model=mod, material=acier)

# conditions aux limites
# bas
cube.imposeDrivenDof(group='31',component=3,dofty='vlocy')
# haut
cube.imposeDrivenDof(group='30',component=3,ct=1.e-3,ramp=1.,dofty='vlocy')

# predicat de distance a un axe
def p(x):

   n=[0., 0., 1.]

   x0=[0.5, 0.5, 0.]

   y = x - x0

   proj = (y[0]*n[0]) + (y[1]*n[1]) + (y[2]*n[2])

   v=[y[0] - (proj*n[0]),y[1] - (proj*n[1]),y[2] - (proj*n[2])]

   d = (v[0]**2) + (v[1]**2) + (v[2]**2)

   return d <= (1.e-1**2)

# creation d'un nouveau groupe
cube.addGroupUsingPredicate(name='line', predicate=p, super_group='all')

# on bloque la fondation
cube.imposeDrivenDof(group='line',component=[1, 2], dofty='vlocy')
ps += cube

# Ecriture des fichiers pour LMGC
writeBodies(ps,chemin='./DATBOX/')
writeModels(ms,chemin='./DATBOX/')
writeDrvDof(ps,chemin='./DATBOX/')
writeDofIni(ps,chemin='./DATBOX/')
writeGPVIni(ps,chemin='./DATBOX/')
writeVlocRlocIni(chemin='./DATBOX/')
writeBulkBehav(mx,chemin='./DATBOX/')
writeTactBehav(tacts,svs,chemin='./DATBOX/')

post = postpro_commands()
set_cube = [(cube, '30')]
cube_sets = postpro_command(name='NEW MECAx SETS',mecax_sets=[set_cube])
post.addCommand(cube_sets)
cube_disp = postpro_command(name='Dep EVOLUTION', step=1)
post.addCommand(cube_disp)
cube_fint = postpro_command(name='Fint EVOLUTION', step=1)
post.addCommand(cube_fint)

writePostpro(post, ps, path='./DATBOX/')

try:
  visuAvatars(ps)
except:
  pass
