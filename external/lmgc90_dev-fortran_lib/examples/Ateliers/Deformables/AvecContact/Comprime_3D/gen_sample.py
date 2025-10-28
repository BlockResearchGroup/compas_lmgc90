
from pylmgc90.pre import *

import os
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# definition des conteneurs:
#   * de corps
bodies = avatars()
#   * de modeles
mods = models()
#   * de materiaux
mats = materials()
#   * pour les tables de visibilite
svs = see_tables()
#   * pour les lois de contact
tacts = tact_behavs()

# on se place en 3D
dim = 3

# materiau rigide

# creation d'un modele de rigide
mR3D = model(name='rigid', physics='MECAx', element='Rxx3D', dimension=3)

# creation d'un materiau
tdur = material(name='TDURx', materialType='RIGID', density=2500.)
mats.addMaterial(tdur)

# definition d'un modele elastique pour les cubes, mailles en hexaedres 
m3Dl = model(name='M3DH8', physics='MECAx', element='H8xxx', dimension=3, 
     external_model='MatL_', kinematic='small', material='elas_', 
     anisotropy='iso__', mass_storage='lump_') #mass_storage='coher')

# on ajoute le modele dans le conteneur
mods.addModel(m3Dl)

# on definit le materiau constitutif des cubes
stone = material(name='stone', materialType='ELAS', density=2500., elas='standard',
   anisotropy='isotropic', young=3.5e10, nu=0.2)  

# on l'ajoute dans le contenaur
mats.addMaterial(stone)

# construction des maillages :

# on contruit le maillage du cube
mesh_cube=buildMeshH8(x0=-0.5, y0=-0.5, z0=0., lx=1., ly=1.0, lz=1., nb_elem_x=4, nb_elem_y=4, nb_elem_z=4)

# on construit un avatar deformable pour le premier cube
cube=buildMeshedAvatar(model=m3Dl, material=stone,mesh=mesh_cube)

## contacteurs :
cube.addContactors(group='down', shape='CSpxx', color='BLEUx')
bodies += cube

# construction d'un polyedre rigide pour la fondation

floor_mesh = readMesh('../../gmsh/fondation.msh', dim)
floor=volumicMeshToRigid3D(volumic_mesh=floor_mesh, model=mR3D, material=tdur, color='FLOOR')

# on bloque la fondation
floor.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
floor.translate(dz=-0.2)

# ajout de la fondation dans le conteneur de corps
bodies.addAvatar(floor)

# gestion des interactions :
#   * declaration des lois
#       - avec la fondation
lcspr=tact_behav('gapc1', 'GAP_SGR_CLB', fric=0.)
tacts+=lcspr
##   * declaration des tables de visibilite
#       - avec la fondation
svcspr = see_table(CorpsCandidat='MAILx', candidat='CSxxx',
   colorCandidat='BLEUx', behav=lcspr, CorpsAntagoniste='RBDY3', 
   antagoniste='POLYR', colorAntagoniste='FLOOR', alert=0.01, halo=0.2)
svs+=svcspr

# Ecriture des fichiers pour LMGC
writeBodies(bodies, chemin='./DATBOX/')
writeDofIni(bodies, chemin='./DATBOX/')
writeDrvDof(bodies, chemin='./DATBOX/')
writeModels(mods,chemin='./DATBOX/')
writeGPVIni(bodies, chemin='./DATBOX/')
writeBulkBehav(mats, chemin='./DATBOX/', dim=dim, gravy=[0.,0.,-9.81])
writeTactBehav(tacts,svs,chemin='DATBOX/')
writeVlocRlocIni(chemin='DATBOX/')

### postpro
post = postpro_commands()
set_cube1 = [(cube, "down")]
cube_sets = postpro_command(name='NEW MECAx SETS',mecax_sets=[set_cube1])
post.addCommand(cube_sets)
# on suit leur deplacement
cube_disp = postpro_command(name='Dep EVOLUTION', step=1)
post.addCommand(cube_disp)
cube_fint = postpro_command(name='Fint EVOLUTION', step=1)
post.addCommand(cube_fint)
# suivi du sol :
#   * cinematique de la brique
floor_disp = postpro_command(name='BODY TRACKING', step=1,rigid_set=[floor])
post.addCommand(floor_disp)
#   * efforts subis par la brique
floor_torque = postpro_command(name='TORQUE EVOLUTION', step=1,rigid_set=[floor])
post.addCommand(floor_torque)
#
energy = postpro_command(name='KINETIC ENERGY', step=1)
post.addCommand(energy)
nlgs = postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(nlgs)
#
writePostpro(post, bodies, path='DATBOX/')

try:
  visuAvatars(bodies)
except:
  pass
