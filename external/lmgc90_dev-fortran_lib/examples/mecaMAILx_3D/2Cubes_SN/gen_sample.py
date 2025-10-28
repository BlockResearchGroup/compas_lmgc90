import copy

import os

if (not os.path.isdir('./DATBOX')):
  os.mkdir('./DATBOX')

from pylmgc90 import pre

# definition des conteneurs:
#   * de corps
bodies = pre.avatars()
#   * de modeles
mods = pre.models()
#   * de materiaux
mats = pre.materials()
#   * pour les tables de visibilite
svs = pre.see_tables()
#   * pour les lois de contact
tacts = pre.tact_behavs()

# on se place en 3D
dim = 3

# definition d'un modele elastique pour les cubes, mailles en hexaedres
m3Dl = pre.model(name='M3DH8', physics='MECAx', element='H8xxx', dimension=3,
                 external_model='MatL_', kinematic='small', material='elas_',
                 anisotropy='iso__', mass_storage='coher')
# on ajoute le modele dans le conteneur
mods.addModel(m3Dl)

# on definit le materiau constitutif des cubes
stone = pre.material(name='stone', materialType='ELAS',\
                     density=2750.,\
                     elas='standard',anisotropy='isotropic', young=7.e10, nu=0.2)
# on l'ajoute dans le conteneur
mats.addMaterial(stone)

# construction des maillages :

nb_e = 4
L_ = 1.

# on contruit le maillage du cube
mesh_cube=pre.buildMeshH8(x0=0., y0=0., z0=0., lx=L_, ly=L_, lz=L_,\
                          nb_elem_x=nb_e, nb_elem_y=nb_e, nb_elem_z=nb_e)

# on construit un avatar deformable pour le premier cube
cube=pre.buildMeshedAvatar(mesh=mesh_cube, model=m3Dl, material=stone)

## contacteurs :
#   * antagonistes sur la face du haut
cube.addContactors(group='up', shape='ASpxx', color='BLEUx')
cube.imposeDrivenDof(group='down',component=[1,2,3],dofty='vlocy')

# ajout du cube dans le conteneur de corps
bodies += cube

# on copie le maillage pour construire le deuxieme cube
mesh_cube_2 = copy.deepcopy(mesh_cube)

# on construit un avatar deformable pour le deuxieme cube
cube_2=pre.buildMeshedAvatar(mesh=mesh_cube_2, model=m3Dl, material=stone)

# contacteurs :
#   * candidats sur la face du bas

cube_2.addContactors(group='down', shape='CSpxx', color='BLEUx') #,quadrature=0)

# on place le deuxieme cube sur le premier
cube_2.translate(dz=L_)

# ajout du cube dans le conteneur de corps
bodies += cube_2


pre.visuAvatars(bodies)

# gestion des interactions :
#   * declaration des lois
##       - entre cubes
lcsas=pre.tact_behav('gapc0','VEL_SGR_CLB',fric=0.3)
tacts+=lcsas

##   * declaration des tables de visibilite
#       - entre cubes

gdist = 2. * L_/nb_e
ldist = gdist/10.

svcsas = pre.see_table(CorpsCandidat='MAILx', candidat='CSxxx', colorCandidat='BLEUx',\
                       behav=lcsas,\
                       CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='BLEUx',\
                       alert=ldist, halo=gdist)
svs+=svcsas

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)
