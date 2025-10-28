import os,sys
import copy

if not os.path.isdir('./DATBOX'):
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

# materiau rigide

# creation d'un modele de rigide
mR3D = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=3)

# creation d'un materiau
tdur = pre.material(name='TDURx', materialType='RIGID', density=2500.)
mats.addMaterial(tdur)

# definition d'un modele elastique pour les cubes, mailles en hexaedres 
m3Dl = pre.model(name='M3DH8', physics='MECAx', element='H8xxx', dimension=3, 
                 external_model='MatL_', kinematic='small', material='elas_', 
                 anisotropy='iso__', mass_storage='lump_')
# on ajoute le modele dans le conteneur
mods.addModel(m3Dl)

# on definit le materiau constitutif des cubes
stone = pre.material(name='stone', materialType='ELAS', density=2750., elas='standard',
                     anisotropy='isotropic', young=7.e10, nu=0.2)  
# on l'ajoute dans le contenaur
mats.addMaterial(stone)

# construction des maillages :

# on contruit le maillage du cube
mesh_cube = pre.buildMeshH8(x0=0., y0=0., z0=0., lx=1., ly=1., lz=1., nb_elem_x=2, nb_elem_y=2, nb_elem_z=2)

# on copie le maillage pour construire le deuxieme cube
mesh_cube_2 = copy.deepcopy(mesh_cube)

# on construit un avatar deformable pour le premier cube
cube = pre.buildMeshedAvatar(mesh=mesh_cube, model=m3Dl, material=stone)

## contacteurs :
#   * antagonistes sur la face du haut
cube.addContactors(group='up', shape='ASpxx', color='BLEUx')
cube.imposeDrivenDof(group='down',component=[1, 2, 3], dofty='vlocy')

# ajout du cube dans le conteneur de corps
bodies += cube

# on construit un avatar deformable pour le deuxieme cube
cube_2 = pre.buildMeshedAvatar(mesh=mesh_cube_2, model=m3Dl, material=stone)

# contacteurs :
#   * candidats sur la face du bas

cube_2.addContactors(group='down', shape='CSpxx', color='BLEUx',quadrature=1)

# on place le deuxieme cube sur le premier
cube_2.translate(dz=1.)

# ajout du cube dans le conteneur de corps
bodies += cube_2




# gestion des interactions :
#   * declaration des lois
##       - entre cubes
lcsas = pre.tact_behav('gapc0','GAP_SGR_CLB',fric=0.3)
tacts+= lcsas

##   * declaration des tables de visibilite
#       - entre cubes
svcsas = pre.see_table(CorpsCandidat='MAILx', candidat='CSxxx', colorCandidat='BLEUx',
                       CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='BLEUx',
                       behav=lcsas, alert=0.1, halo=0.5)
svs+= svcsas


post = pre.postpro_commands()

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)
