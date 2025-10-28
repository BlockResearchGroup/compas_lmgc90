import os
import copy

import numpy

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# definition des conteneurs:
bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

# on se place en 3D
dim = 3

# creation d'un modele de rigide
mR3D = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=3)

# creation d'un materiau
tdur = pre.material(name='TDURx', materialType='RIGID', density=2500.)
mats.addMaterial(tdur)

# definition d'un modele elastique pour les cubes, mailles en tetraedres 
m3Dl = pre.model(name='M3DT4', physics='MECAx', element='TE4xx', dimension=3, 
                 external_model='MatL_', kinematic='small', material='elas_', 
                 anisotropy='iso__', mass_storage='lump_')
mods.addModel(m3Dl)

# on definit le materiau constitutif des cubes
stone = pre.material(name='stone', materialType='ELAS', density=2750., elas='standard',
                     anisotropy='isotropic', young=7.e10, nu=0.2)  
mats.addMaterial(stone)

# condtruction des maillages :
mesh_cube   = pre.readMesh('gmsh/cube_t4.msh', dim)
mesh_cube_2 = copy.deepcopy(mesh_cube)

cube = pre.buildMeshedAvatar(mesh=mesh_cube, model=m3Dl, material=stone)
# contacteurs :
#   * antagonistes sur la face du haut
cube.addContactors(group='102', shape='ASpxx', color='BLEUx')
#   * candidats sur la face du bas
cube.addContactors(group='105', shape='CSpxx', color='BLEUx',quadrature=0)

bodies += cube

cube_2 = pre.buildMeshedAvatar(mesh=mesh_cube_2, model=m3Dl, material=stone)
# contacteurs :
#   * candidats sur la face du bas
cube_2.addContactors(group='105', shape='CSpxx', color='BLEUx',quadrature=0)

cube_2.translate(dz=1.)
bodies += cube_2

# construction d'un polyedre rigide pour la fondation
#   * coordonnees des sommets
vertices=numpy.zeros([8, 3], 'd')
vertices[0, 0]=0. ; vertices[0, 1]=0. ; vertices[0, 2]=0.   
vertices[1, 0]=1.5; vertices[1, 1]=0. ; vertices[1, 2]=0.
vertices[2, 0]=1.5; vertices[2, 1]=1.5; vertices[2, 2]=0.
vertices[3, 0]=0. ; vertices[3, 1]=1.5; vertices[3, 2]=0.
vertices[4, 0]=0. ; vertices[4, 1]=0. ; vertices[4, 2]=0.1
vertices[5, 0]=1.5; vertices[5, 1]=0. ; vertices[5, 2]=0.1
vertices[6, 0]=1.5; vertices[6, 1]=1.5; vertices[6, 2]=0.1
vertices[7, 0]=0. ; vertices[7, 1]=1.5; vertices[7, 2]=0.1
#   * connectivite des faces triangulaires
connectivity=numpy.zeros([12, 3], 'i')
connectivity[ 0, 0]=1; connectivity[ 0, 1]=2; connectivity[ 0, 2]=3
connectivity[ 1, 0]=1; connectivity[ 1, 1]=3; connectivity[ 1, 2]=4
connectivity[ 2, 0]=1; connectivity[ 2, 1]=2; connectivity[ 2, 2]=6
connectivity[ 3, 0]=1; connectivity[ 3, 1]=6; connectivity[ 3, 2]=5
connectivity[ 4, 0]=2; connectivity[ 4, 1]=3; connectivity[ 4, 2]=7
connectivity[ 5, 0]=2; connectivity[ 5, 1]=7; connectivity[ 5, 2]=6
connectivity[ 6, 0]=1; connectivity[ 6, 1]=4; connectivity[ 6, 2]=8
connectivity[ 7, 0]=1; connectivity[ 7, 1]=8; connectivity[ 7, 2]=5
connectivity[ 8, 0]=3; connectivity[ 8, 1]=4; connectivity[ 8, 2]=8
connectivity[ 9, 0]=3; connectivity[ 9, 1]=8; connectivity[ 9, 2]=7
connectivity[10, 0]=5; connectivity[10, 1]=7; connectivity[10, 2]=8
connectivity[11, 0]=5; connectivity[11, 1]=6; connectivity[11, 2]=7

floor = pre.rigidPolyhedron(model=mR3D,material=tdur,color='FLOOR',generation_type='full',
                            vertices=vertices,faces=connectivity)

# on place la fondation sous le premier cube
floor.translate(dx=-0.25, dy=-0.25, dz=-0.1)

# on bloque la fondation
floor.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')

# ajout de la fondation dans le conteneur de corps
bodies.addAvatar(floor)

# gestion des interactions :
#   * declaration des lois
lcsas = pre.tact_behav('gapc0','GAP_SGR_CLB',fric=0.3)
tacts+= lcsas
lcspr = pre.tact_behav('gapc1', 'GAP_SGR_CLB', fric=0.5)
tacts+=lcspr
#   * declaration des tables de visibilite
svcsas = pre.see_table(CorpsCandidat='MAILx', candidat='CSxxx', colorCandidat='BLEUx',
                       CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='BLEUx',
                       behav=lcsas, alert=0.1, halo=0.5)
svs += svcsas
svcspr = pre.see_table(CorpsCandidat='MAILx', candidat='CSxxx', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='FLOOR',
                       behav=lcspr, alert=0.1, halo=1.5)
svs+=svcspr


post = pre.postpro_commands()

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass
