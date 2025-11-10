import os

import numpy
from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# containers definition
bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

# 3D
dim = 3

# rigid model
mR3D = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=3)

# rigid material
tdur = pre.material(name='TDURx', materialType='RIGID', density=2500.)
mats.addMaterial(tdur)

# elastic model for all 
m3Dl1 = pre.model(name='M3DTL', physics='MECAx', element='TE4xx', dimension=3,
                  external_model='MatL_', kinematic='small', material='elas_',
                  anisotropy='iso__', mass_storage='lump_')
mods.addModel(m3Dl1)

m3Dl2 = pre.model(name='M3DTQ', physics='MECAx', element='TE10x', dimension=3,
                  external_model='MatL_', kinematic='small', material='elas_',
                  anisotropy='iso__', mass_storage='lump_')
mods.addModel(m3Dl2)

m3Dl3 = pre.model(name='M3DHL', physics='MECAx', element='H8xxx', dimension=3,
                  external_model='MatL_', kinematic='small', material='elas_',
                  anisotropy='iso__', mass_storage='lump_')
mods.addModel(m3Dl3)

m3Dl4 = pre.model(name='M3DHQ', physics='MECAx', element='H20xx', dimension=3,
                  external_model='MatL_', kinematic='small', material='elas_',
                  anisotropy='iso__', mass_storage='lump_')
mods.addModel(m3Dl4)

m3Dl5 = pre.model(name='M3DPL', physics='MECAx', element='PRI6x', dimension=3,
                  external_model='MatL_', kinematic='small', material='elas_',
                  anisotropy='iso__', mass_storage='lump_')
mods.addModel(m3Dl5)

m3Dl6 = pre.model(name='M3DPQ', physics='MECAx', element='PRI15', dimension=3,
                  external_model='MatL_', kinematic='small', material='elas_',
                  anisotropy='iso__', mass_storage='lump_')
mods.addModel(m3Dl6)

# elastic material
stone = pre.material(name='stone', materialType='ELAS', density=2750., elas='standard',
                     anisotropy='isotropic', young=7.e10, nu=0.2)
mats.addMaterial(stone)

# mesh reading:
mesh_names = {'cube_te4'  :[m3Dl1,0],
              'cube_h8'   :[m3Dl3,1],
              'cube_te10' :[m3Dl2,2],
              'cube_h20'  :[m3Dl4,3],
              'cube_pri6' :[m3Dl5,4],
              'cube_pri15':[m3Dl6,5],
             }

for n, m in list(mesh_names.items()):
  mesh_cube = pre.readMesh('mesh/'+n+'.vtu' , dim)
  cube      = pre.buildMeshedAvatar(mesh=mesh_cube, model=m[0], material=stone)
  cube.translate(dz=m[1]*1.1)
  bodies += cube

# construction d'un polyedre rigide pour la fondation
#   * coordonnees des sommets
vertices = numpy.zeros([8, 3], 'd')
vertices[0, 0] = 0. ; vertices[0, 1] = 0. ; vertices[0, 2] = 0.
vertices[1, 0] = 1.5; vertices[1, 1] = 0. ; vertices[1, 2] = 0.
vertices[2, 0] = 1.5; vertices[2, 1] = 1.5; vertices[2, 2] = 0.
vertices[3, 0] = 0. ; vertices[3, 1] = 1.5; vertices[3, 2] = 0.
vertices[4, 0] = 0. ; vertices[4, 1] = 0. ; vertices[4, 2] = 0.1
vertices[5, 0] = 1.5; vertices[5, 1] = 0. ; vertices[5, 2] = 0.1
vertices[6, 0] = 1.5; vertices[6, 1] = 1.5; vertices[6, 2] = 0.1
vertices[7, 0] = 0. ; vertices[7, 1] = 1.5; vertices[7, 2] = 0.1
#   * connectivite des faces triangulaires
connectivity = numpy.zeros([12, 3], 'i')
connectivity[ 0, 0] = 1; connectivity[ 0, 1] = 2; connectivity[ 0, 2] = 3
connectivity[ 1, 0] = 1; connectivity[ 1, 1] = 3; connectivity[ 1, 2] = 4
connectivity[ 2, 0] = 1; connectivity[ 2, 1] = 2; connectivity[ 2, 2] = 6
connectivity[ 3, 0] = 1; connectivity[ 3, 1] = 6; connectivity[ 3, 2] = 5
connectivity[ 4, 0] = 2; connectivity[ 4, 1] = 3; connectivity[ 4, 2] = 7
connectivity[ 5, 0] = 2; connectivity[ 5, 1] = 7; connectivity[ 5, 2] = 6
connectivity[ 6, 0] = 1; connectivity[ 6, 1] = 4; connectivity[ 6, 2] = 8
connectivity[ 7, 0] = 1; connectivity[ 7, 1] = 8; connectivity[ 7, 2] = 5
connectivity[ 8, 0] = 3; connectivity[ 8, 1] = 4; connectivity[ 8, 2] = 8
connectivity[ 9, 0] = 3; connectivity[ 9, 1] = 8; connectivity[ 9, 2] = 7
connectivity[10, 0] = 5; connectivity[10, 1] = 7; connectivity[10, 2] = 8
connectivity[11, 0] = 5; connectivity[11, 1] = 6; connectivity[11, 2] = 7

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
lcsas  = pre.tact_behav('gapc0','GAP_SGR_CLB',fric=0.3)
tacts += lcsas
lcspr  = pre.tact_behav('gapc1', 'GAP_SGR_CLB', fric=0.5)
tacts += lcspr
#   * declaration des tables de visibilite
svcsas = pre.see_table(CorpsCandidat='MAILx', candidat='CSxxx', colorCandidat='BLEUx',
                       CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='BLEUx',
                       behav=lcsas, alert=0.1, halo=0.5)
svs += svcsas
svcspr = pre.see_table(CorpsCandidat='MAILx', candidat='CSxxx', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='FLOOR',
                       behav=lcspr, alert=0.1, halo=1.5)
svs += svcspr

post = pre.postpro_commands()

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

try:
  pre.visuAvatars(bodies, True)
except:
  pass
