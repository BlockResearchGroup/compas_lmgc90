from __future__ import print_function
import os,sys,copy
from pylmgc90.pre import *

if not os.path.isdir('./output'):
  os.mkdir('./output')

rep = './output/DATBOX'
if not os.path.isdir(rep):
  os.mkdir(rep)

# definition des conteneurs:
#   * de corps
bodies = avatars()
#   * de materiaux
mats = materials()
##   * pour les tables de visibilite
svs = see_tables()
##   * pour les lois de contact
tacts = tact_behavs()

# on se place en 3D
dim = 3

# materiau rigide

# creation d'un materiau
tdur = material(name='TDURx', materialType='RIGID', density=2700.)
pdur = material(name='TDURx', materialType='RIGID', density=2300.)
mats.addMaterial(tdur,pdur)

# creation d'un modele de rigide
mod = model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)

for i in range(1,9):
  # on lit le maillage total dans un fichier, au format gmsh
  mesh = readMesh(name='./bloc'+str(i)+'.msh', dim=dim)
  print('-')
  body = surfacicMeshToRigid3D(surfacic_mesh=mesh, model=mod, material=tdur, color='VERTx')
  body.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
  print('--')
  # on l objet au conteneur de corps
  bodies.addAvatar(body)

### particles ###
box_height = 5.
box_length = 2.
box_width  = 2.

sizeofparticles_min=0.05
sizeofparticles_max=0.15
nparticles= 500

# random distribution  of radii
radii=granulo_Random(nparticles, sizeofparticles_min, sizeofparticles_max)
radius_min=min(radii)
radius_max=max(radii)

[nb_remaining_particles, coor_particles]=depositInBox3D(radii, box_length, box_width, box_height)

if (nb_remaining_particles < nparticles):
   print("Warning: granulometry changed, since some particles were removed!")
   print("nparticles is now : nb_remaining_particles ")
   nparticles= nb_remaining_particles

coor_particles.resize(nb_remaining_particles*dim)
coor_particles.shape=[nparticles,dim]

x0 = 1.8
y0 = 1.5
z0 = 3.

for i in range(nparticles):
  coor_particles[i,0] = coor_particles[i,0]+x0
  coor_particles[i,1] = coor_particles[i,1]+y0
  coor_particles[i,2] = coor_particles[i,2]+z0

for i in range(nparticles):
  print(("Creation particules number", i))
  body=rigidPolyhedron(radius=radii[i],center=coor_particles[i,:],
                                   model=mod,material=pdur,color='BLEUx',
                                   nb_vertices=7,generation_type='random')
  bodies += body


# definitions des interactions
#  * loi d'interaction :
lprpr=tact_behav('iqsc0', 'IQS_CLB', fric=0.3)
tacts+=lprpr

lprpl=tact_behav('iqsc1', 'IQS_CLB', fric=0.3)
tacts+=lprpl

#  * table de visibilite :
svprpr = see_table(CorpsCandidat='RBDY3', candidat='POLYR', 
   colorCandidat='BLEUx', behav=lprpr, CorpsAntagoniste='RBDY3',
   antagoniste='POLYR', colorAntagoniste='BLEUx', alert=5.e-2)
svs+=svprpr

svprpl = see_table(CorpsCandidat='RBDY3', candidat='POLYR', 
   colorCandidat='BLEUx', behav=lprpl, CorpsAntagoniste='RBDY3',
   antagoniste='POLYR', colorAntagoniste='VERTx', alert=0.05, halo=0.15)
svs+=svprpl


if 0 : 
  # ecriture des fichiers de donnees
  writeBodies(bodies, chemin=rep)
  writeBulkBehav(mats, chemin=rep, dim=dim)
  writeDrvDof(bodies, chemin=rep)
  writeDofIni(bodies, chemin=rep)
  writeTactBehav(tacts, svs, chemin=rep)
  writeVlocRlocIni(chemin=rep)

  post = postpro_commands()
  writePostpro(commands=post, parts=bodies, path=rep)

try:
  visuAvatars(bodies)
except:
  pass



