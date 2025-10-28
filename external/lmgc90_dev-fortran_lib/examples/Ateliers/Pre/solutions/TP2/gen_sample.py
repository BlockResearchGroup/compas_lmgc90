import numpy
import math

from pylmgc90.pre import *

import os
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# on se place en 2D
dim = 2

# creration des conteneurs
#   * pour les corps
bodies = avatars()
#   * pour les materiaux
mat = materials()
#   * pour les tables de visibilite
svs = see_tables()
#   * pour les lois de contact
tacts = tact_behavs()

# creations de deux materiaux
tdur = material(name='TDURx',materialType='RIGID',density=1000.)
plex = material(name='PLEXx',materialType='RIGID',density=100.)
mat.addMaterial(tdur,plex)

# on cree un modele de rigide
mod = model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

# initialisation des variables pour un depot sur un reseau triangulaire,
# avec la premiere couche de triangles oriente vers le bas :

# nombre de particules sur une couche
nb_ele = 32
# nombre de couches
nb_layer = 32
# on en deduit le nombre de particules a generer
nb_particles = nbPointsInTriangularLattice2D(nb_ele, nb_layer, 'down') 

# definition de la granulo

# distribtion uiforme dans [0.01, 0.015[ 
radii=granulo_Uniform(nb_particles, 0.01, 0.015)

# on recupere le plus petit et le plus grand rayon
radius_min=min(radii)
radius_max=max(radii)

# definition de la longueur d'un element

# on ne veut pas que les particules se touchent
l = 2.5*radius_max

# on en deduit la taille d'une boite englobante
[lx, ly]=sizeBoundingBoxTriangularLattice2D(nb_ele, nb_layer, l, 'down')

# on genere la liste des coordonnees des particules
coor=triangularLattice2D(nb_ele, nb_layer, l, orientation='down')

# boucle d'ajout des disques :
for i in range(nb_particles):
   # creation un nouveau disque rigide, constitue du materiau plex
   body=rigidDisk(r=radii[i], center=coor[2*i : 2*(i + 1)], 
      model=mod, material=plex, color='BLEUx') 
   # ajout du disque dans le conteneur de corps
   bodies.addAvatar(body)

# ajout d'une boite lisse, i.e. faite de joncs :

# on declare un corps par paroi
hopper_left=avatar(dimension=dim)
hopper_right=avatar(dimension=dim)
silo_left=avatar(dimension=dim)
silo_right=avatar(dimension=dim)

# on attribue un comportement volumique de type rigide aux parois
hopper_left.addBulk( rigid2d() )
hopper_right.addBulk( rigid2d() )
silo_left.addBulk( rigid2d() )
silo_right.addBulk( rigid2d() )

# on positionne les parois dans l'espace
hopper_left.addNode( 
      node(coor=numpy.array([0., 0.]),
      number=1) )
hopper_right.addNode( 
      node(coor=numpy.array([0., 0.]),
      number=1) )
silo_left.addNode( 
      node(coor=numpy.array([-radius_max, 0.5*ly]),
      number=1) )
silo_right.addNode( 
      node(coor=numpy.array([lx + radius_max, 0.5*ly]),
      number=1) )

# on definit les groupes
hopper_left.defineGroups()
hopper_right.defineGroups()
silo_left.defineGroups()
silo_right.defineGroups()

# on definit le modele pour chaque paroi
hopper_left.defineModel(model=mod)
hopper_right.defineModel(model=mod)
silo_left.defineModel(model=mod)
silo_right.defineModel(model=mod)

# on definit le materiau pour chaque paroi
hopper_left.defineMaterial(material=tdur)
hopper_right.defineMaterial(material=tdur)
silo_left.defineMaterial(material=tdur)
silo_right.defineMaterial(material=tdur)

# on affecte un contacteur jonc a chaque paroi
# et on l'affecte aux parois
hopper_left.addContactors(shape='JONCx', color='WALLx', axe1=0.5*lx + radius_max, axe2=radius_max)
hopper_right.addContactors(shape='JONCx', color='WALLx', axe1=0.5*lx + radius_max, axe2=radius_max)
silo_left.addContactors(shape='JONCx', color='WALLx', axe1=0.5*ly + radius_max, axe2=radius_max)
silo_right.addContactors(shape='JONCx', color='WALLx', axe1=0.5*ly + radius_max, axe2=radius_max)

# on calcule la surface et l'inertie de chaque paroi
hopper_left.computeRigidProperties()
hopper_right.computeRigidProperties()
silo_left.computeRigidProperties()
silo_right.computeRigidProperties()

# on tourne les parois verticales (par rapport a leur propres 
# centre d'inertie)
silo_left.rotate(psi=-math.pi/2., center=silo_left.nodes[1].coor)
silo_right.rotate(psi=math.pi/2., center=silo_right.nodes[1].coor)

# positionnement de la tremie
#   * translation du centre d'inertie
hopper_left.translate(dx=-radius_max, dy=-radius_max)
hopper_right.translate(dx=lx + radius_max, dy=-radius_max)

#   * rotation autour du centre d'inertie
hopper_left.rotate(psi=-math.pi/6., center=hopper_left.nodes[1].coor)
hopper_right.rotate(psi=math.pi/6., center=hopper_right.nodes[1].coor)

# on ajoute les parois a la liste des corps
bodies.addAvatar(hopper_left)
bodies.addAvatar(hopper_right)
bodies.addAvatar(silo_left)
bodies.addAvatar(silo_right)

# on fixe les parois
hopper_left.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
hopper_right.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
silo_left.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
silo_right.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

# gestion des interactions :
#   * declaration des lois
#       - entre particules
ldkdk=tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts.addBehav(ldkdk)
#       - avec les parois
ldkjc=tact_behav(name='iqsc1',law='IQS_CLB',fric=0.5)
tacts.addBehav(ldkjc)
#   * declaration des tables de visibilite
#       - entre particules
svdkdk = see_table(CorpsCandidat='RBDY2',candidat='DISKx',
   colorCandidat='BLEUx',behav=ldkdk, CorpsAntagoniste='RBDY2', 
   antagoniste='DISKx',colorAntagoniste='BLEUx',alert=0.1*radius_min)
svs.addSeeTable(svdkdk)
#       - avec les parois
svdkjc = see_table(CorpsCandidat='RBDY2',candidat='DISKx',
   colorCandidat='BLEUx',behav=ldkdk, CorpsAntagoniste='RBDY2', 
   antagoniste='JONCx',colorAntagoniste='WALLx',alert=0.1*radius_min)
svs.addSeeTable(svdkjc)

# ecriture des fichiers
writeBodies(bodies,chemin='DATBOX/')
writeBulkBehav(mat,chemin='DATBOX/')
writeTactBehav(tacts,svs,chemin='DATBOX/')
writeDrvDof(bodies,chemin='DATBOX/')
writeDofIni(bodies,chemin='DATBOX/')
writeVlocRlocIni(chemin='DATBOX/')

try:
  visuAvatars(bodies)
except:
  pass
