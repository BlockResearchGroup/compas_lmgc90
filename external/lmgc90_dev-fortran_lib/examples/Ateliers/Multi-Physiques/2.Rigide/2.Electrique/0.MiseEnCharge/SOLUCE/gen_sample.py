from __future__ import print_function

import numpy
import math

from pylmgc90.pre import *

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
tdur = material(name='TDURx',materialType='RIGID',density=7800.)
plex = material(name='PLEXx',materialType='RIGID',density=7800.)
mat.addMaterial(tdur,plex)

# on cree un modele de rigide
mod = model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

# on genere 1000 particules
nb_particles=1000

# distribtion aleatoire dans [0.5, 2.[ 
radii=granulo_Random(nb_particles, 0.0045, 0.0055)

# on recupere le plus petit et le plus grand rayon
radius_min=min(radii)
radius_max=max(radii)

# depot dans une boite rectangulaire
lx = 0.25
ly = 0.25 
[nb_remaining_particles, coor]=depositInBox2D(radii, lx, ly)

# si toutes les particules deposees n'ont pas ete conservees
if (nb_remaining_particles < nb_particles):
   # on affiche un avertissement
   print("Warning: granulometry changed, since some particles were removed!")

# boucle d'ajout des disques :
for i in range(0,nb_remaining_particles,1):
   # creation un nouveau disque rigide, constitue du materiau plex
   body=rigidDisk(r=radii[i], center=coor[2*i : 2*(i + 1)], 
      model=mod, material=plex, color='BLEUx') 
   # ajout du disque dans le conteneur de corps
   bodies += body

# ajout d'une boite lisse, i.e. faite de joncs :

# on declare un corps par paroi
down=avatar(dimension=dim)
up=avatar(dimension=dim)
left=avatar(dimension=dim)
right=avatar(dimension=dim)

# on attribue un comportement volumique de type rigide aux parois
down.addBulk( rigid2d() )
up.addBulk( rigid2d() )
left.addBulk( rigid2d() )
right.addBulk( rigid2d() )

# on positionne les parois dans l'espace
down.addNode( 
      node(coor=numpy.array([0.5*lx, -radius_max]),
      number=1) )
up.addNode( 
      node(coor=numpy.array([0.5*lx, ly + radius_max]),
      number=1) )
left.addNode( 
      node(coor=numpy.array([-radius_max, 0.5*ly]),
      number=1) )
right.addNode( 
      node(coor=numpy.array([lx + radius_max, 0.5*ly]),
      number=1) )

# on definit les groupes
down.defineGroups()
up.defineGroups()
left.defineGroups()
right.defineGroups()

# on definit le modele pour chaque paroi
down.defineModel(model=mod)
up.defineModel(model=mod)
left.defineModel(model=mod)
right.defineModel(model=mod)

# on definit le materiau pour chaque paroi
down.defineMaterial(material=tdur)
up.defineMaterial(material=tdur)
left.defineMaterial(material=tdur)
right.defineMaterial(material=tdur)

# on affecte un contacteur jonc a chaque paroi
# et on l'affecte aux parois
down.addContactors(shape='JONCx', color='WALLx', axe1=0.5*lx + radius_max, axe2=radius_max)
up.addContactors(shape='JONCx', color='WALLx', axe1=0.5*lx + radius_max, axe2=radius_max)
left.addContactors(shape='JONCx', color='WALLx', axe1=0.5*ly + radius_max, axe2=radius_max)
right.addContactors(shape='JONCx', color='WALLx', axe1=0.5*ly + radius_max, axe2=radius_max)

# on calcule la surface et l'inertie de chaque paroi
down.computeRigidProperties()
up.computeRigidProperties()
left.computeRigidProperties()
right.computeRigidProperties()

# on ajoute les parois a la liste des corps
bodies += down; bodies += up; bodies += left; bodies += right

# on tourne les parois verticales (par rapport a leur propres 
# centre d'inertie)
left.rotate(psi=-math.pi/2., center=left.nodes[1].coor)
right.rotate(psi=math.pi/2., center=right.nodes[1].coor)

# on fixe les parois
down.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
up.imposeDrivenDof(component=[1, 3], dofty='vlocy')
up.imposeDrivenDof(component=[2], dofty='force',ct=-10.,ramp=1000)
left.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
right.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

# gestion des interactions :
#   * declaration des lois
#       - entre particules
ldkdk1=tact_behav(name='iqsc0',law='IQS_CLB',fric=0.0)
tacts+=ldkdk1

ldkdk2=tact_behav(name='iqsc1',law='IQS_CLB',fric=0.0)
tacts+=ldkdk2

#       - avec les parois
ldkjc=tact_behav(name='iqsc2',law='IQS_CLB',fric=0.0)
tacts+=ldkjc

#   * declaration des tables de visibilite
#       - entre particules
svdkdk1 = see_table(CorpsCandidat='RBDY2',candidat='DISKx',
   colorCandidat='BLEUx',behav=ldkdk1, CorpsAntagoniste='RBDY2', 
   antagoniste='DISKx',colorAntagoniste='BLEUx',alert=0.1*radius_min)
svs+=svdkdk1

svdkdk2 = see_table(CorpsCandidat='RBDY2',candidat='DISKx',
   colorCandidat='BLEUx',behav=ldkdk2, CorpsAntagoniste='RBDY2', 
   antagoniste='DISKx',colorAntagoniste='WALLx',alert=0.1*radius_min)
svs+=svdkdk2

#       - avec les parois
svdkjc = see_table(CorpsCandidat='RBDY2',candidat='DISKx',
   colorCandidat='BLEUx',behav=ldkjc, CorpsAntagoniste='RBDY2', 
   antagoniste='JONCx',colorAntagoniste='WALLx',alert=0.1*radius_min)
svs+=svdkjc

# ecriture des fichiers
writeBodies(bodies,chemin='DATBOX/')
writeBulkBehav(mat,chemin='DATBOX/')
writeTactBehav(tacts,svs,chemin='DATBOX/')
writeDrvDof(bodies,chemin='DATBOX/')
writeDofIni(bodies,chemin='DATBOX/')
writeVlocRlocIni(chemin='DATBOX/')
