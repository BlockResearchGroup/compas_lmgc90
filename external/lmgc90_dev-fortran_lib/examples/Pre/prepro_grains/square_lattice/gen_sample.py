import os,sys

import numpy
import math

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

if '--norand' in sys.argv:
  seed = 1
else:
  seed = None

# on se place en 2D
dim = 2

# creration des conteneurs
#   * pour les corps
bodies = pre.avatars()
#   * pour les materiaux
mats = pre.materials()
mods = pre.models()
#   * pour les tables de visibilite
svs = pre.see_tables()
#   * pour les lois de contact
tacts = pre.tact_behavs()

# creations de deux materiaux
tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
plex = pre.material(name='PLEXx',materialType='RIGID',density=100.)
mats.addMaterial(tdur,plex)

# on cree un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mods.addModel(mod)

# initialisation des variables pour un depot sur un reseau carre :

# nombre de particules sur une couche
nb_ele = 20
# nombre de couches
nb_layer = 10
# on en deduit le nombre de particules a generer
nb_particles = nb_ele*nb_layer

# definition de la granulo

# distribtion aleatoire dans [0.5, 2.[ 
radii = pre.granulo_Random(nb_particles, 0.5, 2., seed)

# on recupere le plus petit et le plus grand rayon
radius_min = numpy.amin(radii)
radius_max = numpy.amax(radii)

# definition de la longueur d'un element

# on ne veut pas que les particules se touchent
l = 2.5*radius_max

# on en deduit la taille d'une boite englobante
lx = nb_ele*l
ly = nb_layer*l

# on genere la liste des coordonnees des particules
coor = pre.squareLattice2D(nb_ele, nb_layer, l)

# boucle d'ajout des disques :
for r, c in zip(radii, coor):
   # creation un nouveau disque rigide, constitue du materiau plex
   body = pre.rigidDisk(r=r, center=c, model=mod, material=plex, color='BLEUx') 
   # ajout du disque dans le conteneur de corps
   bodies += body

# ajout d'une boite lisse, i.e. faite de joncs :
down = pre.rigidJonc(axe1=0.5*lx+radius_max, axe2=radius_max, center=[0.5*lx, -radius_max],
                     model=mod, material=tdur, color='WALLx')
up   = pre.rigidJonc(axe1=0.5*lx+radius_max, axe2=radius_max, center=[0.5*lx, ly+radius_max],
                     model=mod, material=tdur, color='WALLx')
left = pre.rigidJonc(axe1=0.5*ly+radius_max, axe2=radius_max, center=[-radius_max, 0.5*ly],
                     model=mod, material=tdur, color='WALLx')
right= pre.rigidJonc(axe1=0.5*ly+radius_max, axe2=radius_max, center=[lx+radius_max, 0.5*ly],
                     model=mod, material=tdur, color='WALLx')

# on ajoute les parois a la liste des corps
bodies += down; bodies += up; bodies += left; bodies += right

# on tourne les parois verticales (par rapport a leur propres 
# centre d'inertie)
left.rotate(psi=-math.pi/2., center=left.nodes[1].coor)
right.rotate(psi=math.pi/2., center=right.nodes[1].coor)

# on fixe les parois
down.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
up.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
left.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
right.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

try:
  pre.visuAvatars(bodies)
except:
  pass

# gestion des interactions :
#   * declaration des lois
#       - entre particules
ldkdk = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts+= ldkdk
#       - avec les parois
ldkjc = pre.tact_behav(name='iqsc1',law='IQS_CLB',fric=0.5)
tacts+= ldkjc
#   * declaration des tables de visibilite
#       - entre particules
svdkdk = pre.see_table(CorpsCandidat='RBDY2',candidat='DISKx', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY2', antagoniste='DISKx',colorAntagoniste='BLEUx',
                       behav=ldkdk, alert=0.1*radius_min)
svs+=svdkdk
#       - avec les parois
svdkjc = pre.see_table(CorpsCandidat='RBDY2',candidat='DISKx', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY2', antagoniste='JONCx',colorAntagoniste='WALLx',
                       behav=ldkjc, alert=0.1*radius_min)
svs+=svdkjc

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)
