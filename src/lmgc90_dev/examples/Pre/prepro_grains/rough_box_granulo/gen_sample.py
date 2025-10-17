import sys
from pathlib import Path

import numpy

from pylmgc90 import pre

datbox_path = Path('DATBOX')
datbox_path.mkdir(exist_ok=True)

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

# on genere 1000 particules
nb_particles=1000

# distribtion aleatoire dans [0.5, 2.[ 
radii = pre.granulo_Random(nb_particles, 0.5, 2., seed)

# on recupere le plus petit et le plus grand rayon
radius_min = numpy.amin(radii)
radius_max = numpy.amax(radii)

# depot dans une boite rectangulaire
lx = 75.
ly = 50. 
nb_remaining_particles, coor, radii = pre.depositInBox2D(radii, lx, ly)

# si toutes les particules deposees n'ont pas ete conservees
if (nb_remaining_particles < nb_particles):
   # on affiche un avertissement
   print("Warning: granulometry changed, since some particles were removed!")

# boucle d'ajout des disques :
for r, c in zip(radii, coor):
   # creation un nouveau disque rigide, constitue du materiau plex
   body = pre.rigidDisk(r=r, center=c, model=mod, material=plex, color='BLEUx') 
   # ajout du disque dans le conteneur de corps
   bodies += body

# ajout d'une boite rugueuse, i.e. faite de cluster de disques rigides :

# creation des quatres parois rugueuses, avec le materiau tdur
down = pre.granuloRoughWall(center=[0.5*lx, -radius_max], theta=0., l=lx + 2.*radius_max, 
        rmin=radius_min, rmax=radius_max, model=mod, material=tdur, color='WALLx')
up = pre.granuloRoughWall(center=[0.5*lx, ly + radius_max], theta=numpy.pi, l=lx + 2.*radius_max, 
        rmin=radius_min, rmax=radius_max, model=mod, material=tdur, color='WALLx')
left = pre.granuloRoughWall(center=[-radius_max, 0.5*ly], theta=-0.5*numpy.pi, l=ly + 2.*radius_max, 
        rmin=radius_min, rmax=radius_max, model=mod, material=tdur, color='WALLx')
right = pre.granuloRoughWall(center=[lx + radius_max, 0.5*ly], theta=0.5*numpy.pi, l=ly + 2.*radius_max, 
        rmin=radius_min, rmax=radius_max, model=mod, material=tdur, color='WALLx')

# on ajoute les parois a la liste des corps
bodies += down; bodies += up; bodies += left; bodies += right

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
# ATTENTION : meme si le contacteur s'appelle DISKb dans le BODIES.DAT, il doit
#             etre declare DISKx dans la table de vidibilite
svdkdkb = pre.see_table(CorpsCandidat='RBDY2',candidat='DISKx', colorCandidat='BLEUx',
                        CorpsAntagoniste='RBDY2', antagoniste='DISKx',colorAntagoniste='WALLx',
                        behav=ldkjc, alert=0.1*radius_min)
svs+=svdkdkb

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)
