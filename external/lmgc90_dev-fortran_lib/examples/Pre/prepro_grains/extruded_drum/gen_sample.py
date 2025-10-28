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

# on se place en 3D
dim = 3

# creration des conteneurs
#   * pour les corps extrudes (3D)
bodies = pre.avatars()
#   * pour les corps a extruder (2D)
bodies2D = pre.avatars()
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

# on cree un modele de rigide 2D
modR2D = pre.model(name='rig2d', physics='MECAx', element='Rxx2D', dimension=2)

# on cree un modele de rigide 3D
modR3D = pre.model(name='rig3d', physics='MECAx', element='Rxx3D', dimension=3)
mods.addModel(modR3D)

# on genere 2000 particules
nb_particles=2000

# distribtion aleatoire dans [0.5, 2.[ 
radii = pre.granulo_Random(nb_particles, 0.5, 2., seed)

# on recupere le plus petit et le plus grand rayon
radius_min = numpy.amin(radii)
radius_max = numpy.amax(radii)

# depot dans un demi-tambour
rext = 50.
nb_remaining_particles, coor, radii = pre.depositInDrum2D(radii, rext)

# si toutes les particules deposees n'ont pas ete conservees
if (nb_remaining_particles < nb_particles):
   # on affiche un avertissement
   print("Warning: granulometry changed, since some particles were removed!")

# boucle d'ajout des disques :
for r, c in zip(radii, coor):
   # creation un nouveau disque rigide, constitue du materiau plex
   body = pre.rigidDisk(r=r, center=c, model=modR2D, material=plex, color='BLEUx') 
   # ajout du disque dans le conteneur de corps a extruder
   bodies2D += body

# on construit les particules 3D par extrusion, sur un profondeur egale au
# diametre de la plus grosse particule :
bodies += pre.extrudeRigids(bodies2D=bodies2D, depth=2.*radius_max, model3D=modR3D)

# on interdit le deplacement hors plan des particules
for body in bodies:
   body.imposeDrivenDof(component=2, dofty='vlocy')

# ajout du tambour
drum = pre.rigidDisk(r=rext, center=[rext,rext], model=modR2D, material=tdur, color='BLEUx', is_Hollow=True)
# on extrude le tambour :
drum3D = pre.extrudeRigid(body2D=drum, model3D=modR3D, depth=2.*radius_max)

## on ajoute le tambour a la liste des corps
bodies += drum3D

# conditions aux lmites :
#   * le tambour ne se translate pas
drum3D.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
#   * le tambour tourne a une vitese constante : 1 tour/min, suivant l'axe
#     gamma (autres rotations bloquees)
drum3D.imposeDrivenDof(component=[4, 5],dofty='vlocy')
drum3D.imposeDrivenDof(component=6,dofty='vlocy',ct=numpy.pi/30.,rampi=1.)

try:
  pre.visuAvatars(bodies)
except:
  pass

# gestion des interactions :
#   * declaration des lois
#       - entre particules
lspsp = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts+= lspsp
#       - avec les parois
lspdc = pre.tact_behav(name='iqsc1',law='IQS_CLB',fric=0.5)
tacts+= lspdc
#   * declaration des tables de visibilite
#       - entre particules
svspsp = pre.see_table(CorpsCandidat='RBDY3',candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='SPHER',colorAntagoniste='BLEUx',
                       behav=lspsp, alert=0.1*radius_min)
svs+=svspsp
#       - avec le tambour
svspdc = pre.see_table(CorpsCandidat='RBDY3',candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='DNLYC',colorAntagoniste='BLEUx',
                       behav=lspdc, alert=0.1*radius_min)
svs+= svspdc

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)
