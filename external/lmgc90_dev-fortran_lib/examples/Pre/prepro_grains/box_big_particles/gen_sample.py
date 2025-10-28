import sys
from pathlib import Path

import numpy

from pylmgc90 import pre

datbox_path = Path('DATBOX')
datbox_path.mkdir(exist_ok=True)

# on se place en 2D
dim = 2

if '--norand' in sys.argv:
  seed = 1
else:
  seed = None

# creration des conteneurs
#   * pour les corps
bodies = pre.avatars()
#   * pour les materiaux
mats   = pre.materials()
mods   = pre.models()
#   * pour les tables de visibilite
svs    = pre.see_tables()
#   * pour les lois de contact
tacts  = pre.tact_behavs()

# creation de trois materiaux :
#   * un pour les petites particules
plex = pre.material(name='PLEXx', materialType='RIGID', density=100.)
mats.addMaterial(plex)
#   * un pour les grosses particules
glass = pre.material(name='GLASS', materialType='RIGID', density=1000.)
mats.addMaterial(glass)
#   * un pour les parois
tdur = pre.material(name='TDURx', materialType='RIGID', density=10000.)
mats.addMaterial(tdur)

# on cree un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mods.addModel(mod)

# on genere 1000 particules
nb_particles=1000

# distribtion aleatoire dans [0.5, 2.[ 
radii= pre.granulo_Random(nb_particles, 0.5, 2., seed)

# on recupere le plus petit et le plus grand rayon
radius_min = numpy.amin(radii)
radius_max = numpy.amax(radii)

# depot dans une boite rectangulaire
lx = 75.
ly = 50.

# on depose deux premieres particules
deposited_radii=[4., 4.]
deposited_coor=[[25., 25.], [50., 25.]]

# on cree les corps correpsondant

# creation d'un nouveau disque rigide, pour la premiere grosse particule
body = pre.rigidDisk(r=deposited_radii[0], center=deposited_coor[0], 
                     model=mod, material=glass, color='BLEUx')
# ajout du disque dans le conteneur de corps
bodies += body

# creation d'un nouveau disque rigide, pour la deuxieme grosse particule
body = pre.rigidDisk(r=deposited_radii[1], center=deposited_coor[1], 
                     model=mod, material=glass, color='BLEUx')
# ajout du disque dans le conteneur de corps
bodies += body

# on depose les particules sous gravite, dans la boite 
nb_remaining_particles, coor, radii = pre.depositInBox2D(radii, lx, ly,
   d_radii=deposited_radii, d_coor=deposited_coor)

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
left.rotate(psi=-numpy.pi/2., center=left.nodes[1].coor)
right.rotate(psi=numpy.pi/2., center=right.nodes[1].coor)

# on fixe les parois
down.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
up.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
left.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
right.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

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
                       behav=ldkdk,alert=0.1*radius_min)
svs+= svdkdk
#       - avec les parois
svdkjc = pre.see_table(CorpsCandidat='RBDY2',candidat='DISKx', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY2', antagoniste='JONCx',colorAntagoniste='WALLx',
                       behav=ldkjc,alert=0.1*radius_min)
svs+=svdkjc

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)

try:
  pre.visuAvatars(bodies)
except:
  pass
