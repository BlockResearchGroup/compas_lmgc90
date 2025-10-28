import sys
from pathlib import Path

import numpy

from pylmgc90 import pre

datbox_path = Path('DATBOX')
datbox_path.mkdir(exist_ok=True)

if '--norand' in sys.argv:
  seed = list(range(33))
else:
  seed = None

# on se place en 3D
dim = 3

# creation des conteneurs
#   * pour les corps
bodies = pre.avatars()
#   * pour les materiaux
mats   = pre.materials()
mods   = pre.models()
#   * pour les tables de visibilite
svs    = pre.see_tables()
#   * pour les lois de contact
tacts  = pre.tact_behavs()

# creation de deux materiaux :
#   * un pour les spheres
plex = pre.material(name='PLEXx', materialType='RIGID', density=100.)
mats.addMaterial(plex)
#   * un pour les parois
tdur = pre.material(name='TDURx', materialType='RIGID', density=1000.)
mats.addMaterial(tdur)

# creation d'un modele rigide 3D
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod)

# initialisation des variables pour un depot dans un cylindre :

# on fixe le nombre de particules a generer
nb_particles = 1000

# definition de la granulo

# distribution aleatoire dans [0.5, 2.[ 
radii = pre.granulo_Random(nb_particles, 0.5, 2., seed)

# on recupere le plus petit et le plus grand rayon
radius_min = numpy.amin(radii)
radius_max = numpy.amax(radii)
# on depose les particules sous gravite, dans un cylindre 
R=7.5 # rayon
lz=10. # demie hauteur
nb_comp_particles, coor, radii = pre.depositInCylinder3D(radii, R, lz, seed=seed)

# si toutes les particules deposees n'ont pas ete deposees
if (nb_comp_particles < nb_particles):
   # on affiche un avertissement
   print("Warning: granulometry changed, since some particles cannot be deposited!")

# boucle d'ajout des spheres :
for r, c in zip(radii, coor):
   # creation d'une nouvelle sphere rigide
   body = pre.rigidSphere(r=r, center=c, model=mod, material=plex, color='BLEUx')
   # ajout de la sphere dans le conteneur de corps
   bodies += body

# creation des parois :
#   * le fond du cylindre:
down = pre.rigidPlan(axe1=R, axe2=R, axe3=radius_min, center=[0., 0., -radius_min],
                     model=mod, material=tdur, color='VERTx')
bodies.addAvatar(down)

#   * le cylindre lui-meme:
cylinder = pre.rigidCylinder(r=R, h=lz, center=[0., 0., 0.5*lz],
                             model=mod, material=tdur, color='VERTx', is_Hollow=True)
bodies.addAvatar(cylinder)

# blocage des parois
#   * le fond
down.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
#   * le cylindre
cylinder.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')

try:
  pre.visuAvatars(bodies)
except:
  pass

# gestion des interactions :
#   * declaration des lois
#       - entre particules
lspsp = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts+= lspsp
#       - avec le parois
lsppl = pre.tact_behav(name='iqsc1', law='IQS_CLB', fric=0.5)
tacts+= lsppl
#   * declaration des tables de visibilite
#       - entre particules
svspsp = pre.see_table(CorpsCandidat='RBDY3',candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='SPHER',colorAntagoniste='BLEUx',
                       behav=lspsp, alert=0.1*radius_min)
svs+=svspsp
#       - avec les parois:
#           * le fond:
svsppl = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='VERTx',
                       behav=lsppl, alert=0.1*radius_min)
svs+=svsppl
#           * le cylindre:
svspdc = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='DNLYC', colorAntagoniste='VERTx',
                       behav=lsppl, alert=radius_min)
svs+=svspdc

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)
