import sys
import itertools
from pathlib import Path

import numpy as np
import math

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
bodies2 = pre.avatars()
#   * pour les materiaux
mats = pre.materials()
mods = pre.models()
#   * pour les tables de visibilite
svs = pre.see_tables()
#   * pour les lois de contact
tacts = pre.tact_behavs()

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

# initialisation des variables pour un depot sur un reseau cubique :

# on fixe le nombre de particules a generer
nb_particles = 1000

# definition de la granulo

# distribution aleatoire dans [0.5, 2.[ 
radii= pre.granulo_Random(nb_particles, 0.5, 2., seed)

# on recupere le plus petit et le plus grand rayon
radius_min=np.amin(radii)
radius_max=np.amax(radii)
# on depose les particules sous gravite, dans une boite 
lx = 15.
ly = 15.
lz = 10.
nb_comp_particles, coor, radii = pre.depositInBox3D(radii, lx, ly, lz, seed=seed)

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


# boucle d'ajout des spheres de la paroi inf :
X = np.arange(0., lx, radius_min)
Y = np.arange(0., ly, radius_min)
for x, y in itertools.product(X,Y):
    # creation un nouveau disque rigide, constitue du materiau plex
    body2=pre.rigidSphere(r=radius_min, center=[x,y,-radius_min],
                          model=mod, material=tdur, color='VERTx')
    body2.imposeDrivenDof(component=[1,2,3,4,5,6], dofty='vlocy')
    # ajout du disque dans le conteneur de corps
    bodies2 += body2

bodies+=bodies2

#### definition de parois rugueuses : pas utilse sous cette forme pour la DDM
##top=roughWall3D(center=[0., 0., lz+radius_min], r=radius_min, lx=lx + 2*radius_min, ly=ly + 2*radius_min, model=mod, material=tdur, color='VERTx')
##
### blocage des parois
##top.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
##
### ajouts de parois au conteneur de corps
##bodies.addAvatar(top)

# gestion des interactions :
#   * declaration des lois
#       - entre particules
lspsp=pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.3)
tacts+=lspsp
#       - avec les parois
lsppl=pre.tact_behav(name='iqsc1', law='IQS_CLB', fric=0.5)
tacts+=lsppl
#   * declaration des tables de visibilite
#       - entre particules
svspsp = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='SPHER', colorAntagoniste='BLEUx',
                       behav=lspsp, alert=0.1*radius_min)
svs+=svspsp
#       - avec les parois
svsppl = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='SPHER', colorAntagoniste='VERTx',
                       behav=lsppl, alert=0.1*radius_min)
svs+=svsppl

post=pre.postpro_commands()

list_tracking = []
for i in range(1,8):
  list_tracking.append(bodies[i])

solv=pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(solv)
viol=pre.postpro_command(name='VIOLATION EVOLUTION', step=1)
post.addCommand(viol)
trac=pre.postpro_command(name='BODY TRACKING', step=1, rigid_set=list_tracking)
post.addCommand(trac)

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass
