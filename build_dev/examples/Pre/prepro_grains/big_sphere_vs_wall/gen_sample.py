import sys
from pathlib import Path

import numpy as np

from pylmgc90 import pre

datbox_path = Path('DATBOX')
datbox_path.mkdir(exist_ok=True)

if '--norand' in sys.argv:
  seed1 = 3
  seed2 = list(range(12,45))
else:
  seed1 = None
  seed2 = None

# on se place en 3D
dim = 3

# creation des conteneurs
#   * pour les corps
bodies = pre.avatars()
#   * pour les materiaux
mats = pre.materials()
mods = pre.models()
#   * pour les tables de visibilite
svs = pre.see_tables()
#   * pour les lois de contact
tacts = pre.tact_behavs()

# creation de trois materiaux :
#   * un pour les petites spheres
plex = pre.material(name='PLEXx', materialType='RIGID', density=100.)
mats.addMaterial(plex)
#   * un pour la grosse sphere
glass = pre.material(name='GLASS', materialType='RIGID', density=1000.)
mats.addMaterial(glass)
#   * un pour les parois
tdur = pre.material(name='TDURx', materialType='RIGID', density=10000.)
mats.addMaterial(tdur)

# creation d'un modele rigide 3D
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod)

# initialisation des variables pour un depot dans une sphere :
R=15. # rayon de la sphere
center=np.zeros(3, 'd') # [0., 0., 0.] # centre de la sphere

# on fixe le nombre de particules a generer
nb_particles = 1000

# definition de la granulo

# distribtion aleatoire dans [0.5, 2.[ 
radii = pre.granulo_Random(nb_particles, 0.5, 2., seed1)

# on recupere le plus petit et le plus grand rayon
radius_min = np.amin(radii)
radius_max = np.amax(radii)

# on depose un premiere particule
deposited_radii=[2.5]

# on cree le corps correpsondant

# creation d'une nouvelle sphere rigide
body = pre.rigidSphere(r=deposited_radii[0], center=center, 
                       model=mod, material=glass, color='BLEUx')
# ajout de la sphere dans le conteneur de corps
bodies += body

# on depose les particules sous gravite, dans une sphere 
nb_comp_particles, coor, radii = pre.depositInSphere3D(radii, R, center,
                                                       d_radii=deposited_radii,
                                                       d_coor=center, seed=seed2)

# si toutes les particules deposees n'ont pas ete deposees
if (nb_comp_particles < nb_particles):
   # on affiche un avertissement
   print("Warning: granulometry changed, since some particles cannot be deposited!")

# boucle d'ajout des spheres :
for r, c in zip(radii, coor):
   # creation d'une nouvelle sphere rigide
   body = pre.rigidSphere(r=r, center=c, model=mod, material=plex, color='BLEUx')
   # on donne une vitesse initiale a la sphere
   body.imposeInitValue(component=2,value=3.0)
   # ajout de la sphere dans le conteneur de corps
   bodies += body

# ajout du mur :
wall = pre.rigidPlan(axe1=2.*R, axe2=2.*R, axe3=radius_min, center=[0., R + radius_min, 0.],
                     model=mod, material=tdur, color='VERTx')

# on place le mur a droite (just for fun) :
# rotation autour de l'axe x, par rapport au centre d'inertie, avec un angle
# pi/2  
wall.rotate(theta=0.5*np.pi, center=wall.nodes[1].coor)
# blocage du mur
wall.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
# ajout du mur au conteneur de corps
bodies.addAvatar(wall)

# gestion des interactions :
#   * declaration des lois
#       - entre particules
lspsp = pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.3)
tacts+= lspsp
#       - avec les parois
lsppl = pre.tact_behav(name='iqsc1', law='IQS_CLB', fric=0.5)
tacts+= lsppl
#   * declaration des tables de visibilite
#       - entre particules
svspsp = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='SPHER', colorAntagoniste='BLEUx',
                       behav=lspsp, alert=0.1*radius_min)
svs += svspsp
#       - avec les parois
svsppl = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='VERTx',
                       behav=lsppl, alert=0.1*radius_min)
svs+=svsppl

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, gravy=[0., 0., 0.])

try:
  pre.visuAvatars(bodies)
except:
  pass
