import sys
from pathlib import Path

import numpy as np

from pylmgc90 import pre

datbox = Path('./DATBOX')
datbox.mkdir(exist_ok=True)

if '--norand' in sys.argv:
  seed = list(range(12,12+33))
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

# initialisation des variables pour un depot sur un reseau cubique :

# on fixe le nombre de particules a generer
nb_particles = 500

# distribution monodisperse 
rad = 0.1
radii = np.empty([nb_particles])
radii[:] = rad

# on depose les particules sous gravite, dans une boite 
lx = 1.0
ly = 1.0
lz = 2.*lx
nb_comp_particles, coor, radii = pre.depositInBox3D(radii, lx, ly, lz, seed=seed)

# boucle d'ajout des spheres :
for i in range(nb_comp_particles):
   if (i%2 == 0):
    # creation d'une nouvelle sphere rigide
    body = pre.rigidSphere(r=radii[i], center=coor[i],
                          model=mod, material=plex, color='BLEUx')
    # ajout de la sphere dans le conteneur de corps
   else :
    body = pre.rigidPolyhedron(center=coor[i], material=plex, model=mod, color='BLACx',generation_type='regular',nb_vertices=20, radius=radii[i])
   #
   bodies += body

# creation de corps pour les parois
down = pre.rigidPlan(axe1=0.55*lx, axe2=0.55*ly, axe3=rad, center=[0., 0., -rad],
                     model=mod, material=tdur, color='VERTx')

down.translate(dx=lx/2., dy=ly/2.)

# blocage des parois
down.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')

# ajouts de parois au conteneur de corps
bodies.addAvatar(down)

# gestion des interactions :
#   * declaration des lois
#       - entre particules
lspsp = pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.3)
tacts+= lspsp
#       - avec les parois
lsppl = pre.tact_behav(name='iqsc1', law='IQS_CLB', fric=0.5)
tacts+= lsppl
#   * declaration des tables de visibilite
#       - entre particules spheres
svspsp = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='SPHER', colorAntagoniste='BLEUx',
                       behav=lspsp, alert=0.1*rad)
svs+=svspsp
#       - sphere avec les parois
svsppl = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='VERTx',
                       behav=lsppl, alert=0.1*rad)
svs+= svsppl

#       - entre particules polyedres
svprpr = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLACx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLACx',
                       behav=lspsp, alert=0.1*rad)
svs+=svprpr

#       - polyedre avec les parois
svprpl = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLACx',
                       CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='VERTx',
                       behav=lsppl, alert=0.1*rad)
svs+= svprpl

#       - entre particules spheres et polyedres
svsppr = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLACx',
                       behav=lspsp, alert=0.1*rad)
svs+=svsppr

g_d = 9.81
gravity = np.array([0., 0., -g_d])

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, gravy = gravity)

try:
  pre.visuAvatars(bodies)
except:
  pass
