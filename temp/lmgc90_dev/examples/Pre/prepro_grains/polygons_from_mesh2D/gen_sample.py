import os,sys

import numpy
import math

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

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

# on lit un maillage 2D
surfacic_mesh = pre.readMesh('gmsh/Carre.msh', dim)
# on construit un corps rigide (polygone) a partir de chaque element du maillage
bodies += pre.rigidsFromMesh2D(surfacic_mesh=surfacic_mesh, model=mod, material=plex, color='BLEUx')

# ajout d'une fondation lisse, i.e. un jonc :
down = pre.rigidJonc(axe1=1.5e-1, axe2=1.e-3, center=[5.e-2, -1.e-3], model=mod, material=tdur, color='WALLx')

# on ajoute la fondation a la liste des corps
bodies.addAvatar(down)

# on fixe la fondation
down.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

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
svdkdk =  pre.see_table(CorpsCandidat='RBDY2',candidat='POLYG', colorCandidat='BLEUx',
                        CorpsAntagoniste='RBDY2', antagoniste='POLYG',colorAntagoniste='BLEUx',
                        behav=ldkdk, alert=5.e-3)
svs+=svdkdk
#       - avec les parois
svdkjc = pre.see_table(CorpsCandidat='RBDY2',candidat='POLYG', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY2', antagoniste='JONCx',colorAntagoniste='WALLx',
                       behav=ldkjc, alert=5.e-3)
svs+= svdkjc

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)
