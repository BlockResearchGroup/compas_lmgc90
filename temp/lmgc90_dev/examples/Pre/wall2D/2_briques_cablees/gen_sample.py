import os,sys

import numpy
import math

from pylmgc90 import pre

# on se place en 2D
dim = 2

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

# creations d'un materiau pour les briques
stone = pre.material(name='STONE',materialType='RIGID',density=1800.)
mats.addMaterial(stone)

# on cree un modele de rigide 2D, pour les briques
mod2D = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mods.addModel(mod2D)

# on deifinit la brique de reference, ici une brique de Paris
brique_Paris = pre.brick2D(name='brique de Paris', lx=0.22, ly=0.06)

# on place deux briques, l'une au dessus-de l'autre
brick_down=brique_Paris.rigidBrick(center=[0., 0.], model=mod2D, material=stone, color='BLEUx')
brick_up=brique_Paris.rigidBrick(center=[0., 0.07], model=mod2D, material=stone, color='BLEUx')

# on bloque la brique du dessus
brick_up.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

# on ajoute un contacteur point au centre d'inertie de chaque brique
brick_down.addContactors(shape='PT2Dx', color='REDxx', shift=[0., 0.])
brick_up.addContactors(shape='PT2Dx', color='REDxx', shift=[0., 0.])

# on ajoute les briques dans le conteneur de corps
bodies.addAvatar(brick_down)
bodies.addAvatar(brick_up)

# gestion des interactions :
#   * declaration des lois
#       - entre briques
lptpt = pre.tact_behav(name='elas0', law='ELASTIC_WIRE', stiffness=5.e3, prestrain=0.)
tacts+= lptpt

#   * declaration des tables de visibilite
#       - entre briques
svbb = pre.see_table(CorpsCandidat='RBDY2', candidat='PT2Dx', colorCandidat='REDxx',
                     CorpsAntagoniste='RBDY2', antagoniste='PT2Dx', colorAntagoniste='REDxx',
                     behav=lptpt,  alert=0.1)
svs+= svbb

# ecriture des fichiers
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)

try:
  pre.visuAvatars(bodies)
except:
  pass

