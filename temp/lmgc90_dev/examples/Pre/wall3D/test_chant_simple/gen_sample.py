from __future__ import print_function
import os,sys

from pylmgc90 import pre

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

# creations de deux materiaux
tdur = pre.material(name='TDURx',materialType='RIGID',density=2500.)
plex = pre.material(name='PLEXx',materialType='RIGID',density=2000.)
mats.addMaterial(tdur,plex)

# on cree un modele de rigide 3D, pour les briques
mod3D = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod3D)

# on deifinit la brique de reference, ici une brique de Paris
brique_Paris = pre.brick3D(name='brique de Paris', lx=0.22, ly=0.11, lz=0.06)

# on declare un nouveau mur, avec apareil sur chant simple, constitue de
# briques de Paris
wall = pre.paneresse_simple(brick_ref=brique_Paris, disposition="chant")

# on caracterise le mur, en longueur

# on definit la premiere rangee de briques, avec la brique de debut, le nombre de briques et l'epaisseur d'un joint
#wall.setFirstRowByNumberOfBricks(first_brick_type="1/2", nb_bricks=10., joint_thickness=0.01)
# on definit la premiere rangee de briques, avec la brique de debut, la longueur et l'epaisseur d'un joint
wall.setFirstRowByLength(first_brick_type="1/2", length=2.3, joint_thickness=0.01)

# on caracterise le mur, en hauteur
#   * nombre de briques dans le mur
wall.setNumberOfRows(10.)
#   * epaisseur des joints
wall.setJointThicknessBetweenRows(0.01)
#   * hauteur du mur
#wall.setHeight(0.7)

# on calcule le nombre de rangees de briques dans le mur
#wall.computeNbRows(trend="max")
# on calcule la hauteur du mur
wall.computeHeight()

print("wall.nb_rows=", wall.nb_rows)
print("wall.height=", wall.height)
print("wall.joint_thickness=", wall.joint_thickness)

# on construit le mur, en donnant ou le placer, les caracteristiques de la brique (modele, materiau) et les couleurs
# pour la detection du contact
bodies = wall.buildRigidWall(origin=[0., 0., 0.], model=mod3D, material=plex, colors=['BLEUx', 'REDxx'])

# ajout d'une fondation rigide, i.e. faite d'un plan :

# on declare un corps pour la fondation
floor = pre.rigidPlan(axe1=1.25, axe2=0.08, axe3=0.03, center=[1.15, 0.03, -0.03],
                      model = mod3D, material = tdur, color='WALLx')
# on fixe la fondation
floor.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')

# on ajoute la fondation a la liste des corps
bodies += floor

# gestion des interactions :
#   * declaration des lois
#       - entre briques
lprpr = pre.tact_behav(name='iqsg0', law='IQS_CLB_g0', fric=0.3)
tacts+= lprpr
#       - avec la fondation
lprpl = pre.tact_behav(name='iqsg1', law='IQS_CLB_g0', fric=0.5)
tacts+= lprpl

#   * declaration des tables de visibilite
#       - entre briques
svbbbb = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLEUx',
                       behav='iqsg0',  alert=0.02)
svs += svbbbb
svbrbr = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='REDxx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx',
                       behav='iqsg0',  alert=0.02)
svs += svbrbr
svbbbr = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx',
                       behav='iqsg0',  alert=0.02)
svs += svbbbr
#       - avec la fondation
svprpl = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='WALLx',
                       behav='iqsg1',  alert=0.02)
svs += svprpl

# ecriture des fichiers
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)

try:
  pre.visuAvatars(bodies)
except:
  pass

