# -*- coding: utf-8 -*-
import os
import numpy as np

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

from pylmgc90 import pre


# # =============================================================================

# on se place en 2D
dim=2
# creation des conteneurs
#   * pour les corps
bodies = pre.avatars()
#   * pour les materiaux
mat = pre.materials()
#   * pour les modeles
mod = pre.models()
#   * pour les tables de visibilite
svs = pre.see_tables()
#   * pour les lois de contact
tacts = pre.tact_behavs()
#   * pour les commandes de post-traitement
post = pre.postpro_commands()

# creations du materiaux
#   * un rigide pour la fondation et les blocs
stone = pre.material(name='TDURx',materialType='RIGID',density=2000.)
# ajout du materiau
mat.addMaterial(stone)

# on cree un modele de rigide
rigid = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mod.addModel(rigid)


with open("geometrie.dat",'r') as fid:
    lines = fid.readlines()

# # =============================================================================
#lecture de la geometrie de la fondation
for line in lines[:2]:
    vertices = np.array(line.split(","),'float')
    vertices = vertices.reshape(4,2)
    # on cree le body et on l'ajoute dans le conteneur
    body = pre.rigidPolygon(model=rigid, material=stone, center=[0.,0.], theta=0.,
                            color='FONDA', generation_type='full', vertices=vertices)
    body.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
    bodies += body

# # =============================================================================
# lecture de la geometrie des blocs
for i,line in enumerate(lines[2:]):
    vertices = np.array(line.split(","),'float')
    vertices = vertices.reshape(4,2)
    # on cree le body et on l'ajoute dans le conteneur
    body = pre.rigidPolygon(model=rigid, material=stone, center=[0.,0.], theta=0.,
                            color='BLOCx', generation_type='full', vertices=vertices)
    if i == 4:
        # body.imposeDrivenDof(component=2, dofty='force', ct=-10000.)
        body.imposeDrivenDof(component=2, dofty='force', ct=-10000.)

    bodies += body


# gestion des interactions :
#   * declaration des lois
#       - entre les blocs
lblbl  = pre.tact_behav(name='gapc0',law='IQS_CLB_g0',fric=0.5)
tacts += lblbl
#       - avec la fondation
lblfd  = pre.tact_behav(name='gapc1',law='IQS_CLB_g0',fric=0.8)
tacts += lblfd

#   * declaration des tables de visibilite
#       - entre blocs
svblbl = pre.see_table(CorpsCandidat    = 'RBDY2', candidat    = 'POLYG', 
                       colorCandidat    = 'BLOCx', behav       = lblbl,
                       CorpsAntagoniste = 'RBDY2', antagoniste = 'POLYG', 
                       colorAntagoniste = 'BLOCx', alert=0.1)
svs += svblbl
#~ #       - avec la fondation
svblfd = pre.see_table(CorpsCandidat    = 'RBDY2', candidat    = 'POLYG', 
                       colorCandidat    = 'BLOCx', behav       = lblfd,
                       CorpsAntagoniste = 'RBDY2', antagoniste = 'POLYG', 
                       colorAntagoniste = 'FONDA', alert=0.1)
svs += svblfd

# ------------------------------------------------------
#post pro

#   * detection des contacts
solveur = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)

post.addCommand(solveur)

# ------------------------------------------------------

# Write all files to compute solution with LMGC90
pre.writeDatbox(dim, mat, mod, bodies, tacts, svs, post=post, gravy=[0.,-9.81, 0.])

try:
  pre.visuAvatars(bodies)
except:
  pass
