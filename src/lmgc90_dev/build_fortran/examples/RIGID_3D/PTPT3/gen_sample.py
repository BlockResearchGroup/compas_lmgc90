# -*- coding: utf-8 -*-

import os, sys

if not os.path.isdir('./DATBOX'):
    os.mkdir('./DATBOX')

from pylmgc90 import pre

# # =============================================================================

# on se place en 3D
dim = 3
# creation des conteneurs
#   * pour les corps
bodies = pre.avatars()
#   * pour les materiaux
mats   = pre.materials()
#   * pour les modeles
mods   = pre.models()
#   * pour les tables de visibilite
svs    = pre.see_tables()
#   * pour les lois de contact
tacts  = pre.tact_behavs()
#   * pour les commandes de post-traitement
post   = pre.postpro_commands()

# creations du materiaux
#   * un rigide pour la fondation et les blocs
stone = pre.material(name='Stone', materialType='RIGID', density=2000.)
# ajout du materiau
mats.addMaterial(stone)

# on cree un modele de rigide
rigid = pre.model(name='Rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(rigid)


# # =============================================================================
#lecture de la geometrie de la fondation
mesh = pre.readMesh("gmsh/fondation.msh", dim=dim)
body = pre.surfacicMeshToRigid3D(mesh, model=rigid, material=stone, color='FONDA')
body.imposeDrivenDof(component=[1,2,3,4,5,6],dofty='vlocy')
bodies += body
    
    
# # =============================================================================
# lecture de la geometrie des blocs
for i, n in enumerate(["gmsh/bloc.msh", "gmsh/bloc.msh"]):
    mesh = pre.readMesh(n, dim=dim)
    body = pre.surfacicMeshToRigid3D(mesh, model=rigid, material=stone, color='BLOCS')

    # caracteristiques geometriques du bloc
    frame = body.bulks[0].axis.T
    if i == 0:
        body.imposeDrivenDof(component=1, dofty='force', ct=10000.)
        shift = frame.dot([0., -0.15, 1.])
        body.addContactors(shape='PT3Dx', color='AGRAF',shift=shift)
        body.translate(dy=0.25)
    elif i == 1:
        shift = frame.dot([0., 0.15, 1.])
        body.addContactors(shape='PT3Dx', color='AGRAF',shift=shift)
        body.translate(dy=-0.25)
    bodies += body

# gestion des interactions :
#   * declaration des lois
#       - entre les blocs
frottant  = pre.tact_behav(name='frott', law='IQS_CLB', fric=1.0)
tacts    += frottant
#       - pour les agraffes
agraf     = pre.tact_behav(name='agraf', law='ELASTIC_WIRE', stiffness=1.e8, prestrain=0.)
tacts    += agraf

#   * declaration des tables de visibilite
# - avec la fondation
svfdbl =  pre.see_table(CorpsCandidat    = 'RBDY3', candidat    = 'POLYR', colorCandidat    = 'FONDA', behav=frottant,
                        CorpsAntagoniste = 'RBDY3', antagoniste = 'POLYR', colorAntagoniste = 'BLOCS', alert=0.02)
svs   += svfdbl
#  - entre les blocs
svblbl =  pre.see_table(CorpsCandidat    = 'RBDY3', candidat    = 'POLYR', colorCandidat    = 'BLOCS', behav=frottant,
                        CorpsAntagoniste = 'RBDY3', antagoniste = 'POLYR', colorAntagoniste = 'BLOCS', alert=0.02)
svs   += svblbl
#       - agraffes
svagraf = pre.see_table(CorpsCandidat    = 'RBDY3', candidat    = 'PT3Dx', colorCandidat    = 'AGRAF', behav=agraf,
                        CorpsAntagoniste = 'RBDY3', antagoniste = 'PT3Dx', colorAntagoniste = 'AGRAF', alert=0.25)
svs += svagraf

# ------------------------------------------------------
#post pro

#   * detection des contacts
solveur = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)

post.addCommand(solveur)

# ------------------------------------------------------

# Write all files to compute solution with LMGC90
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass
