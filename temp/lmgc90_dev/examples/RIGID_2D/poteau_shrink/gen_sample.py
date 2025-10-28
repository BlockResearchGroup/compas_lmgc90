# -*- coding: utf-8 -*-
import os, sys

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

from pylmgc90 import pre


# # =============================================================================

# on se place en 2D
dim = 2
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

# creations du materiaux
#   * un rigide pour la fondation et les blocs
stone  = pre.material(name='stone',materialType='RIGID',density=2000.)
# ajout du materiau
mat.addMaterial(stone)

# on cree un modele de rigide
rigid = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mod.addModel(rigid)


# # =============================================================================
# Fondation

fonda = pre.brick2D('fonda', 1.2, 0.5)
body  = fonda.rigidBrick(center=[0., -0.5], model=rigid, material=stone, color='FONDA')
# fondation rigide
body.imposeDrivenDof(component=[1,2,3],dofty='vlocy',ct=0.0)
    
bodies += body


# # =============================================================================
# Blocs

blocs = pre.brick2D('blocs', 1.2, 0.5)
for i in range(20):
    body    = blocs.rigidBrick(center=[0., i*0.5], model=rigid, material=stone, color='BLOCS')
    bodies += body
    
# force horizontale imposee au haut du poteau
body.imposeDrivenDof(component=1, dofty='force', ct=1000.)


# # =============================================================================
# gestion des interactions :
#   * declaration des lois

#       - entre les blocs
elastic  = pre.tact_behav(name='elast',law='ELASTIC_REPELL_CLB', stiffness=2e10, fric=0.9)
tacts   += elastic


#   * declaration des tables de visibilite
#       - entre blocs
svblbl = pre.see_table(CorpsCandidat    = 'RBDY2', candidat    = 'POLYG', colorCandidat    = 'BLOCS', behav=elastic,
                       CorpsAntagoniste = 'RBDY2', antagoniste = 'POLYG', colorAntagoniste = 'BLOCS', alert=0.01)
svs += svblbl
#~ #       - avec la fondation
svblfd = pre.see_table(CorpsCandidat    = 'RBDY2', candidat    = 'POLYG', colorCandidat    = 'BLOCS', behav=elastic,
                       CorpsAntagoniste = 'RBDY2', antagoniste = 'POLYG', colorAntagoniste = 'FONDA', alert=0.01)
svs += svblfd


# # =============================================================================
# Write all files to compute solution with LMGC90
pre.writeDatbox(dim, mat, mod, bodies, tacts, svs, gravy=[0., -9.81, 0.])

try:
  pre.visuAvatars(bodies)
except:
  pass
