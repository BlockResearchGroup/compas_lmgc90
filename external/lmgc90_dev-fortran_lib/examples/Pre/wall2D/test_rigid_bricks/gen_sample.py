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
#   * pour les modeles
mods = pre.models()
#   * pour les tables de visibilite
svs = pre.see_tables()
#   * pour les lois de contact
tacts = pre.tact_behavs()

# creations de deux materiaux
tdur = pre.material(name='TDURx',materialType='RIGID',density=2500.)
plex = pre.material(name='PLEXx',materialType='RIGID',density=2000.)
mats.addMaterial(tdur,plex)

# on cree un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mods.addModel(mod)

# on definit des briques
#   * brique :
moellon = pre.brick2D('brique', 1.e-1, 5.e-2)
#   * demi-brique :    
demi_moellon = pre.brick2D('demi-brique', 5.e-2, 5.e-2)
#   * linteau :
linteau = pre.brick2D('linteau', 3.e-1, 5.e-2) 
# brique speciale, pour mettre une fenetre :
fantome = pre.brick2D('ghost', 2.e-1, 5.e-2)

# on declare une epaisseur pour les joints
#   * horizontaux :
epaisseur_joint_horizontal=0.
epaisseur_joint_vertical=0.

# on definit deux assises, comme des listes de briques :
assise_paire=[demi_moellon, moellon, moellon, moellon, moellon, demi_moellon]
assise_impaire=[moellon, moellon, moellon, moellon, moellon]
assise_fenetre_paire=[demi_moellon, moellon, fantome, moellon, demi_moellon]
assise_fenetre_impaire=[moellon, demi_moellon, fantome, demi_moellon, moellon]
assise_linteau=[moellon, linteau, moellon]

# on definit le mur comme une liste d'assises :
mur=[assise_paire, assise_impaire, assise_paire, assise_impaire, 
     assise_fenetre_paire, assise_fenetre_impaire, assise_fenetre_paire,
     assise_linteau, assise_paire]

# on construit le mur :

# nombre de brique posees
nb_bricks=0
# position du centre d'inertie de la brique courante :
x=0.
y=0.
# pour chaque assise :
for j in range(0, len(mur), 1):
   # on recupere l'assise courante
   assise=mur[j]
   # on definit la couleur des briques selon la parite de l'indice de l'assise
   if j % 2 == 0:
      color='BLEUx'
   else:
      color='REDxx'
   # on reinitialise l'abscisse du centre d'inertie de la prochaine brique a 0
   x=0.

   # pour chaque brique de l'assise courante
   for i in range(0, len(assise), 1):
      # on incremente le nombre de briques deposees
      nb_bricks += 1
      # on recupere la brique courante
      brique=assise[i]
      # si c'est la premiere brique de l'assise
      if i == 0:
         # on incremente l'ordonnee du centre d'inertie de la prochaine brique
         y += 0.5*brique.ly

      # on incremente l'absisse du centre d'inertie de la prochaine brique
      x += 0.5*brique.lx
      # si la brique n'est pas une brique fantome (pour palcer une fenetre ou 
      # une porte
      if brique.name != 'ghost':
         # on cree et on ajoute la brique a la liste des corps
         bodies += brique.rigidBrick(center=[x, y], model=mod, material=plex, color=color)

      # on incremente l'abscisse du centre d'inertie de la prochaine brique, 
      # pour obtenir la position du bord gauche de la prochaine brique
      x += 0.5*brique.lx + epaisseur_joint_vertical

   # on incremente l'ordonnee du centre d'inertie de la prochaine brique, pour 
   # obtenir la position du bord superieur de la prochaine brique
   y += 0.5*brique.ly + epaisseur_joint_horizontal

# ajout d'une fondation rigide, i.e. faite d'un jonc :

# on declare un corps pour la fondation
floor = pre.rigidJonc(axe1=3.e-1,axe2=2.5e-2,center=[2.5e-1, -2.5e-2],model=mod,material=tdur,color='WALLx')
# on ajoute la fondation a la liste des corps
bodies += floor

# on fixe la fondation
floor.imposeDrivenDof(component=[1, 2, 3],dofty='vlocy')

# gestion des interactions :
#   * declaration des lois
#       - entre briques
lplpl = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts+= lplpl
#       - avec la fondation
lpljc = pre.tact_behav(name='iqsc1',law='IQS_CLB',fric=0.5)
tacts+= lpljc
#   * declaration des tables de visibilite
#       - entre btiques
svbbbb = pre.see_table(CorpsCandidat='RBDY2',candidat='POLYG', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY2', antagoniste='POLYG',colorAntagoniste='BLEUx',
                       behav=lplpl,alert=5.e-3)
svs += svbbbb
svbrbr = pre.see_table(CorpsCandidat='RBDY2',candidat='POLYG', colorCandidat='REDxx',
                       CorpsAntagoniste='RBDY2', antagoniste='POLYG',colorAntagoniste='REDxx',
                       behav=lplpl,alert=5.e-3)
svs+=svbrbr
svbbbr = pre.see_table(CorpsCandidat='RBDY2',candidat='POLYG', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY2', antagoniste='POLYG',colorAntagoniste='REDxx',
                       behav=lplpl,alert=5.e-3)
svs += svbbbr
#       - avec les parois
svpljc = pre.see_table(CorpsCandidat='RBDY2',candidat='POLYG', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY2', antagoniste='JONCx',colorAntagoniste='WALLx',
                       behav=lpljc,alert=5.e-3)
svs += svpljc

# ecriture des fichiers
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)

try:
  pre.visuAvatars(bodies)
except:
  pass

