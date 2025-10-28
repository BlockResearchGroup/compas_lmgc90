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

# creations de deux materiaux :
#   * un rigide pour la fondation
tdur = pre.material(name='TDURx',materialType='RIGID',density=2500.)
#   * un materiau elastique lineaire isotrope pour les briques :
steel = pre.material(name='steel', materialType='ELAS', elas='standard',
                     young=1.e10, nu=0.15, anisotropy='isotropic', density=1800.)
# ajout des materiaux
mats.addMaterial(tdur,steel)

# definition du modele elastique :
#    * avec des T3
m2DlT3 = pre.model(name='M2DLT', physics='MECAx', element='T3xxx', dimension=2, 
                   external_model='MatL_', kinematic='small', 
                   material='elas_', anisotropy='iso__', mass_storage='lump_')
#    * avec des Q4
m2DlQ4 = pre.model(name='M2DLQ', physics='MECAx', element='Q4xxx', dimension=2,
                   external_model='MatL_', kinematic='small',
                   material='elas_', anisotropy='iso__', mass_storage='lump_')
# ajout du modele
mods.addModel(m2DlT3)
mods.addModel(m2DlQ4)

# on cree un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

# longueurs caracteristiques :
#   * en x, la largeur de la demi-brique
lcx=5e-2
#   * en y, la largeur de la brique
lcy=5e-2

# on definit des briques
#   * brique :
moellon = pre.brick2D('brique', 2*lcx, lcy)
#   * demi-brique :    
demi_moellon = pre.brick2D('demi-brique', lcx, lcy)
#   * linteau :
linteau = pre.brick2D('linteau', 6*lcx, lcy) 
# brique speciale, pour mettre une fenetre :
fantome = pre.brick2D('ghost', 4*lcx, lcy)

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
      # on ajoute la brique a la liste des corps, en fonction de son nom
      if brique.name == 'ghost': # cas de la brique fantome
         # on ne fait rien, pour laisser une ouverture (i.e. pour une porte ou
         # une fenetre
         pass
      elif brique.name == 'linteau': # cas du linteau 
         # on calcule le nombre d'elements suivant chaque direction
         nb_elem_x=int(math.floor(brique.lx/(0.5*lcx)))
         nb_elem_y=int(math.floor(brique.ly/(0.5*lcy)))
         # brique fissurable
         bodies += brique.explodedDeformableBrick(center=[x, y], material=steel, model=m2DlT3,
                                                  mesh_type='4T3', nb_elem_x=nb_elem_x, nb_elem_y=nb_elem_y)
      else: # cas general
         # on calcule le nombre d'elements suivant chaque direction
         nb_elem_x=int(math.floor(brique.lx/(0.5*lcx)))
         nb_elem_y=int(math.floor(brique.ly/(0.5*lcy)))
         # on cree et on ajoute la brique a la liste des corps
         bodies += brique.deformableBrick(center=[x, y], material=steel, model=m2DlQ4,
                                          mesh_type='Q4', nb_elem_x=nb_elem_x, nb_elem_y=nb_elem_y)

      # on incremente l'abscisse du centre d'inertie de la prochaine brique, 
      # pour obtenir la position du bord gauche de la prochaine brique
      x += 0.5*brique.lx + epaisseur_joint_vertical

   # on incremente l'ordonnee du centre d'inertie de la prochaine brique, pour 
   # obtenir la position du bord superieur de la prochaine brique
   y += 0.5*brique.ly + epaisseur_joint_horizontal

# ajout d'une fondation rigide, i.e. faite d'un jonc :

# on declare un corps pour la fondation
floor = pre.rigidJonc(axe1=6*lcx,axe2=0.5*lcy,center=[5*lcx, -0.5*lcy],model=mod,material=tdur,color='WALLx')
# on ajoute la fondation a la liste des corps
bodies += floor

# on fixe la fondation
floor.imposeDrivenDof(component=[1, 2, 3],dofty='vlocy')

# gestion des interactions :
#   * declaration des lois
#       - entre elements
lclalpI = pre.tact_behav(name='macc0',law='MAC_CZM',dyfr=0.3,stfr=0.3,cn=1.e+10,
                             ct=1.e+10,b=0.,w=0.1)
tacts+=lclalpI
#       - entre briques
lclalp = pre.tact_behav(name='gapc0',law='GAP_SGR_CLB',fric=0.3)
tacts += lclalp
#       - avec la fondation
lcljc = pre.tact_behav(name='gapc1',law='GAP_SGR_CLB',fric=0.5)
tacts += lcljc
#   * declaration des tables de visibilite
#       - entres elements
svclalpI = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx', colorCandidat='BLEUx',
                         CorpsAntagoniste='MAILx', antagoniste='ALpxx',colorAntagoniste='BLEUx',
                         behav=lclalpI, alert=0.01)
svs += svclalpI
#       - entre briques
svclalpH = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx', colorCandidat='HORIx',
                         CorpsAntagoniste='MAILx', antagoniste='ALpxx',colorAntagoniste='HORIx',
                         behav=lclalp,alert=0.1*min(lcx, lcy))
svs += svclalpH
svclalpV = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx', colorCandidat='VERTx',
                         CorpsAntagoniste='MAILx', antagoniste='ALpxx',colorAntagoniste='VERTx',
                         behav=lclalp,alert=0.1*min(lcx, lcy))
svs += svclalpV
#       - avec la fondation
svcljc = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx', colorCandidat='HORIx',
                       CorpsAntagoniste='RBDY2', antagoniste='JONCx',colorAntagoniste='WALLx',
                       behav=lcljc, alert=0.1*min(lcx, lcy))
svs += svcljc

# ecriture des fichiers
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)

try:
  pre.visuAvatars(bodies)
except:
  pass

