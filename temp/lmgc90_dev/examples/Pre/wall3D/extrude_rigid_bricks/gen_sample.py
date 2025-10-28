import os,sys

import math

from pylmgc90 import pre

# fonction qui contruit un mur
# parametres d'entree :
#   - wall : liste d'assises, une assise etant une liste de briques (i.e. d'objets brick)
#   - horizontal_thickness : epaisseur des joints horizontaux
#   - vertical_thickness : epaisseur des joints verticaux
#   - model : modele pour la creation des briques (2D) 
#   - material : materiau constitutif des briques
# valeur de retour :
#   - un conteneur d'avatars contenant les briques, sous la forme de polygones rigides
def buildWall2D(wall, horizontal_thickness, vertical_thickness, model, material):
   # on cree un conteneur d'avatar pour stocker les briques du mur
   bricks = pre.avatars()

   # nombre de brique posees
   nb_bricks=0
   # position du centre d'inertie de la brique courante :
   x=0.
   y=0.
   # pour chaque assise :
   for j in range(0, len(wall), 1):
      # on recupere l'assise courante
      assise=wall[j]
      # on definit la couleur des briques selon la parite de l'indice de l'assise
      if j % 2 == 0:
         color='BLEUx'
      else:
         color='REDxx'
      # on reinitialise l'abscisse du centre d'inertie de la prochaine brique a 0
      x=0.
   
      # pour chaque brique de l'assise courante
      for i in range(0, len(assise), 1):
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
            # on incremente le nombre de briques deposees
            nb_bricks += 1
            # on cree la brique en 2D
            brick2D = brique.rigidBrick(center=[x, y], model=model, material=material, color=color) 
            # on l'extrude pour obtenir une brique 3D
            bricks += brick2D
   
         # on incremente l'abscisse du centre d'inertie de la prochaine brique, 
         # pour obtenir la position du bord gauche de la prochaine brique
         x += 0.5*brique.lx + vertical_thickness
   
      # on incremente l'ordonnee du centre d'inertie de la prochaine brique, pour 
      # obtenir la position du bord superieur de la prochaine brique
      y += 0.5*brique.ly + horizontal_thickness
    
   # on renvoie la liste de corps generee
   return bricks

# on se place en 3D
dim = 3

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
tdur = pre.material(name='TDURx',materialType='RIGID',density=2500.)
plex = pre.material(name='PLEXx',materialType='RIGID',density=2000.)
mats.addMaterial(tdur,plex)

# on cree un modele de rigide 2D, pour definir les briques 2D
mod2D = pre.model(name='rig2d', physics='MECAx', element='Rxx2D', dimension=2)

# on crek un modele de rigide 3D, pour l'extrusion des briques 2D
mod3D = pre.model(name='rig3d', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod3D)

# longueurs caracteristiques :
#   * en x, la largeur de la demi-brique
lcx=5e-2
#   * en y, la largeur de la brique
lcy=5e-2

# on definit des briques
#   * brique :
moellon=pre.brick2D('brique', 2*lcx, lcy)
#   * demi-brique :    
demi_moellon=pre.brick2D('demi-brique', lcx, lcy)
#   * linteau :
linteau=pre.brick2D('linteau', 6*lcx, lcy) 
# briques fantome :
#   * pour mettre une fenetre :
fantome=pre.brick2D('ghost', 4*lcx, lcy)
#   * pour le harpage :
quart_fantome=pre.brick2D('ghost', lcx, lcy)

# on declare une epaisseur pour les joints
#   * horizontaux :
epaisseur_joint_horizontal=0.
epaisseur_joint_vertical=0.

# on definit des assises, comme des listes de briques :
assise_paire=[quart_fantome, moellon, moellon, moellon, moellon, quart_fantome]
assise_impaire=[moellon, moellon, moellon, moellon, moellon]
assise_ouverture_paire=[quart_fantome, moellon, fantome, moellon, quart_fantome]
assise_ouverture_impaire=[moellon, demi_moellon, fantome, demi_moellon, moellon]
assise_linteau=[moellon, linteau, moellon]

# premier mur : mur avec une porte :

# on definit le mur comme une liste d'assises :
mur_porte=[assise_ouverture_paire, assise_ouverture_impaire, assise_ouverture_paire, 
     assise_ouverture_impaire, assise_ouverture_paire, assise_ouverture_impaire, 
     assise_ouverture_paire, assise_linteau, assise_paire]

# on contruit le mur en 2D
wall2D_door = buildWall2D(wall=mur_porte, horizontal_thickness=epaisseur_joint_horizontal,
   vertical_thickness=epaisseur_joint_vertical, model=mod2D, material=plex)

# on construit le mur en 3D par extrusion
wall3D_door = pre.extrudeRigids(bodies2D=wall2D_door, depth=lcx, model3D=mod3D)

# on l'ajoute a la liste des corps
bodies += wall3D_door

# deuxieme mur : mur avec une fenetre :

# on definit le mur comme une liste d'assises :
mur_fenetre=[assise_paire, assise_impaire, assise_paire, assise_impaire, 
     assise_ouverture_paire, assise_ouverture_impaire, assise_ouverture_paire,
     assise_linteau, assise_paire]

# on contruit le mur en 2D
wall2D_window = buildWall2D(wall=mur_fenetre, horizontal_thickness=epaisseur_joint_horizontal, 
   vertical_thickness=epaisseur_joint_vertical, model=mod2D, material=plex)

# on construit le mur en 3D par extrusion
wall3D_window = pre.extrudeRigids(bodies2D=wall2D_window, depth=lcx, model3D=mod3D)

# on translate le mur pour le placer
wall3D_window.translate(dy=9*lcx)

# on l'ajoute a la liste des corps
bodies += wall3D_window

# troisieme mur : mur plein
mur_plein=[assise_impaire, assise_paire, assise_impaire, assise_paire,
     assise_impaire, assise_paire, assise_impaire, assise_paire, assise_impaire]

# on contruit le mur en 2D
wall2D_plain = buildWall2D(wall=mur_plein, horizontal_thickness=epaisseur_joint_horizontal, 
   vertical_thickness=epaisseur_joint_vertical, model=mod2D, material=plex)

# on construit le mur en 3D par extrusion
wall3D_plain = pre.extrudeRigids(bodies2D=wall2D_plain, depth=lcx, model3D=mod3D)

# on place le mur :

# on le tourne d'un angle pi/2, autour de son axe de symetrie vertical
wall3D_plain.rotate(psi=0.5*math.pi, center=[5*lcx, 0.5*lcx, 9*lcx])

# on le translate pour finir sa mise en place
wall3D_plain.translate(dx=-4.5*lcx, dy=4.5*lcx)

# on l'ajoute a la liste des corps
bodies += wall3D_plain

# quatrieme mur : mur plein

# on contruit le mur en 2D
wall2D_plain = buildWall2D(wall=mur_plein, horizontal_thickness=epaisseur_joint_horizontal, 
   vertical_thickness=epaisseur_joint_vertical, model=mod2D, material=plex)

# on construit le mur en 3D par extrusion
wall3D_plain = pre.extrudeRigids(bodies2D=wall2D_plain, depth=lcx, model3D=mod3D)

# on place le mur :

# on le tourne d'un angle pi/2, autour de son axe de symetrie vertical
wall3D_plain.rotate(psi=0.5*math.pi, center=[5*lcx, 0.5*lcx, 9*lcx])

# on le translate pour finir sa mise en place
wall3D_plain.translate(dx=4.5*lcx, dy=4.5*lcx)

# on l'ajoute a la liste des corps
bodies += wall3D_plain

# ajout d'une fondation rigide, i.e. faite d'un plan :

# on declare un corps pour la fondation
floor = pre.rigidPlan(axe1=7.5*lcx, axe2=7.5*lcx, axe3=0.25*lcy, center=[5*lcx, 5*lcx, -0.25*lcy],
                      model=mod3D, material=tdur, color='WALLx')
# on ajoute la fondation a la liste des corps
bodies += floor

# on fixe la fondation
floor.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')

# gestion des interactions :
#   * declaration des lois
#       - entre briques
lprpr = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts+= lprpr
#       - avec la fondation
lprpl = pre.tact_behav(name='iqsc1',law='IQS_CLB',fric=0.5)
tacts+= lprpl
#   * declaration des tables de visibilite
#       - entre briques
svbbbb = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLEUx',
                       behav='iqsc0', alert=0.1*min(lcx, lcy))
svs += svbbbb
svbrbr = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='REDxx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx',
                       behav='iqsc0', alert=0.1*min(lcx, lcy))
svs += svbrbr
svbbbr = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx',
                       behav='iqsc0', alert=0.1*min(lcx, lcy))
svs += svbbbr
#       - avec la fondation
svprpl = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='WALLx',
                       behav='iqsc1', alert=0.1*min(lcx, lcy))
svs += svprpl

# ecriture des fichiers
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)

try:
  pre.visuAvatars(bodies)
except:
  pass

