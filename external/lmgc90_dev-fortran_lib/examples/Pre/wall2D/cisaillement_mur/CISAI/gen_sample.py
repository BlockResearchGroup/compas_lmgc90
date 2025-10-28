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
# pour les commandes de post-traitement
post = pre.postpro_commands()

# creations de deux materiaux :
#   * un rigide pour la fondation
tdur = pre.material(name='TDURx',materialType='RIGID',density=2500.)
#   * un materiau elastique lineaire isotrope pour les briques :

# parametres materiau d'une brique:
#   * module d'Young (tire de la these)
E_stone=1.67e10
#   * coefficient de Poisson (tire de la these)
nu_stone=0.15
#   * masse volumique (tiree du chapeau ?)
density_stone=2.5e3

# definition du materiau
stone = pre.material(name='stone', materialType='ELAS', elas='standard',
                     young=E_stone, nu=nu_stone, anisotropy='isotropic',
                     density=density_stone)
# ajout des materiaux
mats.addMaterial(tdur, stone)

# definition du modele elastique elsatique lineaire hpp :
m2Dl = pre.model(name='M2D_L', physics='MECAx', element='Q4xxx', dimension=dim, external_model='MatL_',
                 kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')
# ajout du modele
mods.addModel(m2Dl)

# on cree un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

# longueurs du systeme :
#   * largeur de la brique
brick_length = 210.e-3
#   * hauteur de la brique
brick_height = 52.e-3
#   * profondeur de la brique
brick_depth = 100.e-3
#   * epaisseur du joint
joint_thick = 10.e-3

# longueurs caracteristiques :
#   * en x, la largeur de la demi-brique
lcx=(4.5*brick_length + 4.*joint_thick)/(4.5*2.)
#   * en y, la hauteur de la brique
lcy=(18.*brick_height + 17.*joint_thick)/18.

# on definit des briques
#   * brique :
moellon = pre.brick2D('brique', 2*lcx, lcy)
#   * demi-brique :    
demi_moellon = pre.brick2D('demi-brique', lcx, lcy)

# on declare une epaisseur pour les joints
#   * horizontaux :
epaisseur_joint_horizontal=0.
epaisseur_joint_vertical=0.

# on donne les abcisses pour poser les points contact sur une ligne de la brique
apab_brick = [0.25, 0.75] 

# on definit deux assises, comme des listes de briques :
assise_paire=[demi_moellon, moellon, moellon, moellon, moellon]
assise_impaire=[moellon, moellon, moellon, moellon, demi_moellon]

# on definit le mur comme une liste d'assises :
mur=[assise_impaire, assise_paire, assise_impaire, assise_paire, assise_impaire, assise_paire, 
     assise_impaire, assise_paire, assise_impaire, assise_paire, assise_impaire, assise_paire,
     assise_impaire, assise_paire, assise_impaire, assise_paire, assise_impaire, assise_paire]

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
      # si la brique est entiere
      elif brique.name == 'brique':
         # on la scinde en deux demi-briques, en vue d'une liaison
         # par une loi cohesive modelisant la rupture de la brique
 
         # demi-brique de gauche:
 
         # on calcule le nombre d'elements suivant chaque direction
         nb_elem_x=int(math.floor(demi_moellon.lx/(0.5*lcx)))
         nb_elem_y=int(math.floor(demi_moellon.ly/(0.5*lcy)))
         # on cree la brique
         body_left = demi_moellon.deformableBrick(center=[x - 0.25*brique.lx, y], 
                                                  material=stone, model=m2Dl, mesh_type='Q4', 
                                                  nb_elem_x=nb_elem_x, nb_elem_y=nb_elem_y,
                                                  apabh=apab_brick, apabv=apab_brick, 
                                                  colors=['HORIx', 'REDxx', 'HORIx', 'VERTx'])
 
         # demi-brique de droite:
 
         # on calcule le nombre d'elements suivant chaque direction
         nb_elem_x=int(math.floor(demi_moellon.lx/(0.5*lcx)))
         nb_elem_y=int(math.floor(demi_moellon.ly/(0.5*lcy)))
         # on cree la brique
         body_right = demi_moellon.deformableBrick(center=[x + 0.25*brique.lx, y], 
                                                   material=stone, model=m2Dl, mesh_type='Q4', 
                                                   nb_elem_x=nb_elem_x, nb_elem_y=nb_elem_y,
                                                   apabh=apab_brick, apabv=apab_brick, 
                                                   colors=['HORIx', 'VERTx', 'HORIx', 'REDxx'])
 
         # si la brique appartient a la derniere couche
         if j == len(mur) - 1:
            # on ajoute une couche de contacteur ligne sur le haut des briques,
            # pour assurer le contact avec le jonc 
            body_left.addContactors(group='up',shape='CLxxx',color='UPxxx', weights=apab_brick)
            body_right.addContactors(group='up',shape='CLxxx',color='UPxxx', weights=apab_brick)
         # on ajoute la brique a la liste des corps
         bodies += body_left
         bodies += body_right
      # sinon, c'est une demi-brique
      else: 
         # on peut traiter la brique directement
  
         # on calcule le nombre d'elements suivant chaque direction
         nb_elem_x=int(math.floor(brique.lx/(0.5*lcx)))
         nb_elem_y=int(math.floor(brique.ly/(0.5*lcy)))
         # on cree la brique
         body = brique.deformableBrick(center=[x, y], material=stone, model=m2Dl,
                                       mesh_type='Q4', nb_elem_x=nb_elem_x, nb_elem_y=nb_elem_y,
                                       apabh=apab_brick, apabv=apab_brick)
         # si la brique appartient a la derniere couche
         if j == len(mur) - 1:
            # on ajoute une couche de contacteur ligne sur le haut des briques,
            # pour assurer le contact avec le jonc 
            body.addContactors(group='up',shape='CLxxx',color='UPxxx', weights=apab_brick)
         # on ajoute la brique a la liste des corps
         bodies += body

      # on incremente l'abscisse du centre d'inertie de la prochaine brique, 
      # pour obtenir la position du bord gauche de la prochaine brique
      x += 0.5*brique.lx + epaisseur_joint_vertical

   # on incremente l'ordonnee du centre d'inertie de la prochaine brique, pour 
   # obtenir la position du bord superieur de la prochaine brique
   y += 0.5*brique.ly + epaisseur_joint_horizontal

# ajout d'une fondation rigide, i.e. faite d'un jonc :

# on declare un corps pour la fondation
floor = pre.rigidJonc(axe1=4.5*lcx,axe2=0.5*lcy,center=[4.5*lcx, -0.5*lcy],model=mod,material=tdur,color='WALLx')

# on ajoute la fondation a la liste des corps
bodies += floor

# on fixe la fondation
floor.imposeDrivenDof(component=[1, 2, 3],dofty='vlocy')

# ajout d'une poutre rigide pour appliquer le chargement, i.e. faite d'un polygone :

# geometrie de la poutre :
#    * longueur de la poutre (i.e. longueur du mur)
beam_length = 9.*lcx
#    * hauteur de la poutre (i.e. hauteur d'une brique
beam_height = brick_height 
#    * profondeur de la poutre (i.e. profondeur d'une brique)
beam_depth = brick_depth

# on construit la poutre comme un jonc

# on declare un corps pour la poutre
beam = pre.rigidJonc(axe1=0.5*beam_length,axe2=0.5*beam_height,center=[0.5*beam_length, 18.*lcy + 0.5*beam_height],model=mod,material=tdur, color='WALLx')

# on ajoute la poutre a la liste des corps
bodies += beam

# vitesse maximale imposee a la fin du chargement
v=0.02 # en m/s

# on peut alors definir la fonction d'evolution de la vitesse imposee a la poutre
def imposedVelocity(t):
   # 0 jusqu'au temps 0.005 (repos de la structure)
   if t <= 0.005:
      return 0.
   # croissance lineaire de 0 a la vitesse imposee a la poutre durant l'intervalle [0.005, 0.01]
   elif t > 0.005 and t <= 0.01:
      return v*(t - 0.005)/0.005
   # vitesse maintenue apres 0.01
   else:
      return v

# on peut alors ecrire le fichier d'evolution
pre.writeEvolution(f=imposedVelocity, instants=[0., 0.005, 0.01, 10.] ,path='DATBOX/', name='vx.dat')

# on bloque la translation verticale et la rotation de la poutre
beam.imposeDrivenDof(component=[2, 3], dofty='vlocy')
# on impose la vitesse de la poutre 
beam.imposeDrivenDof(description='evolution', component=1, dofty='vlocy', evolutionFile='vx.dat')

# parametres de la loi cohesive pour le joint:
#   * coefficient de frottement (tire de la these)
mu_joint=0.75
#   * raideur normale (tiree de la these)
kn_joint=8.2e10 # en N/m^3 (i.e. Pa/m)
#   * raideur tangentielle (tiree de la these)
kt_joint=3.6e10 # en N/m^3 (i.e. Pa/m)
# contrainte maximale pour le mode I (traction pure, tiree de la these)
sigmaMaxI_joint=2.5e5 # en Pa = N/m^2
#   * energie consommable pour le mode I (tiree de la these)
GI_joint=1.8e1 # en N/m (i.e. en J/m^2)
# contrainte maximale pour le mode II (cisaillement pur, tiree de la these)
sigmaMaxII_joint=1.4*sigmaMaxI_joint # en Pa = N/m^2
#   * energie consommable pour le mode II (tiree de la these)
GII_joint=1.25e2 # en N/m (i.e. en J/m^2)

# parametres de la loi cohesive pour la rupture des briques:
#   * coefficient de frottement (absent de la these, ...)
mu_brick=0.
#   * raideur normale (tiree de la these)
kn_brick=1.e15 # en N/m^3 (i.e. Pa/m)
#   * raideur tangentielle (tiree de la these)
kt_brick=1.e15 # en N/m^3 (i.e. Pa/m)
# contrainte maximale pour le mode I (traction pure, tiree de la these)
sigmaMaxI_brick=2.5e5 # en Pa = N/m^2
#   * energie consommable pour le mode I (tiree de la these)
GI_brick=8.e1 # en N/m (i.e. en J/m^2)

# gestion des interactions :
#   * declaration des lois
#       - pour le joint entre les briques
lclalpj = pre.tact_behav(name='malc0',law='MAL_CZM',dyfr=mu_joint,stfr=mu_joint,
                         cn=kn_joint,s1=sigmaMaxI_joint,G1=GI_joint,
                         ct=kt_joint,s2=sigmaMaxI_joint,G2=GI_joint)
tacts  += lclalpj
#       - pour la rupture des briques
lclalpb = pre.tact_behav(name='malc1',law='MAL_CZM',dyfr=mu_brick,stfr=mu_brick,
                         cn=kn_brick,s1=sigmaMaxI_brick,G1=GI_brick,
                         ct=kt_brick,s2=sigmaMaxI_brick,G2=GI_brick)
tacts  += lclalpb
#       - avec la poutre et la fondation
lplalp = pre.tact_behav(name='cpld0',law='COUPLED_DOF')
tacts += lplalp
#   * declaration des tables de visibilite
#       - entre briques
#           * joints horizontaux
svclalpH = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx', colorCandidat='HORIx',
                         CorpsAntagoniste='MAILx', antagoniste='ALpxx',colorAntagoniste='HORIx',
                         behav=lclalpj,alert=0.1*min(lcx, lcy))
svs+=svclalpH
#           * joints verticaux
svclalpV = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx', colorCandidat='VERTx',
                         CorpsAntagoniste='MAILx', antagoniste='ALpxx',colorAntagoniste='VERTx',
                         behav=lclalpj,alert=0.1*min(lcx, lcy))
svs+=svclalpV
#           * entre deux demi-briques constituant une brique entiere
svclalpB = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx', colorCandidat='REDxx',
                         CorpsAntagoniste='MAILx', antagoniste='ALpxx',colorAntagoniste='REDxx',
                         behav=lclalpb,alert=0.1*min(lcx, lcy))
svs+=svclalpB
#       - avec la fondation
svcljcf = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx', colorCandidat='HORIx',
                        CorpsAntagoniste='RBDY2', antagoniste='JONCx',colorAntagoniste='WALLx',
                        behav=lplalp, alert=0.1*min(lcx, lcy))
svs+=svcljcf
#       - avec la poutre
svcljcb = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx', colorCandidat='UPxxx',
                        CorpsAntagoniste='RBDY2', antagoniste='JONCx',colorAntagoniste='WALLx',
                        behav=lplalp,alert=0.1*min(lcx, lcy))
svs+=svcljcb

# commandes de post-traitement
#   * cinematique de la poutre
beam_disp = pre.postpro_command(name='BODY TRACKING', step=1, rigid_set=[beam])
post.addCommand(beam_disp)
#   * efforts subis par la poutre
beam_torque = pre.postpro_command(name='TORQUE EVOLUTION', step=1, rigid_set=[beam])
post.addCommand(beam_torque)

# ecriture des fichiers sans les fichiers .INI
#pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)
pre.writeBodies(bodies,chemin='DATBOX/')
pre.writeModels(mods,chemin='DATBOX/')
pre.writeBulkBehav(mats,chemin='DATBOX/',dim=dim)
pre.writeTactBehav(tacts,svs,chemin='DATBOX/')
pre.writeDrvDof(bodies,chemin='DATBOX/')
pre.writePostpro(commands=post, parts=bodies, path='DATBOX/')


try:
  pre.visuAvatars(bodies)
except:
  pass

