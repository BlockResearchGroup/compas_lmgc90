import os

import numpy as np

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# definition des conteneurs:
#   * de corps
bodies = pre.avatars()
#   * de materiaux
mats = pre.materials()
mods = pre.models()
#   * pour les tables de visibilite
svs = pre.see_tables()
#   * pour les lois de contact
tacts = pre.tact_behavs()

# on se place en 3D
dim = 3

hauteur=0.11

liste_objets=[]

# on cree deux materiaux rigides
tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
pdur = pre.material(name='MOUxx',materialType='RIGID',density=100.)
mats.addMaterial(tdur,pdur)

# on cree un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod)

brique=pre.brick3D(name='brique de Paris', lx=0.22, ly=0.11, lz=hauteur)

# colonne de 5 briques
for i in range(0,5):
   # on construit un rigide a partir du maillage :
   #   * le maillage du volume donne la masse et l'inertie
   #   * on extrait le maillage de peau pour constituer le contacteur polyedre
   body = brique.rigidBrick(center=[0., 0., hauteur*(i + 0.5)], model=mod, material=pdur, color='BLEUx')   
   bodies.addAvatar(body)

   if i == 0: 
      liste_objets.append(body) 
      up=body
   elif i == 4:
      liste_objets.append(body) 

# creation d'un corps pour la fondation
brique2=pre.brick3D(name='sol', lx=1., ly=1., lz=0.1)
down = brique2.rigidBrick(center=[0., 0., -0.05], model=mod, material=pdur, color='VERTx')   

#down=rigidPLAN(axe1=0.5, axe2=0.5, axe3=0.05, center=numpy.array([0., 0., -0.05]),
#               model=mod, material=tdur, color='VERTx') 

# blocage de la fondation
down.imposeDrivenDof(component=[1, 3, 4, 5, 6], dofty='vlocy')
down.imposeDrivenDof(description = 'evolution', component = 2, dofty = 'vlocy', evolutionFile = 'vx.dat')

liste_objets.append(down) 

# 
def imposedVelocity(t):
   if t <= 0.1:
      return 0.
   else:
      return 0.16*np.sin(2*2*np.pi*(t-0.1))

#
pre.writeEvolution(f=imposedVelocity, instants=np.linspace(0.,10.,10000), path='DATBOX/', name='vx.dat')

# ajouts de la fondation au conteneur de corps
bodies.addAvatar(down)

#visuAvatars(bodies)

# gestion des interactions :
#   * declaration des lois
#       - entre particules
lprpr=pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.3)
tacts+=lprpr
#       - avec les parois
lprpl=pre.tact_behav(name='iqsc1', law='IQS_CLB', fric=0.5)
tacts+=lprpl
#   * declaration des tables de visibilite
#       - entre particules
svprpr = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLEUx',
                       behav=lprpr, alert=1.e-2)
svs+=svprpr
#       - avec les parois
svprpl = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='VERTx',
                       behav=lprpl, alert=1.e-2)

#svprpl = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR',
#   colorCandidat='BLEUx', behav=lprpl, CorpsAntagoniste='RBDY3', 
#   antagoniste='PLANx', colorAntagoniste='VERTx', alert=1.e-2)
svs+=svprpl

post = pre.postpro_commands()
# suivi du sol :
#   * cinematique de la brique
post.addCommand(pre.postpro_command(name='BODY TRACKING', step=1,rigid_set=liste_objets))
#   * efforts subis par la brique
post.addCommand(pre.postpro_command(name='TORQUE EVOLUTION', step=1,rigid_set=liste_objets))
#
post.addCommand(pre.postpro_command(name='DOUBLETS TORQUE EVOLUTION', step=1, doublets=[(up,down)]))
#
# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass
