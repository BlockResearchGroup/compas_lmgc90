import os
import math

import numpy

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# on se place en 2D
dim = 2

# creation des conteneurs
bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

# creations de deux materiaux
tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
stone = pre.material(name='STONE',materialType='RIGID',density=1500.)
mats.addMaterial(tdur,stone)

# on cree un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mods.addModel(mod)

# parametres du script:
# nombre de blocs, ouverture angulaire et rayons int/ext
nb_blocs    = 11 
theta_joint = math.pi/100.
r_int       = 0.8
r_ext       = 1.

# ouverture d'un bloc, epaisseur d'un bloc et epaisseur du join
theta_bloc = (math.pi - (nb_blocs - 1)*theta_joint)/nb_blocs
e_bloc     = r_ext - r_int
e_joint    = r_ext*theta_joint

# on initialise l'angle de premier bloc a 0
theta=0.
for i in range(0, nb_blocs):
   vertices=numpy.zeros([4, 2], 'd')
   vertices[0, 0]=r_int*math.cos(theta);              vertices[0, 1]=r_int*math.sin(theta)
   vertices[1, 0]=r_ext*math.cos(theta);              vertices[1, 1]=r_ext*math.sin(theta)
   vertices[2, 0]=r_ext*math.cos(theta + theta_bloc); vertices[2, 1]=r_ext*math.sin(theta + theta_bloc)
   vertices[3, 0]=r_int*math.cos(theta + theta_bloc); vertices[3, 1]=r_int*math.sin(theta + theta_bloc)

   block = pre.rigidPolygon(model=mod, material=stone, center=numpy.array([0., 0.]), color='REDxx', 
                            generation_type='full',vertices=vertices)

   bodies.addAvatar(block)
   
   theta += theta_bloc + theta_joint

# ajout d'une fondation, i.e. faite d'un jonc :
down = pre.rigidJonc(model=mod, material=tdur, center=numpy.array([0., -0.25*e_bloc]),
                     color='GREEN', axe1=r_ext + 0.5*e_bloc, axe2=0.25*e_bloc)
bodies.addAvatar(down)

down.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

#pre.visuAvatars(bodies)

# gestion des interactions :
#   * declaration des lois
lplpl = pre.tact_behav(name='iqsc0',law='IQS_CLB_g0',fric=0.5)
tacts+= lplpl
lpljc = pre.tact_behav(name='iqsc1',law='IQS_CLB',fric=0.5)
tacts+= lpljc
#   * declaration des tables de visibilite
svplpl = pre.see_table(CorpsCandidat='RBDY2', candidat='POLYG', colorCandidat='REDxx',
                       CorpsAntagoniste='RBDY2', antagoniste='POLYG', colorAntagoniste='REDxx',
                       behav='iqsc0', alert=e_joint)
svs+=svplpl
svpljc = pre.see_table(CorpsCandidat='RBDY2', candidat='POLYG', colorCandidat='REDxx',
                       CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='GREEN',
                       behav='iqsc1', alert=e_joint)
svs+= svpljc

post = pre.postpro_commands()
nlgs = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(nlgs)

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass
