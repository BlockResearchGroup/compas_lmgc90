import os
import math

import numpy

from pylmgc90 import pre

# ecriture des fichiers
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# on se place en 2D
dim = 2
cell_length = 150.
cell_width  = 150.
# creation des conteneurs
bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()
e_joint=0.001
# creations de deux materiaux
tdur = pre.material(name='TDURx',
                    materialType='RIGID',
                    density=1000.)
stone = pre.material(name='STONE',
                     materialType='RIGID',
                     density=1500.)

mats.addMaterial(tdur,stone)

# on cree un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mods.addModel(mod)

v = numpy.array([[-12.5,-12.5], [12.5,-12.5], [12.5,12.5], [-12.5,12.5]],'d')

poly1 = pre.rigidPolygon(model=mod,material=stone,center=[3.*cell_length/4.,cell_width/2.],generation_type='full',vertices=v,color='REDxx')

poly2 = pre.rigidPolygon(model=mod,material=stone,center=[0.,0.],generation_type='full',vertices=v,color='REDxx')
poly2.translate(dx=4.*cell_length/7.,dy=cell_width/2.)


bodies.addAvatar(poly1)
bodies.addAvatar(poly2)

r_ext       = cell_width/2.
e_bloc      = 1.

poly1.imposeInitValue(component=[3],value=[4])

# # gestion des interactions :
# #   * declaration des lois
lplpl = pre.tact_behav(name='iqsc0',law='IQS_CLB_g0',fric=0.01)
tacts+= lplpl

# #   * declaration des tables de visibilite
svplpl = pre.see_table(CorpsCandidat='RBDY2', candidat='POLYG', colorCandidat='REDxx',
                       CorpsAntagoniste='RBDY2', antagoniste='POLYG', colorAntagoniste='REDxx',
                       behav='iqsc0', alert=e_joint)
svs+=svplpl

post = pre.postpro_commands()


# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post, gravy=[0., 0., 0.])

try:
  pre.visuAvatars(bodies)
except:
  pass
