import os,sys
import math

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

dim = 2

# definition du conteneur de partie ou de pieces, de modeles, de materiaux et de commandes de post-traitement
ps = pre.avatars()
ms = pre.models()
mx = pre.materials()
post = pre.postpro_commands()

# modele utilisateur, hpp
mod = pre.model(name='Q4MLx', physics='MECAx', element='Q4xxx', dimension=2,material='elasd',
                external_model='MatL_', kinematic='small', mass_storage='coher',  anisotropy='iso__',
                external_fields=['TEMPERATURE'])
ms.addModel(mod)

# Definition des materiaux
acier = pre.material(name='acier', materialType='ELAS_DILA', density=0., elas='standard', young=0.1e+15, nu=0.2, anisotropy='isotropic',
                    dilatation=1e-5, T_ref_meca=20. )
mx.addMaterial(acier)

# on genere le maillage de la poutre
beam_mesh = pre.buildMesh2D(mesh_type='Q4', x0=0., y0=0., lx=0.0032, ly=0.0324, nb_elem_x=6, nb_elem_y=36)

# on cosntruit un avatar maille pour la poutre
beam = pre.buildMeshedAvatar(mesh=beam_mesh, model=mod, material=acier)

# Application des conditions aux limites
#   * serrage (v_y=0 en haut et en bas)
beam.imposeDrivenDof(group='up', component=2, dofty='vlocy')
beam.imposeDrivenDof(group='down', component=2, dofty='vlocy')
#   * blocage de la transaltion suivant x (v_x=0 en un point)
# predicat
def p(x):
   return abs(x[0] - 0.0016) < 5.e-4
# creation d'un nouveau groupe
beam.addGroupUsingPredicate(name='amarre', predicate=p, super_group='down')
beam.imposeDrivenDof(group='amarre', component=1, dofty='vlocy')

# Ajout des parties dans le conteneur de parties
ps.addAvatar(beam)

post = pre.postpro_commands()

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mx, ms, ps, post=post, gravy=[0., 0., 0.])

try:
  pre.visuAvatars(ps)
except:
  pass
