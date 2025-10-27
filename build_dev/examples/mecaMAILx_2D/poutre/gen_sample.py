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

# modele alastique, lineaire, hpp
mod = pre.model(name='Q4MLx', physics='MECAx', element='Q4xxx', dimension=2,  external_model='MatL_', kinematic='small',
                material='elas_', anisotropy='iso__', mass_storage='lump_')
ms.addModel(mod)

# Definition des materiaux
acier = pre.material(name='acier', materialType='ELAS', elas='standard',
                     young=2.e11, nu=0.3, anisotropy='isotropic',
                     density=0.)
mx.addMaterial(acier)

# recuperation du maille
beam = pre.avatar(dimension=dim)
beam_mesh = pre.readMesh('gmsh/poutre.msh', dim)

# on cosntruit un avatar maille pour la poutre
beam = pre.buildMeshedAvatar(mesh=beam_mesh, model=mod, material=acier)

## Application des conditions initiales
beam.imposeInitValue(group='all', component=[1, 2], value=[0., 0.])

# Application des conditions aux limites
#   * encastrement
beam.imposeDrivenDof(group='7', component=[1, 2], dofty='vlocy')
#   * appui
beam.imposeDrivenDof(group='8', component=1, dofty='vlocy')
beam.imposeDrivenDof(group='8', component=2, dofty='vlocy', ct=-1.e-4, rampi=1.)

# Ajout des parties dans le conteneur de parties
ps.addAvatar(beam)

post = pre.postpro_commands()

# post-traitement : deplacement d'un point de la poutre en flexion
# definition des sets pour suivre noeud oppose au noeud ou est impose la
# condition limite, par rapport a l'axe median :
#   * definition des sets
mecax_set_free = [(beam, "9")]
#   * creation de la commande
beam_sets = pre.postpro_command(name='NEW MECAx SETS', mecax_sets=[mecax_set_free])
post.addCommand(beam_sets)

# on suit leur deplacement
beam_disp = pre.postpro_command(name='Dep EVOLUTION', step=1)
post.addCommand(beam_disp)

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mx, ms, ps, post=post, gravy=[0., 0., 0.])

try:
  pre.visuAvatars(ps)
except:
  pass
