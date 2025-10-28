import os,sys
import math
from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

dim = 2

# definition des conteneurs

ps = pre.avatars()
ms = pre.models()
mx = pre.materials()
svs= pre.see_tables()
tacts = pre.tact_behavs()

# modele alastique, lineaire, hpp
mod = pre.model(name='T3MLx', physics='MECAx', element='T3xxx', dimension=2,  external_model='MatL_', kinematic='small',
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
beam.imposeDrivenDof(group='8', component=2, dofty='vlocy', ct=-1.e-4, rampi=1.)

beam.addContactors(group='101',shape='ALpxx',color='xxxxx',reverse=True)
beam.addContactors(group='102',shape='CLxxx',color='yyyyy',weights=[0.25,0.75],reverse=True)

# Ajout des parties dans le conteneur de parties
ps.addAvatar(beam)

# Definition des interactions et des tables de visibilites
#.. table de visibilite
sv = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx',colorCandidat='yyyyy',
               CorpsAntagoniste='MAILx',antagoniste='ALpxx',colorAntagoniste='xxxxx',
               behav='gapc1',alert=0.01)

svs += sv

#...interaction
b = pre.tact_behav('gapc1','GAP_SGR_CLB',fric=0.9)
tacts += b


post = pre.postpro_commands()

# post-traitement : deplacement d'un point de la poutre en flexion
# definition des sets pour suivre noeud oppose au noeud ou est impose la
# condition limite, par rapport a l'axe median :
#   * definition des sets
mecax_set_free = [(beam, "8")]
#   * creation de la commande
beam_sets = pre.postpro_command(name='NEW MECAx SETS', mecax_sets=[mecax_set_free])
post.addCommand(beam_sets)

# on suit leur deplacement
beam_disp = pre.postpro_command(name='Dep EVOLUTION', step=1)
post.addCommand(beam_disp)

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mx, ms, ps, tacts, svs, post=post, gravy=[0., 0., 0.])
