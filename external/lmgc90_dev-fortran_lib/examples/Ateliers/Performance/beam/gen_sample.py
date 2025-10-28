import os,sys
import math
from pylmgc90.pre import *

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

lx = 0.25
ly = 1.
lz = 0.25

nbx = 20
nby = int(ly/lx) * nbx
nbz = nbx

dim = 2

ps = avatars()
ms = models()
mx = materials()

mod = model(name='Q4MLx', physics='MECAx', element='H8xxx', dimension=3,  external_model='MatL_', kinematic='small',
            material='elas_', anisotropy='iso__', mass_storage='lump_')
ms.addModel(mod)

acier = material(name='acier', materialType='ELAS', elas='standard',
                 young=2.e11, nu=0.3, anisotropy='isotropic',
                 density=0.)
mx.addMaterial(acier)

# recuperation du maille
beam = avatar(dimension=dim)
beam_mesh = buildMeshH8(x0=0., y0=0., z0=0., lx=lx, ly=ly, lz=lz, nb_elem_x=nbx, nb_elem_y=nby, nb_elem_z=nbz)

# on cosntruit un avatar maille pour la poutre
beam=buildMeshedAvatar(mesh=beam_mesh, model=mod, material=acier)

## Application des conditions initiales
beam.imposeInitValue(group='all', component=[1, 2, 3], value=[0., 0., 0.])

# Application des conditions aux limites
#   * encastrement
beam.imposeDrivenDof(group='left', component=[1, 2, 3], dofty='vlocy')
#   * appui
beam.imposeDrivenDof(group='right', component=2, dofty='vlocy', ct=-1.e-4, rampi=1.)

# Ajout des parties dans le conteneur de parties
ps.addAvatar(beam)

# Ecriture des fichiers pour LMGC
writeBodies(ps,chemin='DATBOX/')
writeModels(ms,chemin='DATBOX/')
writeDrvDof(ps,chemin='DATBOX/')
writeDofIni(ps,chemin='DATBOX/')
writeGPVIni(ps,chemin='DATBOX/')
writeBulkBehav(mx,chemin='DATBOX/', gravy=[0., 0., 0.])

post = postpro_commands()
writePostpro(commands=post, parts=ps, path='DATBOX/')

try:
  visuAvatars(ps)
except:
  pass
