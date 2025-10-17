import sys
from pathlib import Path

import numpy as np

from pylmgc90 import pre

dim = 2


#############
Friction          = 0.00
Friction_wall     = 0.00

#############
Rmin              = 0.05
Rmax              = 0.15

lx = 50*Rmax
ly = 10*Rmax
nb_particles      = 1000

#############
datbox_path = Path('Press/DATBOX')
datbox_path.mkdir(parents=True, exist_ok=True)

#############

############## 
bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

############## creations de deux materiaux
# Note : these are in fact the same materials, just to make a difference between grains and walls
tdur = pre.material(name='TDURx',materialType='RIGID',density=2800.)
plex = pre.material(name='PLEXx',materialType='RIGID',density=2800.)
mats.addMaterial(tdur,plex)

############## rigid model creation
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mods.addModel(mod)

############## granulo anb box building
radii=pre.granulo_Uniform(nb_particles, Rmin, Rmax)
Rmoyen = (Rmax+Rmin)/2

nb_remaining_particles, coor, radii = pre.depositInBox2D(radii, lx, ly)

############## loop adding particles:
for r, c in zip(radii, coor):
    body=pre.rigidDisk( model=mod, material=plex, center=c, r=r, color='BLEUx')
    #
    bodies += body

############## four walls creation with tdur material
down=pre.roughWall(center=[0.5*lx, -Rmin], l=lx, r=Rmin, model=mod, material=tdur, color='WALLx')
down.imposeDrivenDof(component=1, dofty='vlocy')
down.imposeDrivenDof(component=2, dofty='vlocy')
down.imposeDrivenDof(component=3, dofty='vlocy')
bodies += down

up   = pre.roughWall(center=[0.5*lx, ly+Rmin], l=lx, r=Rmin, model=mod, material=tdur, color='WALLx')
up.imposeDrivenDof(component=1, dofty='vlocy')
up.imposeDrivenDof(component=2, dofty='force',ct=-100000.,rampi=1.)
up.imposeDrivenDof(component=3, dofty='vlocy')
bodies += up

############## interactions management:
#   * law declarations
#       - particles vs particles and particles va walls
l1=pre.tact_behav(name='iqsc1',law='IQS_CLB',fric=Friction)
tacts+=l1
l2=pre.tact_behav(name='iqsc2',law='IQS_CLB',fric=Friction_wall)
tacts+=l2

#   * visibility tables declaration
#       - between particles of type (disk bleu) vs (disk bleu)
svdkdk = pre.see_table(CorpsCandidat='RBDY2'   , candidat   ='DISKx', colorCandidat   ='BLEUx', behav=l1,
                       CorpsAntagoniste='RBDY2', antagoniste='DISKx', colorAntagoniste='BLEUx', alert=Rmin)
svs+=svdkdk
#       - between particles of type (disk bleu) vs (disk wall)
svdkjc = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='BLEUx', behav=l2,
                        CorpsAntagoniste='RBDY2', antagoniste='DISKx', colorAntagoniste='WALLx', alert=Rmin)
svs+=svdkjc


# file writing
post = pre.postpro_commands()
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post, datbox_path=datbox_path, gravy=[0.,0.,0.])

if '--with-visu' in sys.argv:
  try:
    pre.visuAvatars(bodies)
  except:
    pass

