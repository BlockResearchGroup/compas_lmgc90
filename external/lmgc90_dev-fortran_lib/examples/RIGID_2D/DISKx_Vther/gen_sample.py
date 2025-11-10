
import sys
from pathlib import Path

import math

from pylmgc90 import pre

# ecriture des fichiers
datbox_path = Path('DATBOX')
datbox_path.mkdir(exist_ok=True)

if '--norand' in sys.argv:
  seed = 1
else:
  seed = None

dim = 2

nb_particles = 1000
radius_min   = 1.0
radius_max   = 2.5
radii = pre.granulo_Random(nb_particles, radius_min, radius_max, seed)

lx = 50.
ly = 50.
nb_laid_particles, coors, radii = pre.depositInBox2D(radii,lx,ly)


mat = pre.material(name='PDURx', materialType='RIGID', density=100.)
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

# generate the triangles
bodies = pre.avatars()

for r, c in zip(radii, coors):
  body = pre.rigidDisk(r=r, center=c, model=mod, material=mat, color='BLUEx')
  bodies.addAvatar(body)

max_radius = max(radii)

mut    = pre.material(name='TDURx', materialType='RIGID', density=1000.)

# 
left   = pre.rigidJonc(axe1=ly, axe2=radius_max, center=[-radius_max, 0.5*ly], model=mod, material=mut, color='WALLx')
left.imposeDrivenDof(component=[1,2,3], dofty='vlocy')
left.rotate(description='axis', alpha=math.pi/2., axis=[0., 0., 1.], center=[-radius_max, 0.5*ly])
bodies.addAvatar(left)


right  = pre.rigidJonc(axe1=ly, axe2=radius_max, center=[lx+radius_max, 0.5*ly], model=mod, material=mut, color='WALLx')
right.imposeDrivenDof(component=[1,2,3], dofty='vlocy')
right.rotate(description='axis', alpha=-math.pi/2., axis=[0., 0., 1.], center=[lx+radius_max, 0.5*ly])
bodies.addAvatar(right)


bottom = pre.rigidJonc(axe1=lx, axe2=radius_max, center=[0.5*lx, -radius_max], model=mod, material=mut, color='WALLx')
bottom.imposeDrivenDof(component=[1,2,3], dofty='vlocy')
bodies.addAvatar(bottom)

#try:
#  pre.visuAvatars(bodies)
#except:
#  pass

mats = pre.materials()
mats.addMaterial(mat,mut)
mods = pre.models()
mods.addModel(mod)
svs   = pre.see_tables()
tacts = pre.tact_behavs()

# interaction definition:
ldkdk = pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.3)
tacts+= ldkdk

ldkjc = pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.3)
tacts+= ldkjc

svdkdk = pre.see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='BLUEx',
                       CorpsAntagoniste='RBDY2', antagoniste='DISKx', colorAntagoniste='BLUEx',
                       behav=ldkdk, alert=1.)
svs+=svdkdk

svdkjc = pre.see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='BLUEx',
                       CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='WALLx',
                       behav=ldkjc, alert=.1)
svs+=svdkjc


post = pre.postpro_commands()

# files writing
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)
