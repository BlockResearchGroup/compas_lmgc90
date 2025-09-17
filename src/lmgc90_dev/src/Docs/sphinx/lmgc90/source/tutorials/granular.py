from pathlib import Path
import math

from pylmgc90 import pre

nb_particles = 10000
radius_min   = 1.0
radius_max   = 2.5
radii = pre.granulo_Random(nb_particles, radius_min, radius_max)

lx = 150.
ly = 100.
nb_laid_particles, coors, radii = pre.depositInBox2D(radii,lx,ly)


mat = pre.material(name='TDURx', materialType='RIGID', density=100.)
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=2)

# generate the triangles
bodies = pre.avatars()
nb_vertices = 3
for r, c in zip(radii, coors):
  body = pre.rigidPolygon(radius=r, center=c, nb_vertices=nb_vertices, model=mod, material=mat, color='BLUEx')
  bodies.addAvatar(body)


max_radius = max(radii)
mut    = pre.material(name='TDURx', materialType='RIGID', density=1000.)

# left wall : rough wall
left   = pre.roughWall(center=[-radius_max, 0.5*ly], theta=-0.5*math.pi, l=ly + 2.*radius_max,
                       r=radius_max, model=mod, material=mut, color='WALLx')
left.imposeDrivenDof(component=[1,2,3], dofty='vlocy')
bodies.addAvatar(left)

# right wall : not too rough wall
right  = pre.fineWall(center=[lx+radius_max, 0.5*ly], theta= 0.5*math.pi, l=ly + 2.*radius_max,
                      r=radius_max, model=mod, material=mut, color='WALLx')
right.imposeDrivenDof(component=[1,2,3], dofty='vlocy')
bodies.addAvatar(right)

# bottom wall : small wall (why not use a JONCx ?)
bottom = pre.smoothWall(center=[0.5*lx, -radius_max], theta=0., l=lx + 2.*radius_max,
                        h=2.*radius_max, nb_polyg=12, model=mod, material=mut, color='WALLx')
bottom.imposeDrivenDof(component=[1,2,3], dofty='vlocy')
bodies.addAvatar(bottom)

try:
  pre.visuAvatars(bodies)
except:
  pass

mats = pre.materials()
mats.addMaterial(mat,mut)
mods = pre.models()
mods.addModel(mod)
svs   = pre.see_tables()
tacts = pre.tact_behavs()

# interaction definition:
lplpl  = pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.3)
tacts += lplpl
svplpl = pre.see_table(CorpsCandidat='RBDY2', candidat='POLYG', colorCandidat='BLUEx',
                       CorpsAntagoniste='RBDY2', antagoniste='POLYG', colorAntagoniste='BLUEx',
                       behav=lplpl, alert=.1)
tacts += svplpl
svdkpl = pre.see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='BLUEx',
                       CorpsAntagoniste='RBDY2', antagoniste='POLYG', colorAntagoniste='WALLx',
                       behav=lplpl, alert=.1)
svs += svdkpl

post = pre.postpro_commands()

# files writing
datbox = Path('./DATBOX')
datbox.mkdir(exist_ok=True)


pre.writeDatbox(2, mats, mods, bodies, tacts, svs, post=post, datbox_path=datbox)

