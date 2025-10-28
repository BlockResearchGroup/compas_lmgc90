
import sys
from pathlib import Path

import numpy as np

from pylmgc90 import pre

datbox = Path('./DATBOX')
datbox.mkdir(exist_ok=True)

if '--norand' in sys.argv:
  seed = 1
else:
  seed = None

# FIRST GENERATION

dim = 2

bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
plex = pre.material(name='PLEXx',materialType='RIGID',density=100.)
mats.addMaterial(tdur,plex)

mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mods.addModel(mod)

# particles
nb_particles = 250
r_min = 0.5
r_max = 2.
radii = pre.granulo_Random(nb_particles, r_min, r_max, seed)

# rectangular box deposit
lx = 50.
ly = 25. 
nb_laid_particles, coor, radii = pre.depositInBox2D(radii, lx, ly)

# adding disks
for r, c in zip(radii, coor):
   body = pre.rigidDisk(r=r, center=c, model=mod, material=plex, color='BLEUx') 
   bodies += body


# adding box
down = pre.rigidJonc(axe1=0.5*lx+r_max, axe2=r_max, center=[0.5*lx, -r_max],
                     model=mod, material=tdur, color='WALLx')
up   = pre.rigidJonc(axe1=0.5*lx+r_max, axe2=r_max, center=[0.5*lx, ly+r_max],
                     model=mod, material=tdur, color='WALLx')
left = pre.rigidJonc(axe1=0.5*ly+r_max, axe2=r_max, center=[-r_max, 0.5*ly],
                     model=mod, material=tdur, color='WALLx')
right= pre.rigidJonc(axe1=0.5*ly+r_max, axe2=r_max, center=[lx+r_max, 0.5*ly],
                     model=mod, material=tdur, color='WALLx')

bodies += down; bodies += up; bodies += left; bodies += right

# orienting box
left.rotate(psi=-np.pi/2., center=left.nodes[1].coor)
right.rotate(psi=np.pi/2., center=right.nodes[1].coor)

# null velocity drvdof
down.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
up.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
left.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
right.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

# contact laws
ldkdk = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts+= ldkdk
ldkjc = pre.tact_behav(name='iqsc1',law='IQS_CLB',fric=0.5)
tacts+= ldkjc

# and see tables
svdkdk = pre.see_table(CorpsCandidat='RBDY2',candidat='DISKx', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY2', antagoniste='DISKx',colorAntagoniste='BLEUx',
                       behav=ldkdk, alert=0.1*r_min)
svs+=svdkdk
svdkjc = pre.see_table(CorpsCandidat='RBDY2',candidat='DISKx', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY2', antagoniste='JONCx',colorAntagoniste='WALLx',
                       behav=ldkjc,alert=0.1*r_min)
svs+= svdkjc

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)


# COMPUTATION

from pylmgc90 import chipy
from pylmgc90.chipy import computation

n_step= 10
dt    = 1e-3
theta = 0.5

stype = 'Stored_Delassus_Loops         '
norm  = 'Quad '
tol   = 1e-4
relax = 1.0
gs_it1= 100
gs_it2= 50

f_write = 1
f_disp  = 1

computation.initialize(dim, dt, theta, logmes=True)
for i_step in range(n_step):
  computation.one_step(stype, norm, tol, relax, gs_it1, gs_it2, f_write, f_disp)
computation.finalize()


# READING with graph

mats, mods, bodies, tacts, sees, inters, ginters = pre.readDatbox(dim, datbox_path="./OUTBOX", step=10, with_graph=True)

# select right half of the bodies
selection = []
for ib, b in enumerate(bodies):
  if b.getNodeCoor()[0] > lx/2:
    selection.append(ib)

# generate subgraph and new bodies container
selection = ginters.vs.select(selection)
ghalf = ginters.subgraph(selection)
bodies2 = pre.avatars(ghalf.vs['avatar'])

# rewrite datbox
pre.writeDatbox(dim, mats, mods, bodies2, tacts, svs, ghalf, datbox_path='DATBOX')

# and check
computation.initialize(dim, dt, theta, logmes=True)
computation.one_step(stype, norm, tol, relax, gs_it1, gs_it2, f_write, f_disp)
nb_recup = 0
for ctc_id in [chipy.DKDKx_ID, chipy.DKJCx_ID]:
    nb_recup += chipy.inter_handler_2D_getNbRecup(ctc_id)

assert nb_recup == len(ghalf.es)

computation.finalize()

