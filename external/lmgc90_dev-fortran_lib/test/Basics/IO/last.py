
import math
from pathlib import Path

import numpy as np

from pylmgc90 import pre, chipy

import utils

pre.setStopMode('exception')

datbox = Path('DATBOX')
datbox.mkdir(exist_ok=True)

bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
tacts  = pre.tact_behavs()
sees   = pre.see_tables()

dim = 2

# simple sample generation like 1DK_BoxJC
rmod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

mods.addModel(rmod)

# materials creation
rmat1 = pre.material(name='TDURx', materialType='RIGID', density=1000.)
rmat2 = pre.material(name='PDURx', materialType='RIGID', density=100. )

mats.addMaterial(rmat1)
mats.addMaterial(rmat2)

# adding one diskx:
rd = 0.2
lx = 3*rd
ly = 4*rd
disk = pre.rigidDisk(r=rd, center=[0.,5*rd], model=rmod, material=rmat2, color='DISKx')

# adding two joncx:
jonc1 = pre.rigidJonc(axe1=lx, axe2=rd*0.1, center=[0., -rd*0.1], model=rmod, material=rmat1, color='JONCx')
jonc2 = pre.rigidJonc(axe1=lx, axe2=rd*0.1, center=[0., 0.], model=rmod, material=rmat1, color='JONCx')
jonc2.rotate(psi=math.pi*0.5)
jonc2.translate(dx=lx,dy=ly)

#boundary condition and initial values
disk.imposeInitValue(component=1, value=1.5)
jonc1.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
jonc2.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

for b in (disk, jonc1, jonc2):
     bodies.addAvatar(b)
 
# generating visibility tables for all these
iqsc0 = pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.5)
tacts.addBehav(iqsc0)

# dkjc
sv1 = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='DISKx',
                    CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='JONCx',
                    behav=iqsc0, alert=0.05)
sees.addSeeTable(sv1)

# file writing
pre.writeDatbox( dim, mats, mods, bodies, tacts, sees )


# Run a computation with this case !

# space dimension
dim = 2
# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 1
deformable = False
# time evolution parameters
dt = 5e-2
nb_steps = 50
# theta integrator parameter
theta = 0.501
# interaction parameters
Rloc_tol = 5.e-3
# nlgs parameters
solver_param = { 'conv'  : 1e-4   ,
                 'relax' : 1.0    ,
                 'norm'  : 'Quad ',
                 'gsit1' : 2      ,
                 'gsit2' : 10     ,
                 'stype' : 'Stored_Delassus_Loops         '
               }
# write parameter
freq_write   = 1
# display parameters
freq_display = 1

h5_file = 'last_all.h5'
h5_last = 'last_last.h5'
h5_half = 'last_half.h5'

#initialize
utils.init_lmgc90(dt, theta, dim, mhyp, deformable, h5_file)
for k in range(nb_steps):
  utils.compute_lmgc90_one_step(solver_param, freq_write, freq_display)
  if k == nb_steps//4:
    chipy.WriteLast(h5_half)
chipy.WriteLast(h5_last)
utils.finalize_lmgc90()


#mats2, mods2, bodies2, tacts2, sees2, inters2 = pre.readDatbox(dim, 'DATBOX')
#inters2 = pre.readState(bodies2, 'OUTBOX', 1, 'last_last.h5', tacts2)
#pre.visuAvatars()

import h5py
hf = h5py.File(h5_file, 'r')
hh = h5py.File(h5_half, 'r')
hl = h5py.File(h5_last, 'r')

gr = lambda i: f"Evolution/ID_{i}/RBDY2/rdata"

assert np.all( hf[gr(50)][()] == hl[gr(1)][()] ), 'difference between last and last step of all'
assert np.all( hf[gr(13)][()] == hh[gr(1)][()] ), 'difference between half and half step of all'
