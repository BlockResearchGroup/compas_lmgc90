import os,sys

import numpy
import math

from pylmgc90 import pre


if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# WARNING : in 3D by default z-axis is upward
# this is very important to direct PLANx objects

dim = 3

bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

#create materials
tdur = pre.material(name='BOXxx',materialType='RIGID',density=580.)
pdur = pre.material(name='PLEX3',materialType='RIGID',density=580.)
mats.addMaterial(tdur,pdur)


# create a model of rigid
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod)

#create some bodies
spher= pre.rigidSphere(r=1e-2, center=[1.5e-2,1.5e-2,1.e-1], material=pdur, model=mod, color='BLEUx')
bodies.addAvatar(spher)

ax1 = 9e-2
ax2 = 7e-2
ax3 = 4e-3

box = pre.rigidPlan(axe1=ax1 , axe2=ax2, axe3=ax3, center=[0.,0.,0.], material=tdur, model=mod, color='VERTx')
box.addContactors(shape='PLANx', axe1=ax2 , axe2=ax2 , axe3=ax3, color='VERTx',
                  shift=[ ax1, 0., ax2], frame=[[0.,0.,1.],[0.,1.,0.],[-1.,0.,0]])
box.addContactors(shape='PLANx', axe1=ax2 , axe2=ax2 , axe3=ax3, color='VERTx',
                  shift=[-ax1, 0., ax2], frame=[[0.,0.,-1.],[0.,1.,0.],[1.,0.,0]])
box.addContactors(shape='PLANx', axe1=ax1 , axe2=ax2 , axe3=ax3, color='VERTx',
                  shift=[ 0, ax2, ax2], frame=[[1.,0.,0.],[0.,0.,1.],[0.,-1.,0]])
box.addContactors(shape='PLANx', axe1=ax1 , axe2=ax2 , axe3=ax3, color='VERTx',
                  shift=[ 0,-ax2, ax2], frame=[[1.,0.,0.],[0.,0.,-1.],[0.,1.,0]])

box.computeRigidProperties()
bodies.addAvatar(box)

# impose 0 velocity on walls
box.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

# set initial horizontal velocity on the disk
spher.imposeInitValue(component=[1, 2], value=[1.0, 3.0])


b = pre.tact_behav('rstc1','RST_CLB',fric=0.,rstn=0.1,rstt=0.)
tacts += b

##interactions
sv = pre.see_table(CorpsCandidat   ='RBDY3',candidat   ='SPHER',colorCandidat   ='BLEUx',
                   CorpsAntagoniste='RBDY3',antagoniste='PLANx',colorAntagoniste='VERTx',
                   behav=b,alert=1e-6)
svs+=sv

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)

try:
  pre.visuAvatars(bodies)
except:
  pass
