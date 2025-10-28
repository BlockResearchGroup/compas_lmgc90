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
tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
pdur = pre.material(name='MOUxx',materialType='RIGID',density=100.)
mats.addMaterial(tdur,pdur)


# create a model of rigid
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod)

#create some bodies
left = pre.rigidPlan(axe1=0.5 , axe2=1. , axe3=0.01, center=[0.,0.,0.], material=tdur, model=mod, color='VERTx')
right= pre.rigidPlan(axe1=0.5 , axe2=1. , axe3=0.01, center=[0.,0.,0.], material=tdur, model=mod, color='VERTx')
down = pre.rigidPlan(axe1=0.5 , axe2=0.5, axe3=0.01, center=[0.,0.,0.], material=tdur, model=mod, color='VERTx')
rear = pre.rigidPlan(axe1=0.25, axe2=0.5, axe3=0.01, center=[0.,0.,0.], material=tdur, model=mod, color='VERTx')
front= pre.rigidPlan(axe1=0.25, axe2=0.5, axe3=0.01, center=[0.,0.,0.], material=tdur, model=mod, color='VERTx')

spher= pre.rigidSphere(r=0.05, center=[0.,0.,0.], material=pdur, model=mod)


bodies.addAvatar(left)
bodies.addAvatar(right)
bodies.addAvatar(front)
bodies.addAvatar(rear)
bodies += down
bodies += spher

#arranging our bodies to fit our case
left.translate(dy=-0.5,dz=1.)
right.translate(dy=0.5,dz=1.)
rear.translate(dx=-0.5,dz=0.25)
front.translate(dx=0.5,dz=0.25)
spher.translate(dz=1.75)

# rotation around x-axis, with respect to the mass center and a rotation angle
# -pi/2 (parameters : Euler angles) 
left.rotate(theta=-0.5*math.pi, center=left.nodes[1].coor)
# rotation around x-axis, with respect to the mass center and a rotation angle
# pi/2 (parameters : Euler angles) 
right.rotate(theta=0.5*math.pi, center=right.nodes[1].coor)
# rotation around y-axis, with respect to the mass center and a rotation angle
# pi/2 (parameters : axis + angle) 
rear.rotate(description='axis', alpha=0.5*math.pi, axis=[0., 1., 0.], center=rear.nodes[1].coor)
# rotation around y-axis, with respect to the mass center and a rotation angle
# -pi/2 (parameters : axis + angle) 
front.rotate(description='axis', alpha=-0.5*math.pi, axis=[0., 1., 0.], center=front.nodes[1].coor)

# impose 0 velocity on walls
left.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
right.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
down.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
rear.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
front.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')

## set initial horizontal velocity on the disk
spher.imposeInitValue(component=[1, 2], value=[1.0, 3.0])

#b=tact_behav('rstc1','RST_CLB',fric=0.3,rstn=0.9,rstt=0.5)
#tacts+=b
b = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts += b

#interactions
sv = pre.see_table(CorpsCandidat   ='RBDY3',candidat   ='SPHER',colorCandidat   ='BLUEx',
                   CorpsAntagoniste='RBDY3',antagoniste='PLANx',colorAntagoniste='VERTx',
                   behav=b,alert=.1)
svs+=sv
sv = pre.see_table(CorpsCandidat   ='RBDY3',candidat   ='SPHER',colorCandidat   ='BLUEx',
                   CorpsAntagoniste='RBDY3',antagoniste='SPHER',colorAntagoniste='BLEUx',
                   behav=b,alert=.1)
svs+=sv

post = pre.postpro_commands()
my_command = pre.postpro_command(name='NEW RIGID SETS', step=1, rigid_sets=[[spher], [left, down, right]])
post.addCommand(my_command)

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass
