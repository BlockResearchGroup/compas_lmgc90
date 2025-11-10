import os,sys
import numpy
import math

from pylmgc90 import pre

# working directory:
if not os.path.isdir('generate'):
  os.mkdir('generate')

wd = os.path.join('generate','DATBOX')
if not os.path.isdir(wd):
  os.mkdir(wd)

# 2D example
dim = 2

# containers definitions:
#   * for avatars
bodies = pre.avatars()
#   * for materials
mat = pre.materials()
#   * for see tables
svs = pre.see_tables()
#   * for contact laws
tacts = pre.tact_behavs()

#create materials
tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
pdur = pre.material(name='MOUxx',materialType='RIGID',density=100.)
mat.addMaterial(tdur,pdur)

# create a model of rigid
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

down = pre.rigidJonc(axe1=0.5,axe2=0.01,center=[0.,0.],model=mod,material=tdur,color='VERTx')
left = pre.rigidJonc(axe1=1. ,axe2=0.01,center=[0.,0.],model=mod,material=tdur,color='VERTx')
right= pre.rigidJonc(axe1=1. ,axe2=0.01,center=[0.,0.],model=mod,material=tdur,color='VERTx')
diskx= pre.rigidDisk(r=0.05,center=[0.,0.],model=mod,material=pdur,color='BLEUx')

# add bodies in the bodies cotainer
bodies.addAvatar(left)
bodies.addAvatar(right)
bodies += down
bodies += diskx

#arranging our bodies to fit our case
left.translate(dx=-0.5,dy=1.)
right.translate(dx=0.5,dy=1.)
diskx.translate(dy=1.75)

# WARNING: since we want to rotate with respect to its own center
#   we provide teh center of the body as the center of rotation 
# rotation using Euler's angles
left.rotate(psi=-math.pi/2., center=left.nodes[1].coor)
# rotation using an axis and an angle
right.rotate(description='axis', alpha=math.pi/2., axis=[0., 0., 1.], center=right.nodes[1].coor)

try:
  pre.visuAvatars(bodies)
except:
  pass

# impose 0 velocity on walls
left.imposeDrivenDof(component=[1,2,3],dofty='vlocy')
right.imposeDrivenDof(component=[1,2,3],dofty='vlocy')
down.imposeDrivenDof(component=[1,2,3],dofty='vlocy')

# set initial horizontal velocity on the disk
diskx.imposeInitValue(component=1,value=3.0)

# interaction definition:
#   * contact law
ldkjc=pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.3)
tacts+=ldkjc
#   * see table
svdkjc = pre.see_table( CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='BLEUx',
                        CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='VERTx',
                        behav=ldkjc, alert=.1 )
                      
svs+=svdkjc

solv = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post = pre.postpro_commands()
post+= solv

# files writing
pre.writeDatbox(dim, mat, [mod], bodies, tacts, svs, post=post)

