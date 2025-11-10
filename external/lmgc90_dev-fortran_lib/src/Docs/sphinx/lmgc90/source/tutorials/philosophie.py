from pathlib import Path
import numpy, math

from pylmgc90 import pre

dim = 2

# materials, model and groups definition
mat = pre.material(name='TDURx',materialType='RIGID',density=1000.)
mut = pre.material(name='MOUxx',materialType='RIGID',density=100.)
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

radius = 0.1
disk = pre.rigidDisk(r=radius, center=[0.,0.1], model=mod, material= mat, color='BLUEx')

floor= pre.rigidJonc(axe1=1., axe2=0.05, center=[0.,-0.05], model=mod, material=mat, color='BLUEx')

## disk creation
#radius = 0.1
#disk = pre.avatar(dimension=dim)
#disk.addNode( pre.node(coor=numpy.array([0.,0.1]),number=1) )
#disk.addBulk( pre.rigid2d() )
#disk.defineGroups()
#disk.addContactors(shape='DISKx',color='BLUEx',byrd=radius)
#
## foundation creation
#floor = pre.avatar(dimension=dim)
#floor.addNode( pre.node(coor=numpy.array([0.,-0.05]),number=1) )
#floor.addBulk( pre.rigid2d() )
#floor.defineGroups()
#floor.addContactors(shape='JONCx',color='BLUEx',axe1=1.,axe2=0.05)
#
#disk.defineModel(model=mod)
#disk.defineMaterial(material=mut)
#disk.computeRigidProperties()
#floor.defineModel(model=mod)
#floor.defineMaterial(material=mat)
#floor.computeRigidProperties()

# boundary condition
floor.imposeDrivenDof(component=[1,2,3],dofty='vlocy')

# column creation
import copy
nb_disks = 10
column = pre.avatars()
for i in range(nb_disks):
  new_disk = copy.deepcopy(disk)
  new_disk.translate(dy=i*2.*radius)
  column.addAvatar(new_disk)

# copy column
bodies = pre.avatars()
nb_columns = 3
for i in range(nb_columns):
  new_columns = copy.deepcopy(column)
  new_columns.translate(dx=i*2.*radius)
  bodies += new_columns
  # or
  #bodies.extend(new_columns)

# adding floor and rotation sample
bodies.addAvatar(floor)

bodies.rotate(description='axis', alpha=-math.pi/6., axis=[0., 0., 1.], center=[1.,-0.05])

try:
  pre.visuAvatars(bodies)
except:
  pass

# containers definitions:
mats = pre.materials()
mats.addMaterial(mat,mut)
svs   = pre.see_tables()
tacts = pre.tact_behavs()

# interaction definition:
ldkjc  = pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.3)
tacts += ldkjc
svdkjc = pre.see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='BLUEx', behav=ldkjc,
                       CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='BLUEx', alert=.1)
svs   += svdkjc
svdkdk = pre.see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='BLUEx', behav=ldkjc,
                       CorpsAntagoniste='RBDY2', antagoniste='DISKx', colorAntagoniste='BLUEx', alert=.1)
svs   += svdkdk

# files writing
datbox = Path('./DATBOX')
datbox.mkdir(exist_ok=True)

pre.writeBodies(bodies,chemin=datbox)
pre.writeBulkBehav(mats,chemin=datbox,dim=dim)
pre.writeTactBehav(tacts,svs,chemin=datbox)
pre.writeDrvDof(bodies,chemin=datbox)
pre.writeDofIni(bodies,chemin=datbox)
pre.writeVlocRlocIni(chemin=datbox)

post = pre.postpro_commands()
pre.writePostpro(commands=post, parts=bodies, path=datbox)

