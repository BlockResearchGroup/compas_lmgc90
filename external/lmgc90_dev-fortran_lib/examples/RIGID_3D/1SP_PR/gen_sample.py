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
mats = pre.materials()
mods = pre.models()
svs = pre.see_tables()
tacts = pre.tact_behavs()

#create materials
tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
pdur = pre.material(name='MOUxx',materialType='RIGID',density=100.)
mats.addMaterial(tdur,pdur)


# create a model of rigid
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod)

#create some bodies
x = 0.
y = 0.
z = 0.1
down1 = pre.rigidPolyhedron(center=[x, y, 0.], material=tdur, model=mod, color='VERTx',generation_type='regular',nb_vertices=8, radius=0.2, xr=1., yr=1., zr=0.2)
down1.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += down1

spher1= pre.rigidSphere(r=0.05, center=[x, y, z], material=pdur, model=mod)
spher1.imposeInitValue(component=[3], value=[-1.0])
bodies += spher1

x += 0.5 
down2 = pre.rigidPolyhedron(center=[x, y, 0.], material=tdur, model=mod, color='VERTx',generation_type='regular',nb_vertices=8, radius=0.2, xr=1., yr=1., zr=0.2)
down2.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += down2

spher2= pre.rigidSphere(r=0.05, center=[x - 0.12, y, z], material=pdur, model=mod)
spher2.imposeInitValue(component=[3], value=[-1.0])
bodies += spher2


x += 0.5
down3 = pre.rigidPolyhedron(center=[x, y, 0.], material=tdur, model=mod, color='VERTx',generation_type='regular',nb_vertices=8, radius=0.2, xr=1., yr=1., zr=0.2)
down3.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += down3

spher3= pre.rigidSphere(r=0.05, center=[x + 0.12, y, z], material=pdur, model=mod)
spher3.imposeInitValue(component=[3], value=[-1.0])
bodies += spher3

x += 0.5
down4 = pre.rigidPolyhedron(center=[x, y, 0.], material=tdur, model=mod, color='VERTx',generation_type='regular',nb_vertices=8, radius=0.2, xr=1., yr=1., zr=0.2)
down4.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += down4

spher4= pre.rigidSphere(r=0.05, center=[x, y - 0.12, z], material=pdur, model=mod)
spher4.imposeInitValue(component=[3], value=[-1.0])
bodies += spher4

x += 0.5
down5 = pre.rigidPolyhedron(center=[x, y, 0.], material=tdur, model=mod, color='VERTx',generation_type='regular',nb_vertices=8, radius=0.2, xr=1., yr=1., zr=0.2)
down5.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += down5

spher5= pre.rigidSphere(r=0.05, center=[x, y + 0.12, z], material=pdur, model=mod)
spher5.imposeInitValue(component=[3], value=[-1.0])
bodies += spher5


x = 0.
y = 0.5
z = -0.1
down11 = pre.rigidPolyhedron(center=[x, y, 0.], material=tdur, model=mod, color='VERTx',generation_type='regular',nb_vertices=8, radius=0.2, xr=1., yr=1., zr=0.2)
down11.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += down11

spher11= pre.rigidSphere(r=0.05, center=[x, y, z], material=pdur, model=mod)
spher11.imposeInitValue(component=[3], value=[1.0])
bodies += spher11

x += 0.5 
down12 = pre.rigidPolyhedron(center=[x, y, 0.], material=tdur, model=mod, color='VERTx',generation_type='regular',nb_vertices=8, radius=0.2, xr=1., yr=1., zr=0.2)
down12.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += down12

spher12= pre.rigidSphere(r=0.05, center=[x - 0.12, y, z], material=pdur, model=mod)
spher12.imposeInitValue(component=[3], value=[1.0])
bodies += spher12


x += 0.5
down13 = pre.rigidPolyhedron(center=[x, y, 0.], material=tdur, model=mod, color='VERTx',generation_type='regular',nb_vertices=8, radius=0.2, xr=1., yr=1., zr=0.2)
down13.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += down13

spher13= pre.rigidSphere(r=0.05, center=[x + 0.12, y, z], material=pdur, model=mod)
spher13.imposeInitValue(component=[3], value=[1.0])
bodies += spher13

x += 0.5
down14 = pre.rigidPolyhedron(center=[x, y, 0.], material=tdur, model=mod, color='VERTx',generation_type='regular',nb_vertices=8, radius=0.2, xr=1., yr=1., zr=0.2)
down14.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += down14

spher14= pre.rigidSphere(r=0.05, center=[x, y - 0.12, z], material=pdur, model=mod)
spher14.imposeInitValue(component=[3], value=[1.0])
bodies += spher14

x += 0.5
down15 = pre.rigidPolyhedron(center=[x, y, 0.], material=tdur, model=mod, color='VERTx',generation_type='regular',nb_vertices=8, radius=0.2, xr=1., yr=1., zr=0.2)
down15.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += down15

spher15= pre.rigidSphere(r=0.05, center=[x, y + 0.12, z], material=pdur, model=mod)
spher15.imposeInitValue(component=[3], value=[1.0])
bodies += spher15

x = 0.
y = 1.
z = 0.

x += 0.5 
down22 = pre.rigidPolyhedron(center=[x, y, 0.], material=tdur, model=mod, color='VERTx',generation_type='regular',nb_vertices=8, radius=0.2, xr=1., yr=1., zr=0.2)
down22.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += down22

spher22= pre.rigidSphere(r=0.05, center=[x - 0.18, y -0.18, z], material=pdur, model=mod)
spher22.imposeInitValue(component=[1, 2], value=[1.0, 1.0])
bodies += spher22


x += 0.5
down23 = pre.rigidPolyhedron(center=[x, y, 0.], material=tdur, model=mod, color='VERTx',generation_type='regular',nb_vertices=8, radius=0.2, xr=1., yr=1., zr=0.2)
down23.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += down23

spher23= pre.rigidSphere(r=0.05, center=[x + 0.18, y - 0.18, z], material=pdur, model=mod)
spher23.imposeInitValue(component=[1,2], value=[-1.0, 1.0])
bodies += spher23

x += 0.5
down24 = pre.rigidPolyhedron(center=[x, y, 0.], material=tdur, model=mod, color='VERTx',generation_type='regular',nb_vertices=8, radius=0.2, xr=1., yr=1., zr=0.2)
down24.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += down24

spher24= pre.rigidSphere(r=0.05, center=[x + 0.18, y + 0.18, z], material=pdur, model=mod)
spher24.imposeInitValue(component=[1,2], value=[-1.0, -1.0])
bodies += spher24

x += 0.5
down25 = pre.rigidPolyhedron(center=[x, y, 0.], material=tdur, model=mod, color='VERTx',generation_type='regular',nb_vertices=8, radius=0.2, xr=1., yr=1., zr=0.2)
down25.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += down25

spher25= pre.rigidSphere(r=0.05, center=[x - 0.18, y + 0.18, z], material=pdur, model=mod)
spher25.imposeInitValue(component=[1,2], value=[1.0, -1.0])
bodies += spher25


b=pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts+=b

#interactions
sv = pre.see_table(CorpsCandidat='RBDY3',candidat='SPHER',colorCandidat='BLUEx',
                   CorpsAntagoniste='RBDY3',antagoniste='POLYR',colorAntagoniste='VERTx',
                   behav=b,alert=0.05,halo=0.2)
svs+=sv

post=pre.postpro_commands()
my_command=pre.postpro_command(name='NEW RIGID SETS', step=1, rigid_sets=[[spher1], [down1]])
post.addCommand(my_command)

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post, gravy=[0., 0., 0.])

try:
  pre.visuAvatars(bodies)
except:
  pass
