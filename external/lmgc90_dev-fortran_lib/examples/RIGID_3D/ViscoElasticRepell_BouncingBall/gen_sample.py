import os,sys
import numpy
import math

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# 3D example
dim = 3

# containers definitions:
#   * for avatars
bodies = pre.avatars()
#   * for materials
mats = pre.materials()
mods = pre.models()
#   * for see tables
svs = pre.see_tables()
#   * for contact laws
tacts = pre.tact_behavs()

post_ = []

#create materials
tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
pdur = pre.material(name='MOUxx',materialType='RIGID',density=100.)
mats.addMaterial(tdur,pdur)

# create a model of rigid
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod)

down = pre.rigidPlan(axe1=0.5, axe2=0.5, axe3=0.01, center=[0.4, 0., 0.], model=mod, material=tdur, color='VERTx')
down.imposeDrivenDof(component=[1,2,3,4,5,6], dofty='vlocy')
bodies += down

spx1= pre.rigidSphere(r=0.05,center=[0., 0., 0.3],model=mod,material=pdur,color='BLEUx')
bodies += spx1
post_.append(spx1)

spx2= pre.rigidSphere(r=0.05,center=[0.25, 0., 0.3],model=mod,material=pdur,color='REDxx')
bodies += spx2
post_.append(spx2)

spx3= pre.rigidSphere(r=0.05,center=[0.5, 0., 0.3],model=mod,material=pdur,color='VERTx')
bodies += spx3
post_.append(spx3)

spx4= pre.rigidSphere(r=0.05,center=[0.75, 0., 0.3],model=mod,material=pdur,color='JAUNE')
bodies += spx4
post_.append(spx4)


# interaction definition:
#   * contact law

ldk1jc=pre.tact_behav(name='iqsc1', law='VISCO_ELASTIC_REPELL_CLB', stiffness=1e5, viscosity=1., fric=0.3)
tacts+=ldk1jc
ldk2jc=pre.tact_behav(name='iqsc2', law='VISCO_ELASTIC_REPELL_CLB', stiffness=1e5, viscosity=0., fric=0.3)
tacts+=ldk2jc
ldk3jc=pre.tact_behav(name='iqsc3', law='ELASTIC_REPELL_CLB', stiffness=1e5, fric=0.3)
tacts+=ldk3jc
ldk4jc=pre.tact_behav(name='iqsc4', law='RST_CLB', rstn=1., rstt=0., fric=0.3)
tacts+=ldk4jc

#   * see table
svdk1jc = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='BLEUx',
                        CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='VERTx',
                        behav=ldk1jc, alert=.1)
svs+=svdk1jc

svdk2jc = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='REDxx',
                        CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='VERTx',
                        behav=ldk2jc, alert=.1)
svs+=svdk2jc

svdk3jc = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='VERTx',
                        CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='VERTx',
                        behav=ldk3jc, alert=.1)
svs+=svdk3jc

svdk4jc = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='JAUNE',
                        CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='VERTx',
                        behav=ldk4jc, alert=.1)
svs+=svdk4jc

post = pre.postpro_commands()

# following displacement and force on a rigid object :
disp = pre.postpro_command(name='BODY TRACKING', step=1, rigid_set=post_)
post.addCommand(disp)
torque = pre.postpro_command(name='TORQUE EVOLUTION', step=1, rigid_set=post_)
post.addCommand(torque)
#
my_command=pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(my_command)

# files writing
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass
