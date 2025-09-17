import os, math
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

from copy import deepcopy

# Import python fonction to create LMGC90 model
from pylmgc90 import pre

# ------------------------------------------------------
# To start : Create all pack  of components
# Create the pack of bodies
bodies    = pre.avatars()
# Create the pack of physical model
mods      = pre.models()
# Create the pack of materials behaviours
mats      = pre.materials()
# Create the pack of contact visibility
svs       = pre.see_tables()
# Create the pack of contact behaviours
tacts     = pre.tact_behavs()
# ------------------------------------------------------
# Defined the model dimension of the physical problem
dimension = 3

# ------------------------------------------------------
# Create models

Rigid = pre.model(name='Rigid', physics='MECAx', element='Rxx3D', dimension=dimension)

# And save this model in the pack of physical model
mods.addModel(Rigid)

# ------------------------------------------------------
# Create materials

Steel  = pre.material(name='Rigid', materialType='RIGID', density=7800.)

# And save this material in the pack of materials behaviours
mats.addMaterial(Steel)

# ------------------------------------------------------
# Create bodies

mesh = pre.readMesh('lego.msh', dimension)
# Import element connectivity
lego = pre.volumicMeshToRigid3D(volumic_mesh=mesh, model=Rigid, material=Steel, color='SHAPE')
bodies += lego

lego2 = deepcopy(lego)
lego2.translate(dy=0.025,dz=0.16)
lego2.rotate(description='axis',axis=[0.,1.,0.],center=lego2.nodes[1].coor,alpha=math.pi*(0.95))
lego2.rotate(description='axis',axis=[0.,0.,1.],center=lego2.nodes[1].coor,alpha=math.pi/2)
bodies += lego2

# ------------------------------------------------------
# Apply the boundary conditions on physical group
lego.imposeDrivenDof(component=[1,2,3,4,5,6], dofty='vlocy', ct=0.0)

# ------------------------------------------------------
# Contact law and see table definitions
lprpr = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts+=lprpr
svprpr = pre.see_table(CorpsCandidat='RBDY3'   , candidat='POLYR'   , colorCandidat='SHAPE',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='SHAPE',
                       behav=lprpr,alert=0.1)
svs+=svprpr

# ------------------------------------------------------
# Write all files to compute solution with LMGC90
pre.writeDatbox(dimension, mats, mods, bodies, tacts, svs)

try:
  pre.visuAvatars(bodies)
except:
  pass
