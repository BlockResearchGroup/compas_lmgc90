from __future__ import print_function
import os,sys

import numpy
import math

from pylmgc90 import pre

# 3D
dim = 3

bodies = pre.avatars()
mat    = pre.materials()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

tdur = pre.material(name='TDURx', materialType='RIGID', density=2500.)
plex = pre.material(name='PLEXx', materialType='RIGID', density=2000.)
mat.addMaterial(tdur,plex)

mod3D = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)

# brick definition
brique_Paris = pre.brick3D(name='brique de Paris', lx=0.22, ly=0.11, lz=0.06)

# paneress wall using previous brick type
wall = pre.paneresse_simple(brick_ref=brique_Paris, disposition="paneresse")

# on caracterise le mur, en longueur

# first layer definition 
#wall.setFirstRowByNumberOfBricks(first_brick_type="1/2", nb_bricks=10., joint_thickness=0.01)
wall.setFirstRowByLength(first_brick_type="1/2", length=2.3, joint_thickness=0.01)

# number of layers
wall.setNumberOfRows(10.)
# joint thickness between layers
wall.setJointThicknessBetweenRows(0.01)
#wall.setHeight(0.7)

#wall.computeNbRows(trend="max")
# wall ehight computation
wall.computeHeight()

print("wall.nb_rows=", wall.nb_rows)
print("wall.height=", wall.height)
print("wall.joint_thickness=", wall.joint_thickness)

# wall building
bodies = wall.buildRigidWall(origin=[0., 0., 0.], model=mod3D, material=plex, colors=['BLUEx', 'REDxx'])

# rigid fondation
floor = pre.rigidPlan(axe1=1.25, axe2=0.075, axe3=0.03, center=[1.15, 0.055, -0.03],
                      model = mod3D, material = tdur, color='WALLx')
floor.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies += floor

new_body = brique_Paris.rigidBrick([0.11,0.056,wall.height+4.e-2],mod3D,plex,'GREEN')
bodies += new_body

# interation managmeent
lprpr  = pre.tact_behav(name='iqsg0', law='IQS_CLB_g0', fric=0.3)
tacts += lprpr
lprpl  = pre.tact_behav(name='iqsg1', law='IQS_CLB_g0', fric=0.5)
tacts += lprpl
lprps  = pre.tact_behav(name='iqsg2', law='IQS_CLB_g0', fric=0.)
tacts += lprps

svbbbb = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLUEx', behav='iqsg0', 
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLUEx', alert=0.02)
svs+=svbbbb
svbrbr = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='REDxx', behav='iqsg0', 
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx', alert=0.02)
svs+=svbrbr
svbbbr = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLUEx', behav='iqsg0', 
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx', alert=0.02)
svs+=svbbbr
svprpl = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLUEx', behav='iqsg1', 
                       CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='WALLx', alert=0.02)
svs+=svprpl
svprps = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='GREEN', behav='iqsg2', 
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx', alert=0.02)
svs+=svprps

import os
if not os.path.isdir('DATBOX'):
  os.mkdir('DATBOX')

# file writing
pre.writeBodies(bodies, chemin='DATBOX/')
pre.writeBodies(bodies,chemin='DATBOX/')
pre.writeDofIni(bodies,chemin='DATBOX/')
pre.writeBulkBehav(mat, chemin='DATBOX/', dim=3)
pre.writeTactBehav(tacts, svs, chemin='DATBOX/')
pre.writeDrvDof(bodies, chemin='DATBOX/')
pre.writeDofIni(bodies, chemin='DATBOX/')
pre.writeVlocRlocIni(chemin='DATBOX/')

try:
  pre.visuAvatars(bodies)
except:
  pass

