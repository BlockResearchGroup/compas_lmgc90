from __future__ import print_function
import os,sys

import numpy as np

from pylmgc90.pre import *

##### TP4 : Modify the script to add the moving hollow cylinders and walls for compression #####
#
# Look for comment starting with TODO for instructions !

#define the spheres granulometry
r_min = 0.3
r_max = 0.6
r_mean = 0.5*(r_min+r_max)

#define cylinder container
ratio = 5
R_cyl =  10. * r_mean
H_cyl = ratio * R_cyl

#rough over-estimation of the number of particles
#in the cylinder
nb_layers = int(H_cyl / r_mean)
nb_particles = 1.2 * nb_layers * R_cyl**3 / r_mean**3
print('nb_particles : ', nb_particles)

#classical deposit of spheres in a cylinder

dim = 3

gravity = np.zeros(3)

bodies = avatars()
mat    = materials()
svs    = see_tables()
tacts  = tact_behavs()

steel = material(name='PLEXx', materialType='RIGID', density=7800.)
mat.addMaterial(steel)
tdur = material(name='TDURx', materialType='RIGID', density=1000.)
mat.addMaterial(tdur)

mod = model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)


radii=np.random.uniform(r_min, r_max, nb_particles)

nb_comp_particles, coor=depositInCylinder3D(radii, R_cyl, H_cyl) 
if nb_comp_particles == nb_particles:
  print('WARNING : all particles deposited')
print('nb_comp_particles : ', nb_comp_particles, nb_layers)

for i in range(nb_comp_particles):
   body = rigidSphere(r=radii[i], center=coor[3*i : 3*(i + 1)], model=mod, material=steel, color='BILLE')
   bodies += body


#defining upper and lower walls
down = rigidPlan(axe1=R_cyl, axe2=R_cyl, axe3=r_min, center=[0., 0., -r_min], model=mod, material=tdur, color='WALLx')
up   = rigidPlan(axe1=R_cyl, axe2=R_cyl, axe3=r_min, center=[0., 0., H_cyl+r_min], model=mod, material=tdur, color='WALLx')

down.imposeDrivenDof(component=list(range(1,7)), dofty='vlocy')
up.imposeDrivenDof(component=list(range(1,7)), dofty='vlocy')

up.rotate(theta=np.pi, center=up.nodes[1].coor)

bodies.addAvatar(down)
bodies.addAvatar(up)

#TODO 1:
#
# Create another couple of 'down' and 'up' walls
# and hollow cylinders  with a constant velocity
# for compression.
# Choose another color than 'WALL'.
#
# needed functions:
# - rigidPlan
# - rididCylinder
# - imposeDrivenDof & rotate

try:
  visuAvatars(bodies)
except:
  pass

lspsp = tact_behav(name='iqsc0', law='IQS_CLB', fric=0.3)
tacts += lspsp
lspwl = tact_behav(name='iqsc1', law='IQS_CLB', fric=0.)
tacts += lspwl
svspsp = see_table(CorpsCandidat   ='RBDY3', candidat   ='SPHER', colorCandidat   ='BILLE',behav=lspsp, 
                   CorpsAntagoniste='RBDY3', antagoniste='SPHER', colorAntagoniste='BILLE',alert=0.1*r_min)
svs+=svspsp
svsppl = see_table(CorpsCandidat   ='RBDY3', candidat   ='SPHER', colorCandidat   ='BILLE',behav=lspwl, 
                   CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='WALLx',alert=0.1*r_min)
svs+=svsppl

#TODO 2:
#
# Add visibility tables for the moving
# walls and cylinder.
#
# needed functions:
# see_table

post = postpro_commands()
solv = postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(solv)

datbox_path = 'DATBOX'
if not os.path.isdir(datbox_path):
  os.mkdir(datbox_path)

# ecriture des fichiers
writeBodies(bodies, chemin='DATBOX/')
writeBulkBehav(mat, chemin='DATBOX/', dim=3, gravy=gravity)
writeTactBehav(tacts, svs, chemin='DATBOX/')
writeDrvDof(bodies, chemin='DATBOX/')
writeDofIni(bodies, chemin='DATBOX/')
writeVlocRlocIni(chemin='DATBOX/')
writePostpro(commands=post, parts=bodies, path=datbox_path)
