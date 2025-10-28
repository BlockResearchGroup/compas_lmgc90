from __future__ import print_function

import os, math

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
    os.mkdir('./DATBOX')

dim = 2

mats   = pre.materials()

plex = pre.material(name='PLExx', materialType='RIGID', density=100.)
mats.addMaterial(plex)

# on cree un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

import numpy as np

bodies = pre.avatars()

# gravity angle :
angle = math.pi/4
gravity = [9.81*math.sin(angle), -9.81*math.cos(angle), 0.]

# max number of deposited particles
nb_particles=100000

# radius range:
rmin = 0.5
rmax = 2.

# random distribution between rmin and rmax
radii=pre.granulo_Random(nb_particles, rmin, rmax)

# on recupere le plus petit et le plus grand rayon
radius_min=min(radii)
radius_max=max(radii)

# depot dans une boite rectangulaire
lx = 400.
ly = 100.

[nb_remaining_particles, coor] = pre.depositInBox2D(radii, lx, ly)
coor.shape = [coor.size/2,2]

print("Number of deposited particles: ", nb_remaining_particles)

# adding disks
for i in range(nb_remaining_particles):
    body=pre.rigidDisk(r=radii[i], center=coor[i,:], model=mod, material=plex, color='BLEUx')
    bodies += body

floor = pre.rigidJonc(lx, radius_min, [lx/2., -radius_min], mod, plex, 'WALLx')
floor.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
bodies += floor

sees   = pre.see_tables()
tacts  = pre.tact_behavs()

# friction between disks
ldkdk  = pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.3)
tacts += ldkdk

# friction between disks and foundation
ldkjc  = pre.tact_behav(name='iqsc1', law='IQS_CLB', fric=0.5)
tacts += ldkjc

# see tables:
svdkdk = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='BLEUx', behav=ldkdk,
                       CorpsAntagoniste='RBDY2', antagoniste='DISKx', colorAntagoniste='BLEUx', alert=0.1*radius_min)
svdkpl = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='BLEUx', behav=ldkjc,
                       CorpsAntagoniste='RBDY2', antagoniste='POLYG', colorAntagoniste='WALLx', alert=0.1*radius_min)
svdkjc = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='BLEUx', behav=ldkjc,
                       CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='WALLx', alert=0.1*radius_min)
sees += svdkdk
sees += svdkpl
sees += svdkjc

# ecriture des fichiers
pre.writeBodies(bodies, chemin='DATBOX')
pre.writeDrvDof(bodies, chemin='DATBOX')
pre.writeDofIni(bodies, chemin='DATBOX')

pre.writeBulkBehav(mats, chemin='DATBOX', gravy=gravity)
pre.writeTactBehav(tacts, sees, chemin='DATBOX')

pre.writeVlocRlocIni(chemin='DATBOX')

post = pre.postpro_commands()
nlgs = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(nlgs)
pre.writePostpro(commands=post, parts=bodies, path='DATBOX')


try:
    pre.visuAvatars(bodies)
except:
    pass

