
import random, math
from pathlib import Path

import numpy as np

from pylmgc90 import pre

dim = 2

# definition du conteneur de partie ou de pieces, des modeles et des materiaux
ps = pre.avatars()
ms = pre.models()
mx = pre.materials()
svs= pre.see_tables()
tts= pre.tact_behavs()

# < modeles ...
#   ... definition 

mR2D = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

mo = pre.model(name='M2D_L',physics='MECAx',element='T3xxx',dimension=dim,external_model='MatL_',
               kinematic='small',material='elas_',anisotropy='iso__',mass_storage='coher')
#   ... stockage
ms.addModel(mo)
# ... modeles >

# < materiaux ...
#   ... definition

tdur = pre.material(name='TDURx',materialType='RIGID',density=2000.)

ma1 =  pre.material(name='steel',materialType='ELAS',elas='standard',
                    young=0.1e+15,nu=0.2,anisotropy='isotropic',
                    density=0.25e+4)
#   ... stockage
mx.addMaterial(tdur,ma1)
# ... materiaux >

# definition des parties maillees
mesh_list = []
for nb in range(1,4):
  mesh_list.append( pre.readMesh('gmsh/disk'+str(nb)+'.msh',dim) )

r=[0.1,0.15,0.2]
nb_disks = 500
radii    = np.zeros( nb_disks, dtype=float )
ids      = np.zeros( nb_disks, dtype=int   )
for i in range(nb_disks):
  new_id = random.choice( range(len(r)) )
  ids[i] = new_id
  radii[i] = r[new_id]

# 15 diametres max
lx = 30.*max(r)
# 30 diametre max
ly = 60.*max(r)

nb_laid_particles, coors, radii = pre.depositInBox2D(radii,lx,ly)  
ymax  = max(coors[:,1])

for i in range(nb_laid_particles):
  d_mesh = mesh_list[ids[i]]
  disk=pre.buildMeshedAvatar(mesh=d_mesh,model=mo, material=ma1)
  disk.addContactors(group='1',shape='CLxxx',color='xxxxx',reverse=True)
  disk.addContactors(group='1',shape='ALpxx',color='xxxxx',reverse=True)
  disk.translate(dx=coors[i,0],dy=coors[i,1])
  ps.addAvatar(disk)

####
floor = pre.rigidJonc(axe1=0.6*lx, axe2=0.1, center=[0.5*lx,-0.11], 
                      model=mR2D, material=tdur, color='WALLx')
floor.imposeDrivenDof(component=[1, 2, 3],dofty='vlocy')

ps += floor

left = pre.rigidJonc(axe1=0.6*ly, axe2=0.1, center=[-0.11,0.55*ly], 
                     model=mR2D, material=tdur, color='WALLx')
left.imposeDrivenDof(component=[1, 2, 3],dofty='vlocy')
left.rotate(psi=-math.pi/2., center=left.nodes[1].coor)
ps += left

right = pre.rigidJonc(axe1=0.6*ly, axe2=0.1, center=[lx+0.11,0.55*ly], 
                      model=mR2D, material=tdur, color='WALLx')
right.imposeDrivenDof(component=[1, 2, 3],dofty='vlocy')
right.rotate(psi=math.pi/2., center=right.nodes[1].coor)
ps += right

up = pre.rigidJonc(axe1=0.6*lx, axe2=0.1, center=[0.5*lx,ymax+0.2+0.1], 
                   model=mR2D, material=tdur, color='WALLx')
up.imposeDrivenDof(component=[1, 3],dofty='vlocy')


path = Path('./DATBOX')
path.mkdir(exist_ok=True)

with open('./DATBOX/Fy.dat','w') as ofile:
    ofile.write(f'{0. :12.5e} {0.:12.5e}\n')
    ofile.write(f'{1. :12.5e} {0.:12.5e}\n')
    ofile.write(f'{2. :12.5e} {-100000.:12.5e}\n')
    ofile.write(f'{20.:12.5e} {-100000.:12.5e}\n')

up.imposeDrivenDof(component=[2], dofty='force',description='evolution', evolutionFile='Fy.dat')

ps += up

####


# Definition des interactions et des tables de visibilites
#.. table de visibilite
sv1 = pre.see_table(CorpsCandidat   ='MAILx', candidat   ='CLxxx', colorCandidat   ='xxxxx',
                    CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='WALLx',
                    behav='gapc1',alert=0.01)
svs+=sv1
sv2 = pre.see_table(CorpsCandidat   ='MAILx', candidat   ='CLxxx', colorCandidat   ='xxxxx',
                    CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='xxxxx',
                    behav='gapc1',alert=0.01,halo=0.05)

svs+=sv2

#...interaction
b = pre.tact_behav('gapc1','GAP_SGR_CLB',fric=0.9)
tts+=b

post_obj=[floor,left,right,up]

post = pre.postpro_commands()
my_command=pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(my_command)
f=pre.postpro_command(name='TORQUE EVOLUTION',step=1, rigid_set=post_obj)
post.addCommand(f)
f=pre.postpro_command(name='BODY TRACKING',step=1, rigid_set=post_obj)
post.addCommand(f)

pre.writePostpro(post, ps, path='DATBOX/')

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mx, ms, ps, tts, svs, datbox_path='DATBOX' )

try:
  pre.visuAvatars(ps)
except:
  pass

