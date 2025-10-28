import sys
from pathlib import Path

from pylmgc90 import pre

datbox_path = Path('DATBOX')
datbox_path.mkdir(exist_ok=True)

if '--norand' in sys.argv:
  seed = 1
else:
  seed = None

bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

dim = 2

mod  = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
m2Dl = pre.model(name='M2DQ4', physics='MECAx', element='Q4xxx', dimension=dim, 
                 external_model='MatL_', kinematic='small', material='elas_', 
                 anisotropy='iso__', mass_storage='coher')
mods.addModel(m2Dl)

plexx = pre.material(name='plexx', materialType='RIGID', density=145.)
tdurx = pre.material(name='tdurx', materialType='RIGID', density=2500.)
steel = pre.material(name='steel', materialType='ELAS', density=2500., elas='standard',
                     anisotropy='isotropic', young=1.e14, nu=0.3)  

mats.addMaterial(plexx,tdurx,steel)

#particles
nb_particles = 50
radii = pre.granulo_Random(nb_particles, 0.2, 0.3, seed)

radius_min = min(radii)
radius_max = max(radii)

# size of the walls of the box
lx = 3.0
ex = 0.2 # sometime radius_min/max
ly = 2.0
ey = 0.2 # sometime radius_min/max

# for deformable walls, number of elements on each directions
nb_lx = 4
nb_ex = 1
nb_ly = 4
nb_ey = 1

nb_deposit, coor, radii = pre.depositInBox2D(radii, lx, ly)

for r, c in zip(radii, coor):
  body = pre.rigidDisk(r=r, center=c, model=mod, material=plexx, color='BLEUx')
  bodies += body

#defor box
down_mesh = pre.buildMesh2D('Q4',0.,0.,lx+2*ey,ex,nb_lx,nb_ex)
down_wall = pre.buildMeshedAvatar(mesh=down_mesh, model=m2Dl, material=steel)
down_wall.translate(dx=-ey,dy=-ex)
down_wall.addContactors(group='up', shape='ALpxx', color='BLEUx')
down_wall.imposeDrivenDof(group='down', component=[1,2], dofty='vlocy')
bodies += down_wall
#
wall_mesh = pre.buildMesh2D('Q4',0.,0.,ey,ly,nb_ey,nb_ly)
#
left_wall = pre.buildMeshedAvatar(mesh=wall_mesh, model=m2Dl, material=steel)
left_wall.translate(dx=-ey)
left_wall.addContactors(group='right', shape='ALpxx', color='BLEUx')
left_wall.addContactors(group='down', shape='CLxxx', color='BLEUx', weights=[0.25, 0.75])
bodies += left_wall
#
wall_mesh = pre.buildMesh2D('Q4',0.,0.,ey,ly,nb_ey,nb_ly)
#
right_wall = pre.buildMeshedAvatar(mesh=wall_mesh, model=m2Dl, material=steel)
right_wall.translate(dx=lx)
right_wall.addContactors(group='left', shape='ALpxx', color='BLEUx')
right_wall.addContactors(group='down', shape='CLxxx', color='BLEUx',weights=[0.25, 0.75])
bodies += right_wall

# interactions :
dkdkx  = pre.tact_behav('iqsc0','IQS_CLB',fric=0.9)
dkalp  = pre.tact_behav('gapc0','GAP_SGR_CLB',fric=0.9)
clalp  = pre.tact_behav('mczm0','MAC_CZM',dyfr=0.1, stfr=0.1, cn=1.e13, ct=1.e13, b=1.e-4, w=2.5e-3)

tacts += dkdkx
tacts += dkalp
tacts += clalp

svdkdk = pre.see_table(CorpsCandidat='RBDY2',candidat='DISKx',colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY2',antagoniste='DISKx',colorAntagoniste='BLEUx',
                       behav=dkdkx, alert=0.02)
svdkal = pre.see_table(CorpsCandidat='RBDY2',candidat='DISKx',colorCandidat='BLEUx',
                       CorpsAntagoniste='MAILx',antagoniste='ALpxx',colorAntagoniste='BLEUx',
                       behav=dkalp, alert=0.1)
svclal = pre.see_table(CorpsCandidat='MAILx',candidat='CLxxx',colorCandidat='BLEUx',
                       CorpsAntagoniste='MAILx',antagoniste='ALpxx',colorAntagoniste='BLEUx',
                       behav=clalp, alert=0.01, halo=3.)

svs   += svdkdk
svs   += svdkal
svs   += svclal

post = pre.postpro_commands()
#my_command=postpro_command(name='NEW RIGID SETS', step=1, rigid_sets=[[spher], [left, down, right]])
my_command = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(my_command)
# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass
