from pathlib import Path
import math, numpy
from pylmgc90 import pre

pre.setStopMode('exception')

datbox = Path('DATBOX')
datbox.mkdir(exist_ok=True)

avs   = pre.avatars()
mats  = pre.materials()
tacts = pre.tact_behavs()
svs   = pre.see_tables()

dim = 2

## parameters of the law
fric  = 0.3   # friction coefficient
stiff = 1.e4  # stiffness of the linear part of the law
#stiff = 1.e3  # stiffness of the linear part of the law
gap_0 = 2.5e-1 # gap value corresponding to null spring force
gcrit = 5.e-1 # critical normal reaction  at breaking
rcrit = (gcrit-gap_0) * stiff

## geometric description
radius = 0.1  # radius of the two disks

## material description
mpv  = 1000.

t_ini = 0.
t_fin = 0.3

mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mat = pre.material(name='TDURx', materialType='RIGID', density=mpv)
mats.addMaterial(mat)

# fixed disk
disk1 = pre.rigidDisk(r=radius, center=[0.,0.], model=mod, material=mat, color='DISKx')
disk1.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

disk2 = pre.rigidDisk(r=radius, center=[0.,2.*radius+gap_0], model=mod, material=mat, color='DISKx')
disk2.imposeDrivenDof(description = 'evolution', component = 2, dofty = 'force', evolutionFile = 'Fy.dat')

def imposedForce(t):
  if t < t_fin/3.:
    # pushing
    return -stiff*gap_0
  elif t < 9*t_fin/10:
    # pulling
    return rcrit + 100
  else:
    return -0.75*stiff*gap_0

from matplotlib import pyplot as pl

## to check imposed forced value
instants=numpy.linspace(t_ini,t_fin,10000)
f = []
for i in instants:
  f.append(imposedForce(i))
pl.plot(instants,f)
pl.xlabel('t')
pl.ylabel('f')
pl.grid(True)
pl.title('Force IC')
pl.show()

pre.writeEvolution(f=imposedForce, instants=numpy.linspace(t_ini,t_fin,10000), path='DATBOX/', name='Fy.dat')

avs.addAvatar(disk1)
avs.addAvatar(disk2)

iqsc0 = pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=fric)
tacts.addBehav(iqsc0)
nlaw0 = pre.tact_behav(name='nlaw0', law='BRITTLE_COATING_CLB', fric=fric, stiffness=stiff, g0=gap_0, Fmax=rcrit)
tacts.addBehav(nlaw0)

#sv = see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='DISKx', behav=iqsc0,
#               CorpsAntagoniste='RBDY2', antagoniste='DISKx', colorAntagoniste='DISKx', alert=0.01)
sv = pre.see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='DISKx', behav=nlaw0,
                   CorpsAntagoniste='RBDY2', antagoniste='DISKx', colorAntagoniste='DISKx', alert=gcrit+1.e-2)
svs.addSeeTable(sv)

#pre.visuAvatars(avs)

pre.writeBulkBehav(mats, chemin=datbox, dim=dim, gravy=[0.,0.,0.])
pre.writeBodies(avs, chemin=datbox)
pre.writeDofIni(avs, chemin=datbox)
pre.writeDrvDof(avs, chemin=datbox)
pre.writeTactBehav(tacts, svs, chemin=datbox)
pre.writeVlocRlocIni(chemin=datbox)

post = pre.postpro_commands()
disk_disp = pre.postpro_command(name='BODY TRACKING', step=1,rigid_set=[disk2])
post.addCommand(disk_disp)
disk_torque = pre.postpro_command(name='TORQUE EVOLUTION', step=1,rigid_set=[disk2])
post.addCommand(disk_torque)
pre.writePostpro(post, avs, path=datbox)

