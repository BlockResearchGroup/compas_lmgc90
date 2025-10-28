import os
import math
from copy import deepcopy

from pylmgc90 import pre

if( not os.path.isdir('DATBOX') ):
   os.mkdir('DATBOX')

bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
tacts  = pre.tact_behavs()
sees   = pre.see_tables()
post   = pre.postpro_commands()

dim = 3

mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod)

tdur = pre.material(name='TDURx', materialType='RIGID', density=1000.)
plex = pre.material(name='PLExx', materialType='RIGID', density=100. )
mats.addMaterial(plex, tdur)

byrd = 1.
high = 2.

cylnd = pre.rigidCylinder(r=byrd, h=high, center=[byrd,byrd,high+byrd], model=mod, material=plex, color='BLUEx')
alpha  = math.pi/2.
frame  = [[1., 0., 0.,], [0., math.cos(alpha), -math.sin(alpha)], [0., math.sin(alpha), math.cos(alpha)]]
# beware addContacors need half heigth of the cylinder
cylnd.addContactors(shape='CYLND', byrd=byrd, High=high, frame=frame, color='BLUEx')
frame  = [[math.cos(alpha), 0., -math.sin(alpha)], [0., 1., 0.], [math.sin(alpha), 0., math.cos(alpha)]]
cylnd.addContactors(shape='CYLND', byrd=byrd, High=0.5*high, shift=[byrd,2.*byrd,0.], frame=frame, color='BLUEx')
cylnd.computeRigidProperties()
bodies.addAvatar(cylnd)

cylnd2 = deepcopy(cylnd)
cylnd2.translate(dx=2*byrd, dy=2*byrd, dz=2*high)
bodies.addAvatar(cylnd2)

axe = 8.
planx = pre.rigidPlan(axe1=axe, axe2=axe, axe3=1., center=[1., 1., -1.], model=mod, material=tdur, color='GREEN')
planx.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies.addAvatar(planx)

iqsc1 = pre.tact_behav(name='iqsc1', law='IQS_CLB', fric=0.5)
tacts.addBehav(iqsc1)

sv1 = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='CYLND', colorCandidat   ='BLUEx',
                    CorpsAntagoniste='RBDY3', antagoniste='CYLND', colorAntagoniste='BLUEx',
                    behav=iqsc1, alert=0.1)
sv2 = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='CYLND', colorCandidat   ='BLUEx',
                    CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='GREEN',
                    behav=iqsc1, alert=0.1)

sees.addSeeTable(sv1)
sees.addSeeTable(sv2)


pre.writeDatbox(dim, mats, mods, bodies, tacts, sees, post=post, datbox_path='DATBOX')
pre.visuAvatars(bodies,True)

