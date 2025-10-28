import os
from copy import deepcopy

from pylmgc90 import pre

dim = 2

bodies = pre.avatars()
mat    = pre.materials()
mod    = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()
post   = pre.postpro_commands()

steel = pre.material(name='Steel', materialType='ELAS', elas='standard',
                     young=1.3e8, nu=0.3, anisotropy='isotropic', density=2500.)  
mat.addMaterial(steel)

m2Dl = pre.model(name='M2DNL', physics='MECAx', element='Q4xxx', dimension=dim, external_model='MatL_',
                 kinematic='large', material='neoh_', anisotropy='iso__', mass_storage='lump_',formulation='TotaL')
mod.addModel(m2Dl)

mesh_block = pre.buildMesh2D(mesh_type='Q4', x0=0., y0=0., lx=1., ly=1., nb_elem_x=1, nb_elem_y=1)

cube1 = pre.buildMeshedAvatar(mesh=mesh_block, model=m2Dl, material=steel)
cube2 = deepcopy(cube1)
cube2.translate(dy=1.)

color2law = [ 'malp1' ,
              'malp2' ,
              'malp3' ,
              'malp4'
            ]

for i, c in enumerate(color2law):

  c1 = deepcopy(cube1)
  c2 = deepcopy(cube2)

  c1.translate(dx=i*1.1)
  c2.translate(dx=i*1.1)

  c1.addContactors(group='up'  , shape='ALpxx', color=c)
  c2.addContactors(group='down', shape='CLxxx', color=c, weights=[0.5])
  
  c1.imposeDrivenDof(group='down',component=[1, 2], dofty='vlocy')
  c2.imposeDrivenDof(group='up'  ,component=1     , dofty='vlocy')
  c2.imposeDrivenDof(group='up'  ,component=2     , dofty='vlocy')

  bodies += c1
  bodies += c2

malp1 = pre.tact_behav( name='mal1_', law='MAL_CZM', dyfr=0.1, stfr=0.1,
                        cn=1.e12, s1=3.16227e5, G1=0.1,
                        ct=1.e12, s2=3.16227e5, G2=0.1)
tacts += malp1

malp2 = pre.tact_behav( name='mal2_', law='MAL_CZM', dyfr=0.1, stfr=0.1,
                        cn=1.e12, s1=3.16227e5, G1=0.1,
                        ct=1.e12, s2=3.16227e5, G2=0.1)
tacts += malp2

malp3 = pre.tact_behav( name='mal3_', law='MAL_CZM', dyfr=0.1, stfr=0.1,
                        cn=1.e12, s1=3.16227e5, G1=0.1,
                        ct=1.e12, s2=3.16227e5, G2=0.1)
tacts += malp3

malp4 = pre.tact_behav( name='mal4_', law='MAL_CZM', dyfr=0.1, stfr=0.1,
                        cn=1.e12, s1=3.16227e5, G1=0.1,
                        ct=1.e12, s2=3.16227e5, G2=0.1) 
tacts += malp4

for c in color2law:

  st = pre.see_table(CorpsCandidat   ='MAILx', candidat   ='CLxxx', colorCandidat   =c, behav=eval(c),
                     CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste=c, alert=0.1)

  svs += st

# ecriture des fichiers
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

pre.writeDatbox(dim, mat, mod, bodies, tacts, svs, post=post, gravy=[0.,0.,0.])

try:
  pre.visuAvatars(bodies)
except:
  pass
