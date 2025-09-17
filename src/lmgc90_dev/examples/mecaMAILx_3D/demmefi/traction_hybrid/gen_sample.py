
from copy import deepcopy
import pathlib
import shutil

from pylmgc90 import pre

dim = 3

bodies = pre.avatars()
mats    = pre.materials()
mods    = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

endo3d=True

if endo3d :

  pierre = pre.material(name='mou__',materialType='USER_MAT',density=2500., file_mat='pierre_mod.mat')
  mats.addMaterial(pierre)

  m3Dl = pre.model(name='M3DNL', physics='MECAx', element='H8xxx', dimension=dim, external_model='Demfi',
                   kinematic='small', mass_storage='lump_',user_model_name='ENDO3D',external_fields=['TEMPERATURE'])
  mods.addModel(m3Dl)

else:
  
  pierre = pre.material(name='Gxxxx', materialType='ELAS', elas='standard',
                     young=3.0e10, nu=0.2, anisotropy='isotropic', density=2500.)
  mats.addMaterial(pierre)

  m3Dl = pre.model(name='TET__', physics='MECAx', element='H8xxx', dimension=dim, external_model='no___',
                kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')
  mods.addModel(m3Dl)
  
mesh_block = pre.buildMeshH8(x0=0., y0=0., z0=0., lx=1., ly=1., lz=1., nb_elem_x=1, nb_elem_y=1, nb_elem_z=1)

cube1 = pre.buildMeshedAvatar(mesh=mesh_block, model=m3Dl, material=pierre)
cube2 = deepcopy(cube1)
cube2.translate(dz=1.)

color2law = ['expoc' ,
            ]

MS=[]

for i, c in enumerate(color2law):

  c1 = deepcopy(cube1)
  c2 = deepcopy(cube2)

  c1.translate(dy=i*1.1)
  c2.translate(dy=i*1.1)

  c1.addContactors(group='up'  , shape='ASpxx', color=c)
  c2.addContactors(group='down', shape='CSpxx', color=c, quadrature=0)
  
  c1.imposeDrivenDof(group='down',component=[1, 2, 3], dofty='vlocy')
  c2.imposeDrivenDof(group='up'  ,component=[1, 2]   , dofty='vlocy')
  c2.imposeDrivenDof(group='up'  ,component= 3       , dofty='vlocy', ct=4.5e-2)

  bodies += c1
  bodies += c2

  MS.append([(c1,'up')])
  MS.append([(c2,'down')])  

expoc = pre.tact_behav( name='expo_', law='EXPO_CZM', dyfr=0.1, stfr=0.1,
                        cn=1.e12, s1=3.16227e5, G1=0.1,
                        ct=1.e12, s2=3.16227e5, G2=0.1, eta=1e-4 )
tacts += expoc

for c in color2law:

  st = pre.see_table(CorpsCandidat   ='MAILx', candidat   ='CSxxx', colorCandidat   =c, behav=eval(c),
                     CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste=c, alert=0.1)

  svs += st

# ecriture des fichiers
datbox = pathlib.Path('./DATBOX')
datbox.mkdir(exist_ok=True)
mat_file = pathlib.Path('pierre_mod.mat')
shutil.copyfile(mat_file,datbox/mat_file)
  
pre.writeBodies(bodies,chemin='DATBOX/')
pre.writeModels(mods,chemin='DATBOX/')
pre.writeBulkBehav(mats,chemin='DATBOX/',dim=dim,gravy=[0.,0.,0.])
pre.writeTactBehav(tacts,svs,chemin='DATBOX/')
pre.writeDrvDof(bodies,chemin='DATBOX/')
pre.writeDofIni(bodies,chemin='DATBOX/')
pre.writeVlocRlocIni(chemin='DATBOX/')
pre.writeGPVIni(bodies,chemin='DATBOX/')





post=pre.postpro_commands()
post.addCommand(pre.postpro_command(name='NEW MECAx SETS',mecax_sets=MS))
post.addCommand(pre.postpro_command(name='Dep EVOLUTION', step=1))
post.addCommand(pre.postpro_command(name='Fint EVOLUTION', step=1))
post.addCommand(pre.postpro_command(name='SOLVER INFORMATIONS', step=1))
post.addCommand(pre.postpro_command(name='VIOLATION EVOLUTION', step=1))
pre.writePostpro(post, bodies, path='DATBOX/')

try:
  pre.visuAvatars(bodies)
except:
  pass
