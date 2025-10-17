
from copy import deepcopy
import pathlib
import shutil

from pylmgc90 import pre

dim = 3

bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
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

l=0.1
  
mesh_block = pre.buildMeshH8(x0=0., y0=0., z0=0., lx=l, ly=l, lz=l, nb_elem_x=1, nb_elem_y=1, nb_elem_z=1)
mesh_block_2 = pre.buildMeshH8(x0=0., y0=0., z0=0., lx=2*l, ly=l, lz=l, nb_elem_x=9, nb_elem_y=4, nb_elem_z=4)

cube1 = pre.buildMeshedAvatar(mesh=mesh_block, model=m3Dl, material=pierre)

cube2 = deepcopy(cube1)
cube2.translate(dx=l)

cube3 = pre.buildMeshedAvatar(mesh=mesh_block_2, model=m3Dl, material=pierre)
cube3.translate(dz=-l)

cube4 = deepcopy(cube1)
cube4.translate(dz=-2*l)

cube5 = deepcopy(cube1)
cube5.translate(dx=l,dz=-2*l)

color2law = ['expoc' ,
            ]

MS=[]

vz=0.01


for i, c in enumerate(color2law):

  c1 = deepcopy(cube1)
  c2 = deepcopy(cube2)
  c3 = deepcopy(cube3)
  c4 = deepcopy(cube4)
  c5 = deepcopy(cube5)
  
  c1.translate(dy=i*3*l)
  c2.translate(dy=i*3*l)
  c3.translate(dy=i*3*l)
  c4.translate(dy=i*3*l)
  c5.translate(dy=i*3*l)

  c1.addContactors(group='front', shape='ASpxx', color=c)
  c1.addContactors(group='down', shape='ASpxx', color=c)

  c2.addContactors(group='rear', shape='CSpxx', color=c, quadrature=1)
  c2.addContactors(group='down', shape='ASpxx', color=c)

  c3.addContactors(group='up'  , shape='CSpxx', color=c, quadrature=1)
  c3.addContactors(group='down', shape='CSpxx', color=c, quadrature=1)

  c4.addContactors(group='up', shape='ASpxx', color=c)
  c4.addContactors(group='front', shape='ASpxx', color=c)

  c5.addContactors(group='up', shape='ASpxx', color=c)
  c5.addContactors(group='rear', shape='CSpxx', color=c, quadrature=1)


  c1.imposeDrivenDof(group='up'  , component=[1,2]   ,dofty='vlocy')
  c1.imposeDrivenDof(group='up'  , component= 3      ,dofty='vlocy', ct=vz)
  c1.imposeDrivenDof(group='rear', component=[1,2]   ,dofty='vlocy')
  c1.imposeDrivenDof(group='rear', component= 3      ,dofty='vlocy', ct=vz)

  MS.append((c1,'up'))
  MS.append((c1,'rear'))  

  c2.imposeDrivenDof(group='up'   , component=[1,2,3],dofty='vlocy')
  c2.imposeDrivenDof(group='front', component=[1,2,3],dofty='vlocy')
  
  c3.imposeDrivenDof(group='rear' , component=[1,2]  ,dofty='vlocy')
  c3.imposeDrivenDof(group='rear' , component= 3     ,dofty='vlocy', ct=vz)
  c3.imposeDrivenDof(group='front', component=[1,2,3],dofty='vlocy')

  MS.append((c3,'rear'))
  
  c4.imposeDrivenDof(group='rear' , component=[1,2]  ,dofty='vlocy')
  c4.imposeDrivenDof(group='rear' , component= 3     ,dofty='vlocy', ct=vz)
  c4.imposeDrivenDof(group='down' , component=[1,2]  ,dofty='vlocy')
  c4.imposeDrivenDof(group='down' , component= 3     ,dofty='vlocy', ct=vz)

  MS.append((c4,'rear'))
  MS.append((c4,'down')) 
  
  c5.imposeDrivenDof(group='front',component=[1,2,3] ,dofty='vlocy')
  c5.imposeDrivenDof(group='down' ,component=[1,2,3] ,dofty='vlocy')  
  c5.imposeDrivenDof(group='front',component=[1]     ,dofty='vlocy')

  bodies += c1
  bodies += c2
  bodies += c3
  bodies += c4
  bodies += c5


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
post.addCommand(pre.postpro_command(name='NEW MECAx SETS',mecax_sets=[MS]))
post.addCommand(pre.postpro_command(name='Dep EVOLUTION', step=1))
post.addCommand(pre.postpro_command(name='Fint EVOLUTION', step=1))
post.addCommand(pre.postpro_command(name='SOLVER INFORMATIONS', step=1))
post.addCommand(pre.postpro_command(name='VIOLATION EVOLUTION', step=1))
pre.writePostpro(post, bodies, path='DATBOX/')

try:
  pre.visuAvatars(bodies)
except:
  pass
