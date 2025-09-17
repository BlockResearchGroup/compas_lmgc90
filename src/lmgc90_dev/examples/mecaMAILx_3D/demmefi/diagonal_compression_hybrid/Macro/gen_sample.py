import pathlib
import math
import numpy as np
from copy import deepcopy
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

  pierre = pre.material(name='mou__',materialType='USER_MAT',density=0., file_mat='pierre_M3.mat')
  mats.addMaterial(pierre)

  m3Dl = pre.model(name='M3DNL', physics='MECAx', element='H8xxx', dimension=dim, external_model='Demfi',
                   kinematic='small', mass_storage='lump_',user_model_name='ENDO3D',external_fields=['TEMPERATURE'])
  mods.addModel(m3Dl)

else:
  
  pierre = pre.material(name='Gxxxx', materialType='ELAS', elas='standard',
                     young=9.3e9, nu=0.2, anisotropy='isotropic', density=1200.)
  mats.addMaterial(pierre)

  m3Dl = pre.model(name='TET__', physics='MECAx', element='H8xxx', dimension=dim, external_model='no___',
                kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')
  mods.addModel(m3Dl)

  

lx = (870.)*1e-3
ly = (100.)*1e-3
lz = (840.)*1e-3

wall = pre.buildMeshH8(x0=0., y0=0., z0=0., lx=lx, ly=ly, lz=lz, nb_elem_x=42, nb_elem_y=4, nb_elem_z=43)
body = pre.buildMeshedAvatar(mesh=wall, model=m3Dl, material=pierre)

lc = lx/6

angle = (math.pi/2) - math.atan(lz/lx)

def UL(x):
  return x[0]-(lx-lc-1e-3)>1e-4 and abs(x[2]-lz)<1e-4

def UR(x):
  return abs(x[0]-lx)<1e-4 and x[2]-(lz-lc-1e-3)>1e-4

def DL(x):
  return x[0]<1e-4 and x[2]<=lc

def DR(x):
  return x[0]<=lc and x[2]<1e-4

D=[]
U=[]

body.addGroupUsingPredicate(name='DL', predicate=DL)    
body.addGroupUsingPredicate(name='DR', predicate=DR)    
body.addGroupUsingPredicate(name='UL', predicate=UL)    
body.addGroupUsingPredicate(name='UR', predicate=UR)    
if body.hasGroup('DL'): 
   body.imposeDrivenDof(group='DL',component=[1,2,3], dofty='vlocy')
   D.append((body,'DL'))
if body.hasGroup('DR'): 
   body.imposeDrivenDof(group='DR',component=[1,2,3], dofty='vlocy')
   D.append((body,'DR'))
if body.hasGroup('UL'):
   body.imposeDrivenDof(group='UL',component=[3], dofty='vlocy',description='evolution',evolutionFile='vz.dat')
   U.append((body,'UL'))        
if body.hasGroup('UR'):
   body.imposeDrivenDof(group='UR',component=[3], dofty='vlocy',description='evolution',evolutionFile='vz.dat')
   U.append((body,'UR'))        

body.rotate(description='axis', center=np.array([0.,0.,0.]), axis=[0.,1.,0.], alpha=-angle)
bodies += body


# ecriture des fichiers
datbox = pathlib.Path('./DATBOX')
datbox.mkdir(exist_ok=True)
mat_file = pathlib.Path('pierre_M3.mat')
shutil.copyfile(mat_file,datbox/mat_file)

dt=1e-3
ofile = open('./DATBOX/vz.dat','w')
ofile.write('%12.5e %12.5e\n' % (   0.   , 0.))
ofile.write('%12.5e %12.5e\n' % (   1.   , 0.))
ofile.write('%12.5e %12.5e\n' % (   1.+dt,-1e-5))
ofile.write('%12.5e %12.5e\n' % ( 202.   ,-1e-5))
ofile.write('%12.5e %12.5e\n' % ( 202.+dt,-0.))
ofile.write('%12.5e %12.5e\n' % (1000.   , 0.))
ofile.close()


pre.writeBodies(bodies,chemin='DATBOX/')
pre.writeModels(mods,chemin='DATBOX/')
pre.writeBulkBehav(mats,chemin='DATBOX/',dim=dim,gravy=[0.,0.,0.])
pre.writeTactBehav(tacts,svs,chemin='DATBOX/')
pre.writeDrvDof(bodies,chemin='DATBOX/')
pre.writeDofIni(bodies,chemin='DATBOX/')
pre.writeVlocRlocIni(chemin='DATBOX/')
pre.writeGPVIni(bodies,chemin='DATBOX/')


post=pre.postpro_commands()
post.addCommand(pre.postpro_command(name='NEW MECAx SETS',mecax_sets=[D,U]))
post.addCommand(pre.postpro_command(name='Dep EVOLUTION', step=1))
post.addCommand(pre.postpro_command(name='Fint EVOLUTION', step=1))
post.addCommand(pre.postpro_command(name='SOLVER INFORMATIONS', step=1))
post.addCommand(pre.postpro_command(name='VIOLATION EVOLUTION', step=1))
pre.writePostpro(post, bodies, path='DATBOX/')

# try:
#   pre.visuAvatars(bodies)
# except:
#   pass
