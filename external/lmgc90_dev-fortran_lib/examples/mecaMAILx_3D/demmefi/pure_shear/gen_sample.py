import numpy as np
from copy import deepcopy
import pathlib
import shutil

from pylmgc90 import pre

# ecriture des fichiers
datbox = pathlib.Path('./DATBOX')
datbox.mkdir(exist_ok=True)
mat_file = pathlib.Path('pierre_mod.mat')
shutil.copyfile(mat_file,datbox/mat_file)

dim = 3

bodies = pre.avatars()
mat    = pre.materials()
mod    = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()
post   = pre.postpro_commands()

pierre = pre.material(name='mou__',materialType='USER_MAT',density=0., file_mat='pierre_mod.mat')
mat.addMaterial(pierre)

m3Dl = pre.model(name='M3DNL', physics='MECAx', element='H8xxx', dimension=dim, external_model='Demfi',
                 kinematic='small', mass_storage='lump_',user_model_name='ENDO3D',external_fields=['TEMPERATURE'])
mod.addModel(m3Dl)

mesh_block = pre.buildMeshH8(x0=0., y0=0., z0=0., lx=0.1, ly=0.1, lz=0.1, nb_elem_x=1, nb_elem_y=1, nb_elem_z=1)

MS=[]

cube1 = pre.buildMeshedAvatar(mesh=mesh_block, model=m3Dl, material=pierre)

##
def P1(x):
  return np.linalg.norm(x-np.array([0.,0.,0.])) < 1e-4

cube1.addGroupUsingPredicate(name='CL1', predicate=P1)

def P2(x):
  return np.linalg.norm(x-np.array([0.1,0.,0.])) < 1e-4

cube1.addGroupUsingPredicate(name='CL2', predicate=P2)

def P3(x):
  return np.linalg.norm(x-np.array([0.1,0.1,0.])) < 1e-4

cube1.addGroupUsingPredicate(name='CL3', predicate=P3)

def P4(x):
  return np.linalg.norm(x-np.array([0.,0.1,0.])) < 1e-4

cube1.addGroupUsingPredicate(name='CL4', predicate=P4)

def P5(x):
  return np.linalg.norm(x-np.array([0.,0.,0.1])) < 1e-4

cube1.addGroupUsingPredicate(name='CL5', predicate=P5)

def P6(x):
  return np.linalg.norm(x-np.array([0.1,0.,0.1])) < 1e-4

cube1.addGroupUsingPredicate(name='CL6', predicate=P6)

def P7(x):
  return np.linalg.norm(x-np.array([0.1,0.1,0.1])) < 1e-4

cube1.addGroupUsingPredicate(name='CL7', predicate=P7)

def P8(x):
  return np.linalg.norm(x-np.array([0.,0.1,0.1])) < 1e-4

cube1.addGroupUsingPredicate(name='CL8', predicate=P8)

#
cube1.imposeDrivenDof(group='down',component=[3], dofty='vlocy')

cube1.imposeDrivenDof(group='CL1',component=[1], dofty='vlocy')
cube1.imposeDrivenDof(group='CL1',component=[2], dofty='vlocy')

cube1.imposeDrivenDof(group='CL2',component=[1], dofty='vlocy')
cube1.imposeDrivenDof(group='CL2',component=[2], dofty='vlocy',description='evolution',evolutionFile='v.dat')

cube1.imposeDrivenDof(group='CL3',component=[1], dofty='vlocy',description='evolution',evolutionFile='v.dat')
cube1.imposeDrivenDof(group='CL3',component=[2], dofty='vlocy',description='evolution',evolutionFile='v.dat')

cube1.imposeDrivenDof(group='CL4',component=[1], dofty='vlocy',description='evolution',evolutionFile='v.dat')
cube1.imposeDrivenDof(group='CL4',component=[2], dofty='vlocy')

cube1.imposeDrivenDof(group='CL5',component=[1], dofty='vlocy')
cube1.imposeDrivenDof(group='CL5',component=[2], dofty='vlocy')

cube1.imposeDrivenDof(group='CL6',component=[1], dofty='vlocy')
cube1.imposeDrivenDof(group='CL6',component=[2], dofty='vlocy',description='evolution',evolutionFile='v.dat')

cube1.imposeDrivenDof(group='CL7',component=[1], dofty='vlocy',description='evolution',evolutionFile='v.dat')
cube1.imposeDrivenDof(group='CL7',component=[2], dofty='vlocy',description='evolution',evolutionFile='v.dat')

cube1.imposeDrivenDof(group='CL8',component=[1], dofty='vlocy',description='evolution',evolutionFile='v.dat')
cube1.imposeDrivenDof(group='CL8',component=[2], dofty='vlocy')
##

# #cube1.imposeDrivenDof(group='down',component=[1,2,3], dofty='vlocy')
# #cube1.imposeDrivenDof(group='up',component=[1,2], dofty='vlocy')
# cube1.imposeDrivenDof(group='left',component=[1,2], dofty='vlocy')
# cube1.imposeDrivenDof(group='rear',component=[1,2], dofty='vlocy')
# cube1.imposeDrivenDof(group='down',component=[3], dofty='vlocy')
# cube1.imposeDrivenDof(group='right',component=[1], dofty='vlocy',description='evolution',evolutionFile='v.dat')
# cube1.imposeDrivenDof(group='front',component=[2], dofty='vlocy',description='evolution',evolutionFile='v.dat')

# 

bodies += cube1

MS.append([(cube1,'right')])


dt=1e-1
ep0=3e6/30e9
du=0.1*ep0

f=open('./DATBOX/v.dat','w')
f.write('%12.5e %12.5e\n' % (   0.     , 0.))
f.write('%12.5e %12.5e\n' % (   0.01   , 0.))
f.write('%12.5e %12.5e\n' % (   0.01+dt, du))
f.write('%12.5e %12.5e\n' % (  61.     , du))
f.write('%12.5e %12.5e\n' % (  61.  +dt,-0.))
f.write('%12.5e %12.5e\n' % (5000.     , 0.))
f.close()


pre.writeBodies(bodies,chemin='DATBOX/')
pre.writeModels(mod,chemin='DATBOX/')
pre.writeBulkBehav(mat,chemin='DATBOX/',dim=dim,gravy=[0.,0.,0.])
pre.writeTactBehav(tacts,svs,chemin='DATBOX/')
pre.writeDrvDof(bodies,chemin='DATBOX/')
pre.writeDofIni(bodies,chemin='DATBOX/')
pre.writeVlocRlocIni(chemin='DATBOX/')
pre.writeGPVIni(bodies,chemin='DATBOX/')
pre.writePostpro(post, bodies, path='DATBOX/')


post=pre.postpro_commands()
post.addCommand(pre.postpro_command(name='NEW MECAx SETS',mecax_sets=MS))
post.addCommand(pre.postpro_command(name='Dep EVOLUTION', step=1))
post.addCommand(pre.postpro_command(name='Fint EVOLUTION', step=1))
post.addCommand(pre.postpro_command(name='SOLVER INFORMATIONS', step=1))
post.addCommand(pre.postpro_command(name='VIOLATION EVOLUTION', step=1))
pre.writePostpro(commands=post, parts=bodies, path='DATBOX/')

#try:
#  pre.visuAvatars(bodies)
#except:
#  pass
