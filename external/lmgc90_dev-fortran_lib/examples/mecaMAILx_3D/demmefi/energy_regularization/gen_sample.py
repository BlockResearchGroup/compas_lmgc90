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


# Endo3D model

pierre = pre.material(name='mou__',materialType='USER_MAT',density=0., file_mat='pierre_mod.mat')
mat.addMaterial(pierre)

m3Dl = pre.model(name='M3DNL', physics='MECAx', element='H8xxx', dimension=dim, external_model='Demfi',
                 kinematic='small', mass_storage='lump_',user_model_name='ENDO3D',external_fields=['TEMPERATURE'])
mod.addModel(m3Dl)


# Elastic model

matE = pre.material(name='Gxxxx', materialType='ELAS', elas='standard',
                     young=30.0e9, nu=0.2, anisotropy='isotropic', density=0.)
mat.addMaterial(matE)

modE = pre.model(name='TET__', physics='MECAx', element='H8xxx', dimension=dim, external_model='MatL_',
                kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')
mod.addModel(modE)


MS=[]

mesh = pre.buildMeshH8(x0=0., y0=0., z0=0., lx=0.1, ly=0.1, lz=0.1, nb_elem_x=1, nb_elem_y=3, nb_elem_z=1)

cube = pre.buildMeshedAvatar(mesh=mesh, model=modE, material=matE)


# geometry dimensions in the x and y plane
h = 0.1
h1 = 0.02
#h1 = 0.04
#h1 = 0.06
#h1 = 0.08
d1 = (h-h1)/2

cube.nodes[3].coor[1]=d1
cube.nodes[4].coor[1]=d1
cube.nodes[11].coor[1]=d1
cube.nodes[12].coor[1]=d1

cube.nodes[5].coor[1]=d1+h1
cube.nodes[6].coor[1]=d1+h1
cube.nodes[13].coor[1]=d1+h1
cube.nodes[14].coor[1]=d1+h1


def V2(x):
  return d1<=x[1] and x[1]<=d1+h1

cube.addGroupUsingPredicate(name='Volume_2', predicate=V2)

cube.defineModel(group='Volume_2', model=m3Dl)
cube.defineMaterial(group='Volume_2',material=pierre)

cube.imposeDrivenDof(group='left',component=[2], dofty='vlocy')
cube.imposeDrivenDof(group='rear',component=[1], dofty='vlocy')
cube.imposeDrivenDof(group='rear',component=[1], dofty='vlocy')
cube.imposeDrivenDof(group='rear',component=[1], dofty='vlocy')
cube.imposeDrivenDof(group='down',component=[3], dofty='vlocy') 
cube.imposeDrivenDof(group='right',component=[2], dofty='vlocy',description='evolution',evolutionFile='vy.dat')

bodies += cube

MS.append([(cube,'right')]) 


dt=1e-1

ep0=(3.e6/30.e9)
du=0.1*ep0/10.

print(du)

f=open('./DATBOX/vy.dat','w')
t=0.
f.write('%12.5e %12.5e\n' % ( t   , 0.))
t=t+1.
f.write('%12.5e %12.5e\n' % ( t   , 0.))
f.write('%12.5e %12.5e\n' % ( t+dt, du))
Dt=20.*0.1*ep0/du
t+=Dt
print(t-1,Dt*du)
f.write('%12.5e %12.5e\n' % ( t   , du))
f.write('%12.5e %12.5e\n' % ( t+dt,-0.    ))
t+=Dt
print(t-1)
f.write('%12.5e %12.5e\n' % (100.  , 0.   ))
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

# try:
#   pre.visuAvatars(bodies)
# except:
#   pass
