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
cube1.imposeDrivenDof(group='rear',component=[1], dofty='vlocy')
cube1.imposeDrivenDof(group='left',component=[2], dofty='vlocy')
cube1.imposeDrivenDof(group='down',component=[3], dofty='vlocy')
cube1.imposeDrivenDof(group='front',component=[1], dofty='vlocy',description='evolution',evolutionFile='v.dat')
cube1.imposeDrivenDof(group='right',component=[2], dofty='vlocy',description='evolution',evolutionFile='v.dat')
cube1.imposeDrivenDof(group='up',component=[3], dofty='vlocy',description='evolution',evolutionFile='v.dat')
bodies += cube1

MS.append([(cube1,'up')])


dt=1e-1

ept0=1.0*(3.e6/30.e9)
du=0.1*ept0/10.

print(du)

f=open('./DATBOX/v.dat','w')
t=0.
f.write('%12.5e %12.5e\n' % ( t   ,  0.))
t=t+1.
f.write('%12.5e %12.5e\n' % ( t   , 0.))
f.write('%12.5e %12.5e\n' % ( t+dt,-du))
Dt=30.*0.1*ept0/du
t+=Dt
print(t-1,Dt*du)
f.write('%12.5e %12.5e\n' % ( t   ,-du))
f.write('%12.5e %12.5e\n' % ( t+dt,-0.))
f.write('%12.5e %12.5e\n' % (t+1000.  ,  0.))
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
