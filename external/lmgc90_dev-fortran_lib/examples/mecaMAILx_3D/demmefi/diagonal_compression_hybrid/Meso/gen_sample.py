import math
import numpy as np
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

  pierre = pre.material(name='mou__',materialType='USER_MAT',density=1200., file_mat='pierre_M1.mat')
  mats.addMaterial(pierre)

  m3Dl = pre.model(name='M3DNL', physics='MECAx', element='H8xxx', dimension=dim, external_model='Demfi',
                   kinematic='small', mass_storage='lump_',user_model_name='ENDO3D',external_fields=['TEMPERATURE'])
  mods.addModel(m3Dl)

else:
  
  pierre = pre.material(name='Gxxxx', materialType='ELAS', elas='standard',
                     young=8.26e9, nu=0.2, anisotropy='isotropic', density=1200.)
  mats.addMaterial(pierre)

  m3Dl = pre.model(name='TET__', physics='MECAx', element='H8xxx', dimension=dim, external_model='no___',
                kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')
  mods.addModel(m3Dl)


ly = (100.)*1e-3
lz = (50.+10. )*1e-3

#half + 0.5*joint
lxh = (108.75)*1e-3 
mh = pre.buildMeshH8(x0=0., y0=0., z0=0., lx=lxh, ly=ly, lz=lz, nb_elem_x=3, nb_elem_y=2, nb_elem_z=3)
half = pre.buildMeshedAvatar(mesh=mh, model=m3Dl, material=pierre)

#full + 2*0.5*joint
lxf = (217.5)*1e-3  
mf = pre.buildMeshH8(x0=0., y0=0., z0=0., lx=lxf, ly=ly, lz=lz, nb_elem_x=6, nb_elem_y=2, nb_elem_z=3)
full = pre.buildMeshedAvatar(mesh=mf, model=m3Dl, material=pierre)


even = [half,full,full,full,half]
odd  = [full,full,full,full]
wall = [even,odd,even,odd,even,odd,even,odd,even,odd,even,odd,even,odd]


hw = (840.)*1e-3
lw = (870.)*1e-3
lc = lw/6

angle = (math.pi/2) - math.atan(hw/lw)

def UL(x):
  return x[0]-(lw-lc-1e-3)>1e-4 and abs(x[2]-hw)<1e-4

def UR(x):
  return abs(x[0]-lw)<1e-4 and x[2]-(hw-lc-1e-3)>1e-4

def DL(x):
  return x[0]<1e-4 and x[2]<=lc

def DR(x):
  return x[0]<=lc and x[2]<1e-4

D=[]
U=[]
z=0.
for i,layer in enumerate(wall):
  x=0.
  for brick in layer:

    if brick==half :
      dx=lxh
    if brick==full:
      dx=lxf
    
    body=deepcopy(brick)
    body.translate(dx=x,dz=z)
    body.addContactors(group='down',shape='ASpxx',color='BLEUx')
    body.addContactors(group='up', shape='CSpxx', color='BLEUx',quadrature=0)
    body.addContactors(group='rear', shape='CSpxx', color='REDxx',quadrature=0)
    body.addContactors(group='front', shape='ASpxx', color='REDxx')
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
    x+=dx
  z+=lz

expoc = pre.tact_behav( name='expo_', law='EXPO_CZM', dyfr=0.8, stfr=0.8,
                        cn=4.00e11, s1=3.30e5, G1=16.,
                        ct=1.67e11, s2=1.63e6, G2=700., eta=1e-8 )
tacts += expoc


# Visibility tables declaration

svcsas1 = pre.see_table(   CorpsCandidat='MAILx',    candidat='CSxxx',    colorCandidat='BLEUx', behav=expoc,
                           CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='BLEUx', alert=1e-2, halo=0.1) 
svs+=svcsas1
svcsas2 = pre.see_table(   CorpsCandidat='MAILx',    candidat='CSxxx',    colorCandidat='REDxx', behav=expoc,
                           CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='REDxx', alert=1e-2, halo=0.1) 
svs+=svcsas2


# ecriture des fichiers
datbox = pathlib.Path('./DATBOX')
datbox.mkdir(exist_ok=True)
mat_file = pathlib.Path('pierre_M1.mat')
shutil.copyfile(mat_file,datbox/mat_file)


ofile = open('./DATBOX/vz.dat','w')
ofile.write('%12.5e %12.5e\n' % (0.,   0.))
ofile.write('%12.5e %12.5e\n' % (1.e-5,0.))
ofile.write('%12.5e %12.5e\n' % (2.e-5,-1e-2))
ofile.write('%12.5e %12.5e\n' % (100., -1e-2))
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
