import os

import shutil

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

dim = 3

bodies = pre.avatars()
mats   = pre.materials()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()
mods   = pre.models()

## rigide

mod = pre.model(name='M3D_T',physics='MECAx',element='H8xxx',dimension=dim, external_model='MatL_',
                kinematic='large',mass_storage='coher', formulation='TotaL',
                user_model_name='ISOTROPIC_THERMO_DEPENDENT_HYPER_ELASTICITY', external_fields=['TEMPERATURE','density'])
mods+=mod

#mat = pre.material(name='mou__',materialType='USER_MAT',density='2.5e3', file_mat='pierre.mat')
mat = pre.material(name='mou__',materialType='USER_MAT',density='field', file_mat='pierre.mat')
mats.addMaterial(mat)


m1=pre.buildMeshH8(x0=0., y0=0., z0=0., lx=1., ly=2., lz=1., nb_elem_x=2, nb_elem_y=4, nb_elem_z=2)
c1=pre.buildMeshedAvatar(mesh=m1, model=mod, material=mat)
c1.imposeDrivenDof(group='down', component=[1,2,3], dofty='vlocy')
c1.addContactors(group='up'  , shape='ASpxx', color='BLUE_')
bodies+=c1


m2=pre.buildMeshH8(x0=0., y0=0., z0=1., lx=1., ly=2., lz=1., nb_elem_x=2, nb_elem_y=4, nb_elem_z=2)
c2=pre.buildMeshedAvatar(mesh=m2, model=mod, material=mat)
c2.imposeDrivenDof(group='up', component=3, ct=-1000., dofty='force')
c2.addContactors(group='down', shape='CSpxx', color='BLUE_',quadrature=1)
bodies+=c2

# gestion des interactions :

# CZM non endommageables
b=pre.tact_behav( name='expo_', law='EXPO_CZM',dyfr=0.80,stfr=0.80,
                  cn=1.0e+11, s1=1.00e+10, G1= 4.5e+20,
                  ct=1.0e+11, s2=5.00e+10, G2=3.e+22,
                  eta=1e-4)
tacts+=b

#   * declaration des tables de visibilite
sv = pre.see_table(CorpsCandidat='MAILx',candidat='CSxxx',colorCandidat='BLUE_',behav=b,
                   CorpsAntagoniste='MAILx',antagoniste='ASpxx',colorAntagoniste='BLUE_',alert=2e-3,halo=0.8)
svs+=sv

shutil.copyfile(r'pierre.mat',r'DATBOX/pierre.mat')

post=pre.postpro_commands()

MS=[[(c1,'down')],[(c1,'up')],[(c2,'down')],[(c2,'up')]]

post.addCommand(pre.postpro_command(name='NEW MECAx SETS',mecax_sets=MS))
post.addCommand(pre.postpro_command(name='Dep EVOLUTION', step=1))
post.addCommand(pre.postpro_command(name='Fint EVOLUTION', step=1))
post.addCommand(pre.postpro_command(name='SOLVER INFORMATIONS', step=1))
post.addCommand(pre.postpro_command(name='VIOLATION EVOLUTION', step=1))

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post, datbox_path='DATBOX', gravy=[0.,0.,0.0])

try:
    pre.visuAvatars(bodies,True,drvdof_color=[1,0,0])
except:
    pass

