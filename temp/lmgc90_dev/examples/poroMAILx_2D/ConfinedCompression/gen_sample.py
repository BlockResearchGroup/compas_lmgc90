###################################################################
# Importation of module pre
from pylmgc90 import pre

# containers:
bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

# 2D
dim = 2

# porous model definition on Quad8 2D mesh
Porous_HPP_Q8 = pre.model(name='hpp_8', physics='POROx', element='Q84xx',\
                               dimension = dim, \
                               external_model='no___', kinematic='small',\
                               material='elas_',anisotropy='iso__',\
                               mass_storage='coher',\
                               physical_type = 'solid',\
                               capacity_storage='lump_',\
                               convection_type = 'supg_')

mods.addModel(Porous_HPP_Q8)

# porous model definition on Tri6 2D mesh
Porous_HPP_T6 = pre.model(name='hpp_6', physics='POROx', element='T63xx',\
                               dimension = dim, \
                               external_model='no___', kinematic='small',\
                               material='elas_',anisotropy='iso__',\
                               mass_storage='coher',\
                               physical_type = 'solid',\
                               capacity_storage='lump_',\
                               convection_type = 'supg_')

mods.addModel(Porous_HPP_T6)


# porous model definition on Quad8 2D mesh for large deformation
Porous_GD_Q8 = pre.model(name='gd__8', physics='POROx', element='Q84xx',\
                              dimension = dim, \
                              external_model='MatL_', kinematic='large',formulation = 'TotaL',\
                              anisotropy='iso__',\
                              mass_storage='coher',\
                              physical_type = 'solid',\
                              capacity_storage='lump_',\
                              convection_type = 'supg_', \
                              user_model_name='NEOHOOKEAN_EOS')

mods.addModel(Porous_GD_Q8)

# porous model definition on Tri6 2D mesh for large deformation
Porous_GD_T6 = pre.model(name='gd__6', physics='POROx', element='T63xx',\
                              dimension = dim, \
                              external_model='MatL_', kinematic='large',formulation = 'TotaL',\
                              anisotropy='iso__',\
                              mass_storage='coher',\
                              physical_type = 'solid',\
                              capacity_storage='lump_',\
                              convection_type = 'supg_', \
                              user_model_name='NEOHOOKEAN_EOS')

mods.addModel(Porous_GD_T6)

# linear material definition
Biot = pre.material(name='line_', materialType='PORO_ELAS', density=0.0, \
                         specific_capacity=0.0, conductivity=0.0075, \
                         elas = 'standard', young = 0.4667, nu = 0.1667, \
                         anisotropy = 'isotropic',hydro_cpl = 1.0)  

mats.addMaterial(Biot)

# non linear material definition
Mat_gd = pre.material(name='noli_', materialType='USER_MAT', density=0.0,file_mat = 'NEOHOOKEAN_EOS.mat')

mats.addMaterial(Mat_gd)

# definition of Qua8 mesh:
mail1   = pre.readMesh('MESH/MeshQ8.msh', dim)
Q8_hpp  = pre.buildMeshedAvatar(mesh=mail1,model=Porous_HPP_Q8,material=Biot)
bodies += Q8_hpp

# definition of a Tri6 mesh:
mail2   = pre.readMesh('MESH/MeshT6.msh', dim)
T6_hpp  = pre.buildMeshedAvatar(mesh=mail2,model=Porous_HPP_T6,material=Biot)
bodies += T6_hpp

# translating mesh
T6_hpp.translate(dy=3.0)

# definition of Qua8 mesh:
mail3   = pre.readMesh('MESH/MeshQ8.msh', dim)
Q8_gd   = pre.buildMeshedAvatar(mesh=mail1,model=Porous_GD_Q8,material=Mat_gd)
bodies += Q8_gd

# translating mesh
Q8_gd.translate(dy=6.0)

# definition of a Tri6 mesh:
mail4   = pre.readMesh('MESH/MeshT6.msh', dim)
T6_gd   = pre.buildMeshedAvatar(mesh=mail2,model=Porous_GD_T6,material=Mat_gd)
bodies += T6_gd

# translating mesh
T6_gd.translate(dy=9.0)

for b in bodies:
  # boundary conditions
  # [CT......+......AMP..*..cos.(..OMEGA.*.time.+.PHI..)] *  [RAMPI.....+.....RAMP.*.time]
  b.imposeDrivenDof(group='7', component=2 , dofty='vlocy')
  b.imposeDrivenDof(group='10', component=1 , dofty='vlocy')
  b.imposeDrivenDof(group='8', component=1 , dofty='vlocy')
  b.imposeDrivenDof(group='9', component=2 , dofty='vlocy',\
                    description = 'evolution', evolutionFile = 'Vimp_t.txt')
  b.imposeDrivenDof(group='9', component=3 , dofty='vlocy')

  # initial values
  b.imposeInitValue(group='11',component=1 ,value=0.0)
  b.imposeInitValue(group='11',component=2 ,value=0.0)
  b.imposeInitValue(group='11',component=3 ,value=0.0)


post = pre.postpro_commands()

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mats, mods, bodies, post=post, gravy=[0., 0., 0.])

try:
  pre.visuAvatars(bodies)
except:
  pass

