###################################################################
# Importation of module pre
from pylmgc90 import pre

# containers:
bodies = pre.avatars()
mods   = pre.models()
mats   = pre.materials()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

# 3D
dim = 3

# porous model definition
Porous = pre.model(name='Poro_', physics='POROx', element='H208x',\
                   user_model_name='NEOHOOKEAN',external_model='MatL_',\
                   kinematic='large',formulation = 'TotaL',\
                   material='elas_',anisotropy='iso__',\
                   mass_storage='lump_', capacity_storage='lump_', dimension=dim,\
                   convection_type = 'char_', physical_type = 'solid')

mods.addModel(Porous)

# non linear material definition
Biot = pre.material(name='biot_', materialType='USER_MAT', file_mat = 'NEOHOOKEAN.mat',density = 0.0)

mats.addMaterial(Biot)

# hexahedron mesh of a cube:
mail = pre.readMesh('MESH/Disc.msh', dim)
conc = pre.buildMeshedAvatar(mesh=mail,model=Porous,material=Biot)
bodies += conc

# boundary conditions:
conc.imposeDrivenDof(group='160', component=[1,2,4] , dofty='vlocy')

conc.imposeDrivenDof(group='161', component=[1,2] , dofty='vlocy')
conc.imposeDrivenDof(group='161', component=3 , dofty='vlocy', description = 'evolution', evolutionFile = 'Vimp_t.txt')

conc.imposeDrivenDof(group='162', component=[1,2,3] , dofty='vlocy')

# application des conditions initiales :
conc.imposeInitValue(group='170',component=[1,2,3,4] ,value=[0.0,0.0,0.0,0.0])
conc.imposeInitValue(group='171',component=[1,2,3,4] ,value=[0.0,0.0,0.0,0.0])


post = pre.postpro_commands()

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mats, mods, bodies, post=post, gravy=[0., 0., 0.])

try:
  pre.visuAvatars(bodies)
except:
  pass
