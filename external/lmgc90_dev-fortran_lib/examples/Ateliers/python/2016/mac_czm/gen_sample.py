
import os
from copy import deepcopy

from pylmgc90 import pre

dim = 3

bodies = pre.avatars()
mat    = pre.materials()
mod    = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()
post   = pre.postpro_commands()

steel = pre.material(name='Steel', materialType='ELAS', elas='standard',
                     young=1.3e8, nu=0.3, anisotropy='isotropic', density=2500.)  
mat.addMaterial(steel)

m3Dl = pre.model(name='M2DNL', physics='MECAx', element='H8xxx', dimension=dim, external_model='MatL_',
                 kinematic='large', material='neoh_', anisotropy='iso__', mass_storage='lump_',formulation='TotaL')
mod.addModel(m3Dl)

mesh_block = pre.buildMeshH8(x0=0., y0=0., z0=0., lx=1., ly=1., lz=1., nb_elem_x=1, nb_elem_y=1, nb_elem_z=1)

cube1 = pre.buildMeshedAvatar(mesh=mesh_block, model=m3Dl, material=steel)
cube2 = deepcopy(cube1)
cube2.translate(dz=1.)

cube1.addContactors(group='up'  , shape='ASpxx', color='BLUEx')
cube2.addContactors(group='down', shape='CSpxx', color='BLUEx', quadrature=0)

cube1.imposeDrivenDof(group='down',component=[1, 2, 3], dofty='vlocy')
cube2.imposeDrivenDof(group='up'  ,component=[1, 2]   , dofty='vlocy')
cube2.imposeDrivenDof(group='up'  ,component= 3       , dofty='vlocy', ct=4.5e-2)

bodies += cube1
bodies += cube2


maccz = pre.tact_behav( name='mac__', law='MAC_CZM', dyfr=0.1, stfr=0.1,
                        cn=1.e12, ct=1.e12, b=0., w=0.1 )

tacts += maccz

seet = pre.see_table(CorpsCandidat   ='MAILx', candidat   ='CSxxx', colorCandidat   ='BLUEx', behav=maccz,
                     CorpsAntagoniste='MAILx', antagoniste='ASpxx', colorAntagoniste='BLUEx', alert=0.1)

svs += seet

# ecriture des fichiers
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

pre.writeBodies(bodies,chemin='DATBOX')
pre.writeModels(mod,chemin='DATBOX')
pre.writeBulkBehav(mat,chemin='DATBOX',dim=dim,gravy=[0.,0.,0.])
pre.writeTactBehav(tacts,svs,chemin='DATBOX')
pre.writeDrvDof(bodies,chemin='DATBOX')
pre.writeDofIni(bodies,chemin='DATBOX')
pre.writeVlocRlocIni(chemin='DATBOX')
pre.writeGPVIni(bodies,chemin='DATBOX')
pre.writePostpro(post, bodies, path='DATBOX')

try:
  pre.visuAvatars(bodies)
except:
  pass
