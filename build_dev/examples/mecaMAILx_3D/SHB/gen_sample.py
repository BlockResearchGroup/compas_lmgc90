import os,sys

from pylmgc90 import pre

space_dim = 3

shell_thickness = 0.01

gravity = [0., 0.,-9.81]

mods   = pre.models()
mats   = pre.materials()
bodies = pre.avatars()
tacts  = pre.tact_behavs()
sees   = pre.see_tables()

rig3D = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=space_dim)
mgrain = pre.material(name='subsM', materialType='RIGID',density=1.e3)

if 0:
  shel3 = pre.model(name='SHEL3', physics='MECAx', element='PRI6x', dimension=space_dim,                      
                    external_model='MatL_', kinematic='large', material='neoh_',
                    formulation='TotaL', mass_storage='lump_', anisotropy='iso__')
else:
  shel3 = pre.model(name='SHEL3', physics='MECAx', element='SHB6x', dimension=space_dim,
                    external_model='MatL_', kinematic='large', material='neoh_',
                    formulation='TotaL', mass_storage='lump_', anisotropy='iso__',
                    )

mods.addModel(shel3)

mplate = pre.material(name='mtubx', materialType='ELAS', density=0.5e3, elas='standard',
                           young=2.1e05, nu=0.3, anisotropy='isotropic')

mats.addMaterial(mplate)
mats.addMaterial(mgrain)

mesh = pre.readMesh("gmsh/plate.msh",space_dim)
mesh.rankRenumbering()
mesh.extrudePhysicalEntity('P_plate', shell_thickness, True )

ref  = pre.avatar(space_dim)
ref.addNodes(mesh.nodes)
ref.addBulks(mesh.bulks)
ref.defineGroups()
ref.defineModel("EP_plate",shel3)
ref.defineMaterial("EP_plate", mplate)

ref.addContactors(group='P_plate', shape='CSpxx', color='BLEUx', reverse=True)
ref.addContactors(group='PP_plate', shape='ASpxx', color='BLEUx')
ref.imposeDrivenDof(group='left',component = [1, 2, 3], dofty = 'vlocy')
ref.imposeDrivenDof(group='right',component = [1, 2, 3], dofty = 'vlocy')
bodies += ref

try:
  pre.visuAvatars(bodies)
except ImportError:
  pass

cs_law = pre.tact_behav(name='rstcl', law='GAP_SGR_CLB', fric=0.1)
tacts.addBehav(cs_law)

alert_ = 0.05
halo_  = 0.1

cs_see = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLEUx',
                       behav= cs_law,
                       CorpsAntagoniste='MAILx',antagoniste='ASpxx', colorAntagoniste='BLEUx',
                       alert=alert_,halo=halo_)

sees.addSeeTable(cs_see)

post = pre.postpro_commands()
nlgs = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(nlgs)

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

pre.writeDatbox(space_dim, mats, mods, bodies, tacts, sees, post=post, gravy=gravity)

