import os, sys

from pylmgc90 import pre

# definition des conteneurs:
bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()
post   = pre.postpro_commands()

# on se place en 3D
dim = 3

# materiau rigide

# creation d'un materiau
tdur = pre.material(name='TDURx', materialType='RIGID', density=1800.)
mats.addMaterial(tdur)

# creation d'un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)

# creation d'un materiau elastique
stone = pre.material(name='stone', materialType='ELAS', elas='standard',
                     young=1.e10, nu=0.15, anisotropy='isotropic', density=1800.)
mats.addMaterial(stone)

# creation d'un modele elastique lineaire en petites deformations
te4ml = pre.model(name='TE4ML', physics='MECAx', element='TE4xx', dimension=dim,
                  external_model='MatL_', kinematic='small', material='elas_',
                  anisotropy='iso__', mass_storage='lump_')
mods.addModel(te4ml)

# on lit le maillage total dans un fichier, au format gmsh
complete_mesh = pre.readMesh(name='gmsh/2_briques.msh', dim=dim)

# on separe les differents maillages volumiques stockes dans le fichier, corespondants aux differents 
# solides : la dalle, la brique rigide et la brique deformable
entity2mesh=complete_mesh.separateMeshes(dim=dim, 
   entity_type="physicalEntity", keep_all_elements=True)

mesh_dalle=entity2mesh["dalle"]
mesh_rigid_brick=entity2mesh["brique rigide"]
mesh_deformable_brick=entity2mesh["brique deformable"]

# on construit un rigide a partir du maillage de la dalle
body_dalle = pre.volumicMeshToRigid3D(volumic_mesh=mesh_dalle, model=mod, material=tdur, color='BLACK')   
bodies.addAvatar(body_dalle)
body_dalle.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')

# on construit un rigide a partir du maillage de la brique rigide
body_rigid_brick = pre.volumicMeshToRigid3D(volumic_mesh=mesh_rigid_brick, model=mod, material=tdur, color='BLEUx')
bodies.addAvatar(body_rigid_brick)

# on construit un maille a partir de la brique deformable
body_deformable_brick = pre.buildMeshedAvatar(mesh=mesh_deformable_brick, model=te4ml, material=stone)
bodies.addAvatar(body_deformable_brick)
body_deformable_brick.addContactors(group='10', shape='CSpxx', color='REDxx')


# definitions des interactions
#  * loi d'interaction :
lprpr = pre.tact_behav('iqsG0', 'IQS_CLB_g0', fric=0.3)
tacts+= lprpr
lcspr = pre.tact_behav('gapG0', 'GAP_SGR_CLB_g0', fric=0.3)
tacts+= lcspr
#  * table de visibilite :
svprpr = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLEUx',
                       behav=lprpr, alert=2.5e-2)
svs += svprpr
svprpr2 = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLEUx',
                        CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLACK',
                        behav=lprpr, alert=2.5e-1)
svs += svprpr2
svcspr = pre.see_table(CorpsCandidat='MAILx', candidat='CSxxx', colorCandidat='REDxx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLACK',
                       behav=lcspr, alert=2.5e-2, halo=1.e0)
svs+=svcspr

# definition des sets pour suivre la brique deformable :
mecax_set = [(body_deformable_brick, "coin")]
deformable_brick_set = pre.postpro_command(name='NEW MECAx SETS', mecax_sets=[mecax_set])

# ajout de la commande au conteneur de commandes
post.addCommand(deformable_brick_set)

# suivi de la brique deformable :
deformable_brick_disp = pre.postpro_command(name='Dep EVOLUTION', step=1)
deformable_brick_fint = pre.postpro_command(name='Fint EVOLUTION', step=1)

post.addCommand(deformable_brick_disp)
post.addCommand(deformable_brick_fint)

# suivi de la brique rigide :
rigid_brick_disp = pre.postpro_command(name='BODY TRACKING', step=1, rigid_set=[body_rigid_brick])
rigid_brick_torque = pre.postpro_command(name='TORQUE EVOLUTION', step=1, rigid_set=[body_rigid_brick])

post.addCommand(rigid_brick_disp)
post.addCommand(rigid_brick_torque)

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# ecriture des fichiers de donnees
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass
