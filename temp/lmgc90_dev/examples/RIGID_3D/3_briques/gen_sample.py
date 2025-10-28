import os, sys

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# definition des conteneurs:
bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

# on se place en 3D
dim = 3

# on cree deux materiaux rigides
tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
pdur = pre.material(name='MOUxx',materialType='RIGID',density=100.)
mats.addMaterial(tdur,pdur)

# on cree un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod)

# on lit le maillage total dans un fichier, au format gmsh
complete_mesh = pre.readMesh(name='gmsh/3_briques.msh', dim=dim)

# on separe les differents maillages volumiques stockes dans le fichier, corespondants a differents solides
entity2mesh=complete_mesh.separateMeshes(dim=dim, entity_type="geometricalEntity", keep_all_elements=False)

# on pour chaque maillage volumique
# on construit un rigide a partir du maillage :
for volumic_mesh in list(entity2mesh.values()):
   body = pre.volumicMeshToRigid3D(volumic_mesh=volumic_mesh, model=mod, material=pdur, color='BLEUx')
   bodies.addAvatar(body)

# creation d'un corps pour la fondation
down=pre.rigidPlan(axe1=2.5e-1, axe2=1.e-1, axe3=1.25e-2, center=[2.e-1, 2.5e-2, -1.25e-2],
                   model=mod, material=tdur, color='VERTx') 
down.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies.addAvatar(down)

#visuAvatars(bodies)

# gestion des interactions :
#   * declaration des lois
lprpr=pre.tact_behav(name='iqsc0', law='IQS_CLB_g0', fric=0.3)
tacts+=lprpr
lprpl=pre.tact_behav(name='iqsc1', law='IQS_CLB', fric=0.5)
tacts+=lprpl
#   * declaration des tables de visibilite
svprpr = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLEUx',
                       behav=lprpr, alert=1.25e-2)
svs+=svprpr
svprpl = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='VERTx',
                       behav=lprpl, alert=1.25e-2)
svs+=svprpl

post=pre.postpro_commands()
my_command=pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(my_command)

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass
