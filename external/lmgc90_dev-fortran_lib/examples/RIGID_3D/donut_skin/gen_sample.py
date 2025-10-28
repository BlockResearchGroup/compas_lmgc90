import os,sys
import math

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# definition des conteneurs:
#   * de corps
bodies = pre.avatars()
#   * de materiaux
mats = pre.materials()
#   * de modeles
mods = pre.models()
#   * pour les tables de visibilite
svs = pre.see_tables()
#   * pour les lois de contact
tacts = pre.tact_behavs()

# on se place en 3D
dim = 3

# materiau rigide

# creation d'un materiau
tdur = pre.material(name='TDURx', materialType='RIGID', density=2500.)
mats.addMaterial(tdur)

# creation d'un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod)

# on lit le maillage total dans un fichier, au format gmsh
mesh_all = pre.readMesh(name='./gmsh/All_skin.msh', dim=dim)

# on retouve les maillages des enveloppes des differents objets du maillage
pre.identifyEntitiesInSurfacicMesh(surfacic_mesh=mesh_all)

# on separe les differents maillages surfaciques stockes dans le fichier, corespondants aux 
# enveloppes des differents solides : le donut et la fondation
entity2mesh=mesh_all.separateMeshes(dim=2, 
   entity_type="geometricalEntity", keep_all_elements=False)

# on recupere le maillage
#    * du donut
mesh_donut=entity2mesh["1"]
#    * de la fondation
mesh_fondation=entity2mesh["2"]

# on construit un rigide a partir du maillage de l'enveloppe du donut
body_donut = pre.surfacicMeshToRigid3D(surfacic_mesh=mesh_donut, model=mod, material=tdur, color='BLEUx')
# on ajoute le donut au conteneur de corps
bodies.addAvatar(body_donut)
# on depalce le donut pour qu'il puisse rouler sur la fondation... 
body_donut.rotate(psi=0.5*math.pi, center=body_donut.nodes[1].coor)

# Gestion de la fondation (a l'ancienne)

# on construit un rigide a partir du maillage de la fondation
body_fondation = pre.surfacicMeshToRigid3D(surfacic_mesh=mesh_fondation, model=mod, material=tdur, color='BLEUx')   
# on ajoute la fondation au conteneur de corps
bodies.addAvatar(body_fondation)
# on bloque la fondation
body_fondation.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
# on deplace la fondation pour que le donut puisse lui rouler dessus... 
body_fondation.translate(dz=-0.02)
body_fondation.rotate(theta=-0.1*math.pi, center=body_fondation.nodes[1].coor)

# definitions des interactions
#  * loi d'interaction :
lprpr=pre.tact_behav('iqsc0', 'IQS_CLB', fric=0.3)
tacts+=lprpr

#  * table de visibilite :
svprpr = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLEUx',
                       behav=lprpr, alert=1.e-2)
svs+=svprpr

post = pre.postpro_commands()

# ecriture des fichiers de donnees
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass
