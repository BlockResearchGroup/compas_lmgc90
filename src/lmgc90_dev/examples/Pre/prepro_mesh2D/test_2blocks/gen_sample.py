import os,sys

import numpy

from pylmgc90 import pre

# on se place en 2D
dim=2

# creation des conteneurs
#   * pour les corps
bodies = pre.avatars()
#   * pour les materiaux
mats   = pre.materials()
#   * pour les modeles
mods   = pre.models()
#   * pour les tables de visibilite
svs    = pre.see_tables()
#   * pour les lois de contact
tacts  = pre.tact_behavs()
#   * pour les commandes de post-traitement
post = pre.postpro_commands()

# creations de deux materiaux
#   * un rigide pour la fondation
tdur = pre.material(name='TDURx',materialType='RIGID',density=2500.)
# ajout du materiau
mats.addMaterial(tdur)
#   * un materiau elastique lineaire isotrope pour les blocs :
stone = pre.material(name='stone', materialType='ELAS', elas='standard',
                     young=7.e10, nu=0.2, anisotropy='isotropic', density=2750.)  
# ajout du materiau
mats.addMaterial(stone)

# definition du modele elastique :
m2Dl = pre.model(name='M2D_L', physics='MECAx', element='Q4xxx', dimension=dim, external_model='MatL_',
                 kinematic='small', material='elas_', anisotropy='iso__', mass_storage='lump_')
# ajout du modele
mods.addModel(m2Dl)

# on cree un modele de rigide
mR2D = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

# construction du bloc du dessous

# on genere le maillage du bloc
mesh_block = pre.buildMesh2D(mesh_type='Q4', x0=0., y0=0., lx=0.15, ly=0.05, nb_elem_x=15, nb_elem_y=5)
# on contruit un corps maille a partir du maillage du bloc
body = pre.buildMeshedAvatar(mesh=mesh_block, model=m2Dl, material=stone)

# contacteurs antagonistes sur le dessus du bloc
body.addContactors(group='up', shape='ALpxx', color='REDxx')

# contacteurs candidats sur le dessous
body.addContactors(group='down', shape='CLxxx', color='REDxx')

# ajout du corps a la liste des corps
bodies += body

# construction du bloc du dessus

# on genere le maillage du bloc
mesh_block = pre.buildMesh2D('Q4', x0=0.025, y0=0.05, lx=0.10, ly=0.05, nb_elem_x=10, nb_elem_y=5)
# on contruit un corps maille a partir du maillage du bloc
body = pre.buildMeshedAvatar(mesh=mesh_block, model=m2Dl, material=stone)

# contacteurs candidats sur le dessous
body.addContactors(group='down', shape='CLxxx', color='REDxx')

# post-processing : suivi d'un point du CLxxx du groupe 'down' de l'avatar courant

# predicat permettant d'identifier le CLxxx
def f(x):
   return abs(x[0] - 0.075) < 0.005

# definition de l'ensemble des points candidats a suivre
top_set = pre.CLxxx_set(body=body, group='down', predicate=f)
# definition de la commande de post-traitement ad hoc
clxxx_analysis = pre.postpro_command(name='CLxxx ANALYSIS', step=1, CLxxx_sets=[top_set])
# ajout de la commande dans le container de commandes de post-traitement
post.addCommand(clxxx_analysis)

# ajout du corps a la liste des corps
bodies += body

# ajout d'une fondation rigide, i.e. faite d'un jonc :

# on declare un corps pour la fondation
floor = pre.avatar(dimension=dim)

# on attribue un comportement volumique a la fondation
floor.addBulk( pre.rigid2d() )

# on positionne la fondation dans l'espace
floor.addNode( pre.node(coor=numpy.array([0.075, -0.005]),
                        number=1) )

# on definit les groupes
floor.defineGroups()

# on definit le modele pour chaque paroi
floor.defineModel(model=mR2D)

# on definit le materiau pour chaque paroi
floor.defineMaterial(material=tdur)

# on affecte un contacteur jonc a la fondation
floor.addContactors(shape='JONCx', color='WALLx', axe1=0.08, axe2=0.005)

# on calcule la surface et l'inertie de la fondation
floor.computeRigidProperties()

# on ajoute la fondation a la liste des corps
bodies += floor

# on fixe la fondation
floor.imposeDrivenDof(component=[1, 2, 3],dofty='vlocy')

# gestion des interactions :
#   * declaration des lois
#       - entre les blocs
lclalp = pre.tact_behav(name='gapc0',law='GAP_SGR_CLB',fric=0.3)
tacts += lclalp
#       - avec la fondation
lcljc  = pre.tact_behav(name='gapc1',law='GAP_SGR_CLB',fric=0.5)
tacts += lcljc
#   * declaration des tables de visibilite
#       - entre blocs
svclalp = pre.see_table(CorpsCandidat='MAILx', candidat='CLxxx', colorCandidat='REDxx',
                        CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='REDxx',
                        behav=lclalp, alert=0.001)
svs += svclalp
#       - avec la fondation
svcljc = pre.see_table(CorpsCandidat='MAILx', candidat='CLxxx', colorCandidat='REDxx',
                       CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='WALLx',
                       behav=lcljc, alert=0.001)
svs += svcljc

# ecriture des fichiers
if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass
