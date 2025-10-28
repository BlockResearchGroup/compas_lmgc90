import numpy
from pylmgc90.pre import *

# on se place en 2D
dim=2

# creation des conteneurs
#   * pour les corps
bodies = avatars()
#   * pour les materiaux
mat = materials()
#   * pour les modeles
mod = models()
#   * pour les tables de visibilite
svs = see_tables()
#   * pour les lois de contact
tacts = tact_behavs()
#   * pour les commandes de post-traitement
post = postpro_commands()

# creations de deux materiaux
#   * un rigide pour la bille
tdur = material(name='TDURx',materialType='RIGID',density=2500.)
# ajout du materiau
mat.addMaterial(tdur)
#   * un materiau elastique lineaire isotrope pour la fondation :
acier = material(name='acier', materialType='ELAS', elas='standard',
   young=2.1e11, nu=0.3, anisotropy='isotropic', density=8000.)  
# ajout du materiau
mat.addMaterial(acier)

# definition du modele elastique :
m2Dl = model(name='M2D_L', physics='MECAx', element='Q4xxx', dimension=dim, external_model='MatL_',
     kinematic='small', material='elas_', anisotropy='iso__', mass_storage='coher')
# ajout du modele
mod.addModel(m2Dl)

# on cree un modele de rigide
mR2D = model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

# construction de la fondation

# on genere le maillage de la fondation
mesh_floor=buildMesh2D(mesh_type='Q4', x0=0., y0=0., lx=0.15, ly=0.05, nb_elem_x=15, nb_elem_y=5)
# on contruit un corps maille a partir du maillage de la fondation
floor=buildMeshedAvatar(mesh=mesh_floor, model=m2Dl, material=acier)

# contacteurs antagonistes sur le dessus de la fondation
floor.addContactors(group='up', shape='ALpxx', color='REDxx')

# on fixe la fondation
floor.imposeDrivenDof(group='down', component=[1, 2],dofty='vlocy')

# ajout du corps a la liste des corps
bodies.addAvatar(floor)

# ajout de la bille rigide :

# on declare un corps pour la bille
ball=avatar(dimension=dim)

# on attribue un comportement volumique a la bille
ball.addBulk( rigid2d() )

# on positionne la bille dans l'espace
ball.addNode( 
     node(coor=numpy.array([0.075, 0.1]),
     number=1) )

# on definit les groupes
ball.defineGroups()

# on definit le modele pour la bille
ball.defineModel(model=mR2D)

# on definit le materiau pour la bille
ball.defineMaterial(material=tdur)

# on affecte un contacteur disque a la bille
ball.addContactors(shape='DISKx', color='BLEUx', byrd=0.01)

# on calcule la surface et l'inertie de la bille
ball.computeRigidProperties()

# on ajoute la bille a la liste des corps
bodies.addAvatar(ball)

# creation d'une nouvelle bille en copiant la premiere
new_ball =

# translation de la nouvelle bille : x <-x + 0.02

# ajout de la nouvelle bille dans le conteneur

# boucle d'ajout des autres billes
for i in range(5):
   # creation d'une nouvelle bille par copie de la
   # precedente
   new_ball =

   # rotation de la bille autour de la premiere
   #    - centre : (0.075, 0.1)
   #    - angle : pi/3

   # ajout de la nouvelle bille au conteneur

# translation du conteneur de corps pour que l'axe (0y)
# devienne un axe de symetrie : x <- x-0.075

# gestion des interactions :
#   * declaration de la loi de contact entre la bille et la fondation
ldkalp=tact_behav(name='gapc0',law='GAP_SGR_CLB',fric=0.3)
tacts.addBehav(ldkalp)
#   * declaration de la table de visibilite entre la bille et la fondation
svcljc = see_table(CorpsCandidat='RBDY2', candidat='DISKx',
   colorCandidat='BLEUx', behav=ldkalp, CorpsAntagoniste='MAILx', 
   antagoniste='ALpxx', colorAntagoniste='REDxx', alert=0.001)
svs.addSeeTable(svcljc)

# definition d'une loi de contact frottant, adaptee
# au contact rigide/rigide (coefficient de
# frottement : 0.3)
iqsc0=

# ajout de la loi dans le conteneur de lois

# definition d'une table de visibilite pour le
# contact disque-disque (i.e. bille-bille)
#    - candidat : disque rigide
#    - antagoniste : disque
#    - loi de contact : iqsc0
#    - distance d'alerte : 1 mm
sv = 

# ajout de la table de visibilite dans le conteneur
# de tables de visibilite

# ecriture des fichiers
writeBodies(bodies,chemin='DATBOX/')
writeModels(mod,chemin='DATBOX/')
writeBulkBehav(mat,chemin='DATBOX/',dim=dim)
writeTactBehav(tacts,svs,chemin='DATBOX/')
writeDrvDof(bodies,chemin='DATBOX/')
writeDofIni(bodies,chemin='DATBOX/')
writeVlocRlocIni(chemin='DATBOX/')
writeGPVIni(bodies,chemin='DATBOX/')
writePostpro(post, bodies, path='DATBOX/')
