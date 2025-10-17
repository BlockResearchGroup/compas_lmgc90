import os, sys

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# import des modules
import math, numpy
from pylmgc90 import pre

# definition des conteneurs:
#   * de corps
bodies = pre.avatars()
#   * de materiaux
mats = pre.materials()
mods = pre.models()
#   * de lois de contacts
tacts = pre.tact_behavs()
#   * de tables de visibilite
svs = pre.see_tables()

# exemple 3D
dim = 3

# definition d'un modele rigide
mR3D = pre.model(name='rigid', physics='MECAx', 
                 element='Rxx3D', dimension=dim)
mods.addModel(mR3D)

# definition du materiau pour la fondation
tdur = pre.material(name='TDURx', materialType='RIGID', 
                    density=1000.)
# definition du materiau pour les blocs
stone = pre.material(name='STONE', materialType='RIGID', 
                     density=2750.)
# ajout des materiaux dans le conteneur
mats.addMaterial(tdur); mats.addMaterial(stone)

# parametres du script
#   * nombre de blocs
nb_blocs = 11
#   * ouverture angulaire d'un joint
theta_joint = math.pi/100.
#   * rayons interieur et exterieur
r_int = 0.8; r_ext = 1.

# calcul de l'ouverture corespondant a un bloc
theta_bloc = (math.pi - (nb_blocs - 1)*theta_joint)/ \
   nb_blocs
# calcul de l'epaisseur d'un bloc
e_bloc = r_ext - r_int
# calcul de l'epaisseur d'un joint
e_joint = r_ext*theta_joint

# initialisation de l'angle de debut du prochain bloc
theta = 0.
# pour chaque bloc
for i in range(0, nb_blocs):
   #    * coordonnees des sommets (repere global)
   vertices = numpy.zeros([8, 3], 'd')
   #       - sommet 1
   vertices[0, 0]=r_int*math.cos(theta + theta_bloc)
   vertices[0, 1]=-0.5*e_bloc
   vertices[0, 2]=r_int*math.sin(theta + theta_bloc)
   #       - sommet 2
   vertices[1, 0]=r_int*math.cos(theta)
   vertices[1, 1]=-0.5*e_bloc
   vertices[1, 2]=r_int*math.sin(theta)
   #       - sommet 3
   vertices[2, 0]=r_int*math.cos(theta)
   vertices[2, 1]= 0.5*e_bloc
   vertices[2, 2]=r_int*math.sin(theta)
   #       - sommet 4
   vertices[3, 0]=r_int*math.cos(theta + theta_bloc)
   vertices[3, 1]= 0.5*e_bloc
   vertices[3, 2]=r_int*math.sin(theta + theta_bloc)
   #       - sommet 5
   vertices[4, 0]=r_ext*math.cos(theta + theta_bloc)
   vertices[4, 1]=-0.5*e_bloc
   vertices[4, 2]=r_ext*math.sin(theta + theta_bloc)
   #       - sommet 6
   vertices[5, 0]=r_ext*math.cos(theta)
   vertices[5, 1]=-0.5*e_bloc
   vertices[5, 2]=r_ext*math.sin(theta)
   #       - sommet 7
   vertices[6, 0]=r_ext*math.cos(theta)
   vertices[6, 1]= 0.5*e_bloc
   vertices[6, 2]=r_ext*math.sin(theta)
   #       - sommet 8
   vertices[7, 0]=r_ext*math.cos(theta + theta_bloc)
   vertices[7, 1]= 0.5*e_bloc
   vertices[7, 2]=r_ext*math.sin(theta + theta_bloc)
   #    * connectivite des faces
   faces = numpy.zeros([12, 3], 'i')
   faces[ 0, 0]=1; faces[ 0, 1]=2; faces[ 0, 2]=3
   faces[ 1, 0]=1; faces[ 1, 1]=3; faces[ 1, 2]=4
   faces[ 2, 0]=1; faces[ 2, 1]=2; faces[ 2, 2]=6
   faces[ 3, 0]=1; faces[ 3, 1]=6; faces[ 3, 2]=5
   faces[ 4, 0]=2; faces[ 4, 1]=3; faces[ 4, 2]=7
   faces[ 5, 0]=2; faces[ 5, 1]=7; faces[ 5, 2]=6
   faces[ 6, 0]=1; faces[ 6, 1]=4; faces[ 6, 2]=8
   faces[ 7, 0]=1; faces[ 7, 1]=8; faces[ 7, 2]=5
   faces[ 8, 0]=3; faces[ 8, 1]=4; faces[ 8, 2]=8
   faces[ 9, 0]=3; faces[ 9, 1]=8; faces[ 9, 2]=7
   faces[10, 0]=5; faces[10, 1]=7; faces[10, 2]=8
   faces[11, 0]=5; faces[11, 1]=6; faces[11, 2]=7   

   # creation d'un nouvel avatar rigide pour le bloc
   block = pre.rigidPolyhedron(model=mR3D, material=stone, generation_type='full',
                               vertices=vertices, faces=faces, color='REDxx')
   # ajout du bloc a l'ensemble des corps
   bodies.addAvatar(block)
   
   # actualisation de l'angle pour la contruction du 
   # prochain bloc
   theta += theta_bloc + theta_joint

# creation d'un nouvel avatar rigide pour la fondation
floor = pre.rigidPlan(axe1=r_ext+0.5*e_bloc, axe2=e_bloc, axe3=0.25*e_bloc, center=[0., 0., -0.25*e_bloc],
                      model=mR3D, material=tdur, color='FLOOR')

# condition limites : fondation bloquee
floor.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')

# ajout de la fondation au conteneur de corps
bodies.addAvatar(floor)

# definition d'une loi de contact frottant, avec pre-gap
iqsg0=pre.tact_behav(name='iqsg0', law='IQS_CLB_g0', fric=0.5)
# ajout de la loi dans le conteneur de lois
tacts.addBehav(iqsg0)

# definition d'une table de visibilite pour le
# contact polyedre-polyedre (i.e. entre blocs)
sv = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='REDxx',
                   CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx',
                   behav=iqsg0, alert=e_joint)
# ajout de la table de visibilite dans le conteneur
# de tables de visibilite
svs.addSeeTable(sv)

# definition d'une loi de contact frottant
iqsc0=pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.5)
# ajout de la loi dans le conteneur de lois
tacts.addBehav(iqsc0)

# definition d'une table de visibilite pour le
# contact polyedre-plan (i.e. avec la fondation)
sv = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='REDxx',
                   CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='FLOOR',
                   behav=iqsc0, alert=e_joint)
# ajout de la table de visibilite dans le conteneur
# de tables de visibilite
svs.addSeeTable(sv)

try:
  pre.visuAvatars(bodies)
except:
  pass

post = pre.postpro_commands()
nlgs = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(nlgs)

# ecriture des fichiers de donnees pour LMGC90
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

