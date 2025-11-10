from pylmgc90.pre import *
import numpy

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

# definition d'un modele rigide, en 2D
mR2D = 

# definition d'un materiau rigide :
#   - nom : 'TDURx'
#   - masse volumique : 2500 kg/m^3
tdur =

# ajout du materiau au conteneur de materiaux


# definition d'un modele elastique, lineaire, isotrope,
# hpp, en 2D, avec les options suivantes :
#    - adaptes aux quadrangles lineaires ('Q4xxx')
#    - modele externe
#    - stockage de la matrice de masse : coherent
m2Dl = 

# ajout du modele au conteneur de modeles


# definition d'un materiau elastique, isotrope
#    - nom : 'acier'
#    - masse volumique : 8000 kg/m^3
#    - module d'Young : 210 GPa
#    - coefficient de Poisson : 0.3
acier =

# ajout du materiau au conteneur de materiaux


# generation du maillage en Q4 pour la fondation :
#    - coin inferieur gauche en : (0, 0)
#    - longueur : 15 cm ; hauteur : 5 cm
#    - nombre d'elements : 15 en x, 5 en y
mesh_floor = 

# definition de l'avatar pour la fondation
floor=

# ajout de contacteurs "lignes antagonistes" (ALpxx)
# sur le dessus de la fondation
#    - groupe : 'up'
#    - couleur : 'REDxx'


# ancrage de la fondation (v_x=v_y=0 ; groupe : 'down')


# ajout de la fondation au conteneur d'avatars


# declaration d'un avatar pour la bille
ball=

# ajout d'un element a la bille


# ajout d'un noeud a la bille
#     - numero : 1
#     - coordonnees : (7.5 cm, 10 cm)


# definition des groupes pour la bille


# definition du modele de la bille


# definition du materiau de la bille


# affectation d'un contacteur disque a la bille
#    - rayon : 1 cm
#    - coluleur : 'BLEUx'


# calcul la surface et l'inertie de la bille


# ajout la bille a la liste des corps


# definition d'une loi de contact frottant, adaptee
# au contact rigide/deformable (coefficient de
# frottement : 0.3)
gapc0=

# ajout de la loi dans le conteneur de lois


# definition d'une table de visibilite pour le
# contact disque-ligne (i.e. bille-fondation)
#    - candidat : disque rigide
#    - antagoniste : ligne antagoniste
#    - loi de contact : gapsc0
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
