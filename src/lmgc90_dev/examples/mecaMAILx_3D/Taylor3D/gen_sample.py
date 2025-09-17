import os, sys

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')


# definition des conteneurs:
#   * de corps
bodies = pre.avatars()
#   * de modeles
mods = pre.models()
#   * de materiaux
mats = pre.materials()

tacts = pre.tact_behavs()
svs   = pre.see_tables()

# on se place en 3D
dim = 3

# on definit le materiau constitutif de la barre
steel = pre.material(name='steel', materialType='ELAS_PLAS', density=8.93e3, elas='standard',
                     anisotropy='isotropic', young=1.17e11, nu=0.35, critere='Von-Mises', 
                     isoh='linear', iso_hard=4.e8, isoh_coeff=1e8, cinh='none', visc='none')  
# on l'ajoute dans le contenaur
mats.addMaterial(steel)

# definition d'un modele elasto-plastique pour la barre 
m3Dnl = pre.model(name='M2DNL', physics='MECAx', element='H20Rx', dimension=dim, 
                  external_model='MatL_', kinematic='large', formulation='TotaL',
                  material='J2iso', anisotropy='iso__', mass_storage='lump_')
#     material='J2iso', anisotropy='iso__', mass_storage='coher')
# on ajoute le modele dans le conteneur
mods.addModel(m3Dnl)

# on lit le maillage de la barre dans le fichier
rod_mesh = pre.readMesh('gmsh/taylor_3D.msh', dim)
# on renumerote les noeuds en utilisant leur rang
rod_mesh.rankRenumbering()

# definition de la barre:
# il s'agit d'un corps maille
rod = pre.avatar(dimension=dim)
# on ajoute les noeuds a la barre
rod.addBulks(rod_mesh.bulks)
# on ajoute les elements a la barre
rod.addNodes(rod_mesh.nodes)
# on definit les groupes pour la barre
rod.defineGroups()
# on affecte son modele a la barre
rod.defineModel(model=m3Dnl)
# on affecte son materiau a la barre
rod.defineMaterial(material=steel)
# ajout de la barre dans le conteneur de corps
bodies.addAvatar(rod)

# application de la condition initiale :
# la barre chute avec une vitesse initale donnee
rod.imposeInitValue(group='all', component=3, value=-227.)
# les noeuds en contact avec la fondation ont une vitesse initiale nulle
rod.imposeInitValue(group='102', component=3, value=0.)

# application des conditions aux limites :
#   * condition de symetries :
#      - en y=0, v_y=0
rod.imposeDrivenDof(group='100', component=2, dofty='vlocy')
#      - en x=0, v_x=0
rod.imposeDrivenDof(group='101', component=1, dofty='vlocy')
#   * non penetration de la fondation rigide : en z=0, v_z=0
rod.imposeDrivenDof(group='102', component=3, dofty='vlocy')

post = pre.postpro_commands()

# Ecriture des fichiers pour LMGC
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post, gravy=[0., 0., 0.])

try:
  visuAvatars(bodies)
except:
  pass
