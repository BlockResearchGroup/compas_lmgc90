###################################################################
# Importation du module pre
from pylmgc90.pre import *

# definition des conteneurs:
#   * de corps
bodies = avatars()
#   * de modeles
mods = models()
#   * de materiaux
mats = materials()
#   * pour les tables de visibilite
svs = see_tables()
#   * pour les lois de contact
tacts = tact_behavs()

# on se place en 2D
dim = 2
# definition d'un modele diffusif couple en terme source
Porous_model = model(name='toto_', physics='POROx', element='Q84xx',\
                     dimension = dim, \
                     external_model='no___', kinematic='small',\
                     material='elas_',anisotropy='iso__',\
                     mass_storage='coher',\
                     physical_type = 'solid',\
                     capacity_storage='lump_',\
                     convection_type = 'supg_')

# on ajoute le modele dans le conteneur
mods.addModel(Porous_model)

# on definit le materiau constitutif du probleme
Biot = material(name='dede_', materialType='PORO_ELAS', density=0.0, \
                specific_capacity=0.0, conductivity=0.0075, \
                elas = 'standard', young = 0.4667, nu = 0.1667, \
                anisotropy = 'isotropic',hydro_cpl = 1.0)  

# on l'ajoute dans le conteneur
mats.addMaterial(Biot)


# definition maillage en Qua8 :
# on lit le mailage pour recuperer la liste des noeuds et des elements
mail1 = readMesh('Mesh.msh', dim)

# Gestion du corps maille
# il s'agit d'un corps maille
TP1 = avatar(number=1, dimension=dim)
# on ajoute les noeuds au corps maille
TP1.addBulks(mail1.bulks)
# on ajoute les elements au corps maille
TP1.addNodes(mail1.nodes)
# on definit les groupes physiques pour le corps maille
TP1.defineGroups()
# on affecte son modele au corps maille sur un groupe physique
TP1.defineModel(model=Porous_model, group = '11')
# on affecte son materiau au corps maille sur un groupe physique
TP1.defineMaterial(material=Biot, group = '11')

# ajout du cube dans le conteneur de corps
bodies += TP1


# application des conditions aux limites sur les groupes physiques du maillage GMSH:
# [CT......+......AMP..*..cos.(..OMEGA.*.time.+.PHI..)] *  [RAMPI.....+.....RAMP.*.time]
TP1.imposeDrivenDof(group='7', component=2 , dofty='vlocy')
TP1.imposeDrivenDof(group='10', component=1 , dofty='vlocy')
TP1.imposeDrivenDof(group='8', component=3 , dofty='vlocy')
TP1.imposeDrivenDof(group='9', component=2 , dofty='vlocy',\
                    type = 'evolution', evolutionFile = 'Vimp_t.txt')
TP1.imposeDrivenDof(group='8', component=1 , dofty='force',\
                    ct = 0.0, rampi = 0.0, amp = 0.0, omega = 0.0,\
                    phi = 0.0, ramp = 0.0)

# application des conditions initiales :
TP1.imposeInitValue(group='11',component=1 ,value=0.0)
TP1.imposeInitValue(group='11',component=1 ,value=0.0)
TP1.imposeInitValue(group='11',component=1 ,value=0.0)

# Ecriture des fichiers pour LMGC
writeBodies(bodies, chemin='./DATBOX/')
writeDofIni(bodies, chemin='./DATBOX/')
writeDrvDof(bodies, chemin='./DATBOX/')
writeModels(mods,chemin='./DATBOX/')
writeGPVIni(bodies, chemin='./DATBOX/')
writeBulkBehav(mats, chemin='./DATBOX/', dim=dim, gravy=[0., 0., 0.])
writeTactBehav(tacts,svs,chemin='DATBOX/')
writeVlocRlocIni(chemin='DATBOX/')
