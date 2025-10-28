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
dim = --
# definition d'un modele diffusif couple en terme source
Porous_model = model(name='-----', physics='-----', element='-----',\
                     dimension = ---, \
                     external_model='-----', kinematic='-----',\ 
                     material='-----',anisotropy='-----',\
                     mass_storage='-----',\
                     physical_type = '-----',\
                     capacity_storage='-----',\
                     convection_type = '-----')

# on ajoute le modele dans le conteneur
mods.addModel(Porous_model)

# on definit le materiau constitutif du probleme
Biot = material(name='-----', materialType='PORO_ELAS', density=---, \
                specific_capacity=---, conductivity=---, \
                elas = 'standard', young = ---, nu = ---, \
                anisotropy = 'isotropic',hydro_cpl = ---)  

# on l'ajoute dans le conteneur
mats.addMaterial(Biot)


# definition maillage en Qua8 :
# on lit le mailage pour recuperer la liste des noeuds et des elements
mail1 = readMesh('Mesh.msh', dim)

# Gestion du corps maille
# il s'agit d'un corps maille
TP1 = avatar(number=---, dimension=----)
# on ajoute les noeuds au corps maille
TP1.addBulks(-----.bulks)
# on ajoute les elements au corps maille
TP1.addNodes(-----.nodes)
# on definit les groupes physiques pour le corps maille
TP1.defineGroups()
# on affecte son modele au corps maille sur un groupe physique
TP1.defineModel(model=------, group = '--')
# on affecte son materiau au corps maille sur un groupe physique
TP1.defineMaterial(material=------, group = '--')

# ajout du cube dans le conteneur de corps
bodies += TP1


# application des conditions aux limites sur les groupes physiques du maillage GMSH:
# [CT......+......AMP..*..cos.(..OMEGA.*.time.+.PHI..)] *  [RAMPI.....+.....RAMP.*.time]
TP1.imposeDrivenDof(group='---', component=-- , dofty='-----')
TP1.imposeDrivenDof(group='---', component=-- , dofty='-----')
TP1.imposeDrivenDof(group='---', component=-- , dofty='-----')
TP1.imposeDrivenDof(group='---', component=-- , dofty='-----',\
                    description = 'evolution', evolutionFile = 'Vimp_t.txt')
TP1.imposeDrivenDof(group='---', component=-- , dofty='-----',\
                    ct = 0.0, rampi = 0.0, amp = 1.0, omega = 0.0,\
                    phi = 0.0, ramp = 0.0)

# application des conditions initiales :
TP1.imposeInitValue(group='---',component=-- ,value=0.0)
TP1.imposeInitValue(group='---',component=-- ,value=0.0)
TP1.imposeInitValue(group='---',component=-- ,value=0.0)

# Ecriture des fichiers pour LMGC
writeBodies(bodies, chemin='./DATBOX/')
writeDofIni(bodies, chemin='./DATBOX/')
writeDrvDof(bodies, chemin='./DATBOX/')
writeModels(mods,chemin='./DATBOX/')
writeGPVIni(bodies, chemin='./DATBOX/')
writeBulkBehav(mats, chemin='./DATBOX/', dim=dim, gravy=[0., 0., 0.])
writeTactBehav(tacts,svs,chemin='DATBOX/')
writeVlocRlocIni(chemin='DATBOX/')
