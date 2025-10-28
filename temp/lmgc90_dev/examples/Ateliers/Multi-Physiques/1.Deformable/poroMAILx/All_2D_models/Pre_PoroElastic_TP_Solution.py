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

# definition d'un modele poreux en HPP sur maille Quad8 2D
Porous_HPP_Q8 = model(name='hpp_8', physics='POROx', element='Q84xx',\
                     dimension = dim, \
                     external_model='no___', kinematic='small',\
                     material='elas_',anisotropy='iso__',\
                     mass_storage='coher',\
                     physical_type = 'solid',\
                     capacity_storage='lump_',\
                     convection_type = 'supg_')

# on ajoute le modele dans le conteneur
mods.addModel(Porous_HPP_Q8)

# definition d'un modele poreux en HPP sur maille Tri6 2D
Porous_HPP_T6 = model(name='hpp_6', physics='POROx', element='T63xx',\
                     dimension = dim, \
                     external_model='no___', kinematic='small',\
                     material='elas_',anisotropy='iso__',\
                     mass_storage='coher',\
                     physical_type = 'solid',\
                     capacity_storage='lump_',\
                     convection_type = 'supg_')

# on ajoute le modele dans le conteneur
mods.addModel(Porous_HPP_T6)


# definition d'un modele poreux en GD sur maille Quad8 2D
Porous_GD_Q8 = model(name='gd__8', physics='POROx', element='Q84xx',\
                     dimension = dim, \
                     external_model='MatL_', kinematic='large',formulation = 'TotaL',\
                     material='neoh_',anisotropy='iso__',\
                     mass_storage='coher',\
                     physical_type = 'solid',\
                     capacity_storage='lump_',\
                     convection_type = 'supg_')

# on ajoute le modele dans le conteneur
mods.addModel(Porous_GD_Q8)

# definition d'un modele poreux en GD sur maille Quad8 2D
Porous_GD_T6 = model(name='gd__6', physics='POROx', element='T63xx',\
                     dimension = dim, \
                     external_model='MatL_', kinematic='large',formulation = 'TotaL',\
                     material='neoh_',anisotropy='iso__',\
                     mass_storage='coher',\
                     physical_type = 'solid',\
                     capacity_storage='lump_',\
                     convection_type = 'supg_')

# on ajoute le modele dans le conteneur
mods.addModel(Porous_GD_T6)

# on definit le materiau constitutif du probleme
Biot = material(name='dede_', materialType='PORO_ELAS', density=0.0, \
                specific_capacity=0.0, conductivity=0.0075, \
                elas = 'standard', young = 0.4667, nu = 0.1667, \
                anisotropy = 'isotropic',hydro_cpl = 1.0)  

# on l'ajoute dans le conteneur
mats.addMaterial(Biot)


# definition maillage en Qua8 :
# on lit le mailage pour recuperer la liste des noeuds et des elements
mail1 = readMesh('MeshQ8.msh', dim)

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
TP1.defineModel(model=Porous_HPP_Q8, group = '11')
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

# definition maillage en Qua8 :
# on lit le mailage pour recuperer la liste des noeuds et des elements
mail2 = readMesh('MeshT6.msh', dim)

# Gestion du corps maille
# il s'agit d'un corps maille
TP2 = avatar(number=2, dimension=dim)
# on ajoute les noeuds au corps maille
TP2.addBulks(mail2.bulks)
# on ajoute les elements au corps maille
TP2.addNodes(mail2.nodes)
# on definit les groupes physiques pour le corps maille
TP2.defineGroups()
# on affecte son modele au corps maille sur un groupe physique
TP2.defineModel(model=Porous_HPP_T6, group = '11')
# on affecte son materiau au corps maille sur un groupe physique
TP2.defineMaterial(material=Biot, group = '11')

# ajout du cube dans le conteneur de corps
bodies += TP2


# application des conditions aux limites sur les groupes physiques du maillage GMSH:
# [CT......+......AMP..*..cos.(..OMEGA.*.time.+.PHI..)] *  [RAMPI.....+.....RAMP.*.time]
TP2.imposeDrivenDof(group='7', component=2 , dofty='vlocy')
TP2.imposeDrivenDof(group='10', component=1 , dofty='vlocy')
TP2.imposeDrivenDof(group='8', component=3 , dofty='vlocy')
TP2.imposeDrivenDof(group='9', component=2 , dofty='vlocy',\
                    type = 'evolution', evolutionFile = 'Vimp_t.txt')
TP2.imposeDrivenDof(group='8', component=1 , dofty='force',\
                    ct = 0.0, rampi = 0.0, amp = 0.0, omega = 0.0,\
                    phi = 0.0, ramp = 0.0)

# application des conditions initiales :
TP2.imposeInitValue(group='11',component=1 ,value=0.0)
TP2.imposeInitValue(group='11',component=1 ,value=0.0)
TP2.imposeInitValue(group='11',component=1 ,value=0.0)

# deplacement du corps maille
TP2.translate(dy=3.0)

# definition maillage en Qua8 :
# on lit le mailage pour recuperer la liste des noeuds et des elements
mail3 = readMesh('MeshQ8.msh', dim)

# Gestion du corps maille
# il s'agit d'un corps maille
TP3 = avatar(number=3, dimension=dim)
# on ajoute les noeuds au corps maille
TP3.addBulks(mail3.bulks)
# on ajoute les elements au corps maille
TP3.addNodes(mail3.nodes)
# on definit les groupes physiques pour le corps maille
TP3.defineGroups()
# on affecte son modele au corps maille sur un groupe physique
TP3.defineModel(model=Porous_GD_Q8, group = '11')
# on affecte son materiau au corps maille sur un groupe physique
TP3.defineMaterial(material=Biot, group = '11')

# ajout du cube dans le conteneur de corps
bodies += TP3


# application des conditions aux limites sur les groupes physiques du maillage GMSH:
# [CT......+......AMP..*..cos.(..OMEGA.*.time.+.PHI..)] *  [RAMPI.....+.....RAMP.*.time]
TP3.imposeDrivenDof(group='7', component=2 , dofty='vlocy')
TP3.imposeDrivenDof(group='10', component=1 , dofty='vlocy')
TP3.imposeDrivenDof(group='8', component=3 , dofty='vlocy')
TP3.imposeDrivenDof(group='9', component=2 , dofty='vlocy',\
                    type = 'evolution', evolutionFile = 'Vimp_t.txt')
TP3.imposeDrivenDof(group='8', component=1 , dofty='force',\
                    ct = 0.0, rampi = 0.0, amp = 0.0, omega = 0.0,\
                    phi = 0.0, ramp = 0.0)

# application des conditions initiales :
TP3.imposeInitValue(group='11',component=1 ,value=0.0)
TP3.imposeInitValue(group='11',component=1 ,value=0.0)
TP3.imposeInitValue(group='11',component=1 ,value=0.0)

# deplacement du corps maille
TP3.translate(dy=6.0)

# definition maillage en Qua8 :
# on lit le mailage pour recuperer la liste des noeuds et des elements
mail4 = readMesh('MeshT6.msh', dim)

# Gestion du corps maille
# il s'agit d'un corps maille
TP4 = avatar(number=4, dimension=dim)
# on ajoute les noeuds au corps maille
TP4.addBulks(mail4.bulks)
# on ajoute les elements au corps maille
TP4.addNodes(mail4.nodes)
# on definit les groupes physiques pour le corps maille
TP4.defineGroups()
# on affecte son modele au corps maille sur un groupe physique
TP4.defineModel(model=Porous_GD_T6, group = '11')
# on affecte son materiau au corps maille sur un groupe physique
TP4.defineMaterial(material=Biot, group = '11')

# ajout du cube dans le conteneur de corps
bodies += TP4


# application des conditions aux limites sur les groupes physiques du maillage GMSH:
# [CT......+......AMP..*..cos.(..OMEGA.*.time.+.PHI..)] *  [RAMPI.....+.....RAMP.*.time]
TP4.imposeDrivenDof(group='7', component=2 , dofty='vlocy')
TP4.imposeDrivenDof(group='10', component=1 , dofty='vlocy')
TP4.imposeDrivenDof(group='8', component=3 , dofty='vlocy')
TP4.imposeDrivenDof(group='9', component=2 , dofty='vlocy',\
                    type = 'evolution', evolutionFile = 'Vimp_t.txt')
TP4.imposeDrivenDof(group='8', component=1 , dofty='force',\
                    ct = 0.0, rampi = 0.0, amp = 0.0, omega = 0.0,\
                    phi = 0.0, ramp = 0.0)

# application des conditions initiales :
TP4.imposeInitValue(group='11',component=1 ,value=0.0)
TP4.imposeInitValue(group='11',component=1 ,value=0.0)
TP4.imposeInitValue(group='11',component=1 ,value=0.0)

# deplacement du corps maille
TP4.translate(dy=9.0)

# Ecriture des fichiers pour LMGC
writeBodies(bodies, chemin='./DATBOX/')
writeDofIni(bodies, chemin='./DATBOX/')
writeDrvDof(bodies, chemin='./DATBOX/')
writeModels(mods,chemin='./DATBOX/')
writeGPVIni(bodies, chemin='./DATBOX/')
writeBulkBehav(mats, chemin='./DATBOX/', dim=dim, gravy=[0., 0., 0.])
