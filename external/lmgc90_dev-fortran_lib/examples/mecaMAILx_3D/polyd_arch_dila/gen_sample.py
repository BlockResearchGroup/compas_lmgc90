import numpy as np
from pylmgc90 import pre

bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
sees   = pre.see_tables()
tacts  = pre.tact_behavs()

dim = 3

# elastic model & material

te4ml = pre.model(name='TE4ML', physics='MECAx', element='TE4xx', dimension=dim, 
                  external_model='MatL_', kinematic='small', 
                  material='elasd', anisotropy='iso__', mass_storage='lump_', 
                  external_fields=['TEMPERATURE'])
mods.addModel(te4ml)

stone = pre.material(name='stone', materialType='ELAS_DILA',
                     elas='standard',young=1.67e10, nu=0.3, anisotropy='isotropic',
                     dilatation=1.e-4,T_ref_meca=20.,density=2.5e3)
mats.addMaterial(stone)



# on lit le maillage total dans un fichier, au format gmsh
complete_mesh = pre.readMesh(name='./Arche_JP.msh', dim=dim)

# split of voussoirs and foundations
meshes_vous = complete_mesh.separateMeshes(dim=dim,entity_type="geometricalEntity", keep_all_elements=True)

for elem in meshes_vous.values():
    #
    #reorientSurfacicElements(volumic_mesh=elem)
    
    # on renumerote les noeuds du maillage en utilisant leur rang
    elem.rankRenumbering()

    # on construit un avatar deformable pour le voussoir
    # il s'agit d'un corps maille
    body_deformable_vous = pre.avatar(dimension=dim)

    # on ajoute les noeuds au voussoir
    body_deformable_vous.addNodes(elem.nodes)
    # on ajoute les elements au voussoir
    body_deformable_vous.addBulks(elem.bulks)

    # on ajoute un element point bidon, pour accrocher le contacteur POLYD, par la suite
    body_deformable_vous.addBulk( pre.element(elem_dim=0, nbNodes=1, connectivity=[1], physicalEntity='fake' ) )
    
    # on definit les groupes pour le voussoir
    body_deformable_vous.defineGroups()
    # on affecte son modele au voussoir
    body_deformable_vous.defineModel(model=te4ml)
    # on affecte son materiau au voussoir
    body_deformable_vous.defineMaterial(material=stone)

    # on extrait la surface libre du maillage
    free_surface=pre.extractFreeSurface(elem)
    # on stocke les connectivites des faces des triangles de la surface libre dans une liste
    l_free_conn=[face.connectivity for face in free_surface.bulks]
    # on peut maintenant ajouter le contacteur POLYD au voussoir
    body_deformable_vous.addContactors(group='fake', shape='POLYD', color='BLEUx',
                                       nb_vertices=len(free_surface.nodes), nb_faces=len(l_free_conn),
                                       connectivity=np.array(l_free_conn))

    # ajout du voussoir dans le conteneur de corps
    bodies.addAvatar(body_deformable_vous)

# definitions des interactions
#  * loi d'interaction :
lprpr = pre.tact_behav('iqsG0', 'IQS_CLB_g0', fric=0.9)
tacts+= lprpr

#  * table de visibilite :
svprpr = pre.see_table(CorpsCandidat   ='MAILx', candidat   ='POLYR', colorCandidat   ='BLEUx', behav=lprpr,
                       CorpsAntagoniste='MAILx', antagoniste='POLYR', colorAntagoniste='BLEUx', alert=2.5e-1)
sees  += svprpr

# ecriture des fichiers de donnees
pre.writeDatbox(dim, mats, mods, bodies, tacts, sees)
pre.visuAvatars(bodies, True)
