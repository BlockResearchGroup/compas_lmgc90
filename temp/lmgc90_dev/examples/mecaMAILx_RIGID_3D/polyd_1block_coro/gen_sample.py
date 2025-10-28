import numpy as np
from pylmgc90 import pre

bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
sees   = pre.see_tables()
tacts  = pre.tact_behavs()

dim = 3


te4ml = pre.model(name='TE4ML', physics='MECAx', element='TE4xx', dimension=dim, 
                  external_model='MatL_', kinematic='small', 
                  material='elas_', anisotropy='iso__', mass_storage='lump_'   )
mods.addModel(te4ml)

stone = pre.material(name='stone', materialType='ELAS',
                     elas='standard',young=1.67e4, nu=0.3, anisotropy='isotropic',
                     density=2.5e3)
mats.addMaterial(stone)

# on lit le maillage total dans un fichier, au format gmsh
complete_mesh = pre.readMesh(name='./colonne.msh', dim=dim)

# on separe les differents maillages volumiques stockes dans le fichier, corespondants aux differents 
# solides : les voussoirs, les fondations
meshes_vous = complete_mesh.separateMeshes(dim=dim,entity_type="geometricalEntity", keep_all_elements=True)

elem = meshes_vous['139']
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
free_surface = pre.extractFreeSurface(elem)
# on stocke les connectivites des faces des triangles de la surface libre dans une liste
l_free_conn=[face.connectivity for face in free_surface.bulks]
# on peut maintenant ajouter le contacteur POLYD au voussoir
body_deformable_vous.addContactors(group='fake', shape='POLYD', color='BLEUx',
                                   nb_vertices=len(free_surface.nodes), nb_faces=len(l_free_conn),
                                   connectivity=np.array(l_free_conn))
body_deformable_vous.rotate(description='axis', center=np.zeros([3]), axis=[1.,-1.,0.], alpha=np.pi/20.)
# ajout du voussoir dans le conteneur de corps
bodies.addAvatar(body_deformable_vous)


# rigid model & material
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod)

tdur = pre.material(name='TDURx', materialType='RIGID', density=2500.)
mats.addMaterial(tdur)

#basex = pre.rigidPolyhedron(axe1=0.6 , axe2=0.6, axe3=0.02, center=[0.5,0.5,-0.02], material=tdur, model=mod, color='BASEx')
vertices = np.zeros( [8,3] )
vertices[  :4,2] = -0.02 
vertices[ ::2,1] = -0.1 
vertices[1::2,1] =  1.1 
vertices[  : ,0] = -0.1 
vertices[[2,3,6,7],0] =  1.1 
basex = pre.rigidPolyhedron(model=mod, material=tdur, color='BASEx',
                            generation_type='vertices', vertices=vertices)
basex.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
bodies.addAvatar(basex)

# definitions des interactions
#  * loi d'interaction :
lprpr = pre.tact_behav('iqsG0', 'IQS_CLB_g0', fric=0.9)
tacts+= lprpr

#  * table de visibilite :
svprpr = pre.see_table(CorpsCandidat   ='MAILx', candidat   ='POLYR', colorCandidat   ='BLEUx', behav=lprpr,
                       CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BASEx', alert=2.5e-1)
sees  += svprpr

# ecriture des fichiers de donnees
pre.writeDatbox(dim, mats, mods, bodies, tacts, sees)
pre.visuAvatars(bodies, True)
