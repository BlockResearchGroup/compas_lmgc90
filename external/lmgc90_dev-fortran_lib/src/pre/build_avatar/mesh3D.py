# module qui fournit des macros pour construire et/ou manipuler des maillages 3D

import numpy
from copy import deepcopy
from ..avatar.avatar import *
from ..avatar.bulk.element import *
from ..avatar.bulk.rigid3d import *
from ..avatar.contactor.rigid_properties_3D import computeVolumeInertiaMesh

from .mesh import *

from ..avatars  import *
from .particles import rigidPolyhedron

from ..utilities.error    import *

# import du module permettant de savoir si on pourra importer les pre_tools
from ..utilities.check_compiled_modules import *


# si on peut essayer d'importer le module pre_tools sans tout faire planter
if import_lmgc90():
   # on essaye
   try:
      from ...chipy import lmgc90
   except ImportError:
      print('Unable to import wrapped part surafcic mesh handling module!')
      print('You will not be able to build polyhedron from surfacic meshes')
   except:
      raise

# connectivite des faces d'un tetraedre (lineaire)
faces_tetra=[ [2, 1, 0],
              [0, 1, 3],
              [1, 2, 3],
              [2, 0, 3] ]

# connectivite des faces d'un hexaedre (lineaire)
faces_hexa=[ [0, 3, 2, 1],
             [4, 5, 6, 7],
             [0, 1, 5, 4],
             [2, 3, 7, 6],
             [0, 4, 7, 3],
             [1, 2, 6, 5] ]

# connectivite des faces d'un prisme (lineaire)
faces_pri=[ [3, 4, 5],
            [0, 1, 4, 3],
            [1, 2, 5, 4],
            [0, 2, 1],
            [0, 3, 5, 2] ]

# fonction qui extrait la surface libre d'un maillage 3D volumique, sous la forme d'un maillage 3D surfacique
# N.B.: les numeros des noeuds du maillage surfacique ne sont pas renumerotes
def extractFreeSurface(volumic_mesh):
   """extractFreeSurface(volumic_mesh):

   this function computes and returns the free surface of a volumic mesh, as a
   surfacic mesh.

   N.B.: this function handles tetrahedra and prism

   parameters:

   - volumic_mesh: the given volumic mesh

   returned value: the built surfacic mesh
   """
   # si le maillage volumique donne n'est pas un maillage
   if not isinstance(volumic_mesh, mesh):
      # on affiche un message d'erreur
      showError("the given volumic mesh is not a mesh")

   # si le maillage n'est pas defini avec de noeuds a trois dimensions
   if volumic_mesh.dimension != 3:
      # ca ne peut pas etre un maillage volumique!
      # on affiche un message d'erreur 
      showError("the given mesh is 2D!")

   # numerotation des elements

   # on enumere les elements
   for num, bulk in enumerate(volumic_mesh.bulks):
      # on affecte l'indice courant a l'element courant
      bulk.number=num

   # construction des listes d'adjacence de chaque noeud

   # on definit un container pour stocker les listes d'adjacence de chaque 
   # noeud 
   l_node2ele = {}
   # pour chaque noeud du corps, on intialise une liste d'adjacence vide
   for nod in volumic_mesh.nodes:
      l_node2ele[nod.number]=[]
   # pour chaque element du maillage 
   for bulk in volumic_mesh.bulks:
      # pour chaque noeud de l'element
      for n in bulk.connectivity:
         # on ajoute l'element courant dans la liste d'adjacence du noeud 
         # courant
         l_node2ele[n].append(bulk)
 
   # extraction de la surface libre
   
   # on definit la liste des nombres de faces n'appartenant pas a la surface libre, pour chaque element
   nb_non_free_faces=numpy.zeros(len(volumic_mesh.bulks), 'i')
   # on definit le dictionnaire qui indique pour chaque element, identifie par son numero, quelle face est libre
   is_free_face={}
   # pour chaque element
   for bulk in volumic_mesh.bulks:
      # si l'element est un element volumique
      if bulk.etype in dimension2geoElement[3]:
         # si l'element est un tetraedre
         if bulk.etype == 'TE4xx':
            # on suppose que les quatre faces du tetraedre sont sur la surface libre
            is_free_face[bulk.number]=[True, True, True, True]
            # on recupere les numeros des noeuds du tetraedre
            i1=bulk.connectivity[0]
            i2=bulk.connectivity[1]
            i3=bulk.connectivity[2]
            i4=bulk.connectivity[3]

            # on teste d'abord les faces qui portent le noeud i1 de l'element courant
            
            # pour chaque element adjacent au noeud i1
            for adj_bulk in l_node2ele[i1]:
               # si l'element adjacent courant est l'element courant
               if adj_bulk.number == bulk.number:
                  # on passe au suivant
                  continue
               # si l'element courant n'est pas un tetraedre
               if adj_bulk.etype != 'TE4xx':
                  # on passe au suivant
                  continue

               # si la face {i1, i2, i3} fait partie du prisme adjacent courant
               if i2 in adj_bulk.connectivity and i3 in adj_bulk.connectivity:
                  # on indique que la premiere face du tetraedre courant n'appartient pas a la surface libre
                  is_free_face[bulk.number][0]=False
                  # on incremente le nombre de faces du tetraedre courant qui n'appartiennent pas a la surface libre
                  nb_non_free_faces[bulk.number] += 1
               # si la face {i1, i2, i4} fait partie du tetraedre adjacent courant
               if i2 in adj_bulk.connectivity and i4 in adj_bulk.connectivity:
                  # on indique que la deuxieme face du tetraedre courant n'appartient pas a la surface libre
                  is_free_face[bulk.number][1]=False
                  # on incremente le nombre de faces du tetraedre courant qui n'appartiennent pas a la surface libre
                  nb_non_free_faces[bulk.number] += 1
               # si la face {i1, i3, i4} fait partie du tetraedre adjacent courant
               if i3 in adj_bulk.connectivity and i4 in adj_bulk.connectivity:
                  # on indique que la quatrieme face du tetraedre courant n'appartient pas a la surface libre
                  is_free_face[bulk.number][3]=False
                  # on incremente le nombre de faces du tetraedre courant qui n'appartiennent pas a la surface libre
                  nb_non_free_faces[bulk.number] += 1

            # on teste ensuite la derniere face de l'element courant
            
            # pour chaque element adjacent au noeud i4
            for adj_bulk in l_node2ele[i4]:
               # si l'element adjacent courant est l'element courant
               if adj_bulk.number == bulk.number:
                  # on passe au suivant
                  continue
               # si l'element courant n'est pas un tetraedre
               if adj_bulk.etype != 'TE4xx':
                  # on passe au suivant
                  continue
               # si la face {i4, i2, i3} fait partie du tetraedre adjacent courant
               if i2 in adj_bulk.connectivity and i3 in adj_bulk.connectivity:
                  # on indique que la troisieme face du tetraedre courant n'appartient pas a la surface libre
                  is_free_face[bulk.number][2]=False
                  # on incremente le nombre de faces du tetraedre courant qui n'appartiennent pas a la surface libre
                  nb_non_free_faces[bulk.number] += 1

         elif bulk.etype == 'PRI6x':

            # on suppose que les faces sont sur la surface libre (ce commentaire est reellement debile)
            # par contre on ne test qu'avec d'autres pri6 !!
            is_free_face[bulk.number]=[True]*5
            # on recupere les numeros des noeuds
            i1=bulk.connectivity[0]
            i2=bulk.connectivity[1]
            i3=bulk.connectivity[2]
            i4=bulk.connectivity[3]
            i5=bulk.connectivity[4]
            i6=bulk.connectivity[5]
            # on teste d'abord les faces qui portent le noeud i1 de l'element courant
            
            #faces_pri=[ [4, 5, 6],
            #          [1, 2, 5, 4],
            #          [2, 3, 6, 5],
            #          [1, 3, 2],
            #          [1, 4, 6, 3] ]

            # pour chaque element adjacent au noeud i1
            for adj_bulk in l_node2ele[i1]:
               # si l'element adjacent courant est l'element courant
               if adj_bulk.number == bulk.number:
                  # on passe au suivant
                  continue
                              
               # on ne test qu'avec des PRI6 ... a modifier.
               if adj_bulk.etype != 'PRI6x':
                  continue
               # si la face {i1, i2, i5, i4} fait partie de l'adjacent
               if i2 in adj_bulk.connectivity and i5 in adj_bulk.connectivity and i4 in adj_bulk.connectivity:
                  # on la vire
                  is_free_face[bulk.number][1]=False
                  # on incremente le nombre de faces qui n'appartiennent pas a la surface libre
                  nb_non_free_faces[bulk.number] += 1
               # si la face {i1, i3, i2} fait partie de l'adjacent
               if i2 in adj_bulk.connectivity and i3 in adj_bulk.connectivity:
                  # on la vire
                  is_free_face[bulk.number][3]=False
                  # on incremente le nombre de faces qui n'appartiennent pas a la surface libre
                  nb_non_free_faces[bulk.number] += 1
               # si la face {i1, i4, i6, i3} fait partie de l'adjacent
               if i4 in adj_bulk.connectivity and i6 in adj_bulk.connectivity and i3 in adj_bulk.connectivity:
                  # on la vire
                  is_free_face[bulk.number][4]=False
                  # on incremente le nombre de faces qui n'appartiennent pas a la surface libre
                  nb_non_free_faces[bulk.number] += 1

            # pour chaque element adjacent au noeud i6
            for adj_bulk in l_node2ele[i6]:
               # si l'element adjacent courant est l'element courant
               if adj_bulk.number == bulk.number:
                  # on passe au suivant
                  continue
               # si l'element courant n'est pas un tetraedre
               if adj_bulk.etype != 'PRI6x':
                  # on passe au suivant
                  continue
               # si la face {i4, i5, i6} fait partie de l'adjacent
               if i4 in adj_bulk.connectivity and i5 in adj_bulk.connectivity:
                  # on la vire
                  is_free_face[bulk.number][0]=False
                  # on incremente le nombre de faces qui n'appartiennent pas a la surface libre
                  nb_non_free_faces[bulk.number] += 1
               # si la face {i2, i3, i6, i5} fait partie de l'adjacent courant
               if i2 in adj_bulk.connectivity and i3 in adj_bulk.connectivity and i5 in adj_bulk.connectivity:
                  # on la vire
                  is_free_face[bulk.number][2]=False
                  # on incremente le nombre de faces qui n'appartiennent pas a la surface libre
                  nb_non_free_faces[bulk.number] += 1

         # si l'element n'est pas gere
         else:
            ## on quitte le pgm
            msg  = "no rigid can be built from this mesh, since it involves unhandled elements!"
            msg += bulk.etype + ' is not supported'
            showError(msg)

   # ici, on est sur que tous les elements volumiques du maillage sont pris en charge

   #
   # recuperation d'une triangulation de la surface libre
   #

   # on declare un maillage pour stocker la triangulation de la surface libre
   # N.B.: le maillage est defini avec des neouds a trois dimensions, meme s'il ne contient que des elements surfaciques
   free_surface=mesh(dimension=3)

   # recuperation des elements de la surface libre

   # pour chaque element
   for bulk in volumic_mesh.bulks:
      # si l'element est un element volumique
      if bulk.etype in dimension2geoElement[3]:
         # si l'element est un tetraedre
         if bulk.etype == 'TE4xx':
            # si aucune face du tetraedre appartient a la surface libre
            if nb_non_free_faces[bulk.number] == 4:
               # on passe a l'element suivant
               continue
            # pour chaque face du tetraedre
            for i in range(0, 4):
               # si la face courante est libre
               if is_free_face[bulk.number][i]:
                  # on recupere la connectivite de la face
                  conn_free_face=[ bulk.connectivity[faces_tetra[i][0]], 
                                   bulk.connectivity[faces_tetra[i][1]],
                                   bulk.connectivity[faces_tetra[i][2]] ]
                  # on ajoute la face a la liste des faces de la surface libre
                  free_surface.addBulk( element(elem_dim=2, connectivity=conn_free_face) )
         elif bulk.etype == 'PRI6x':
            # si aucune face du tetraedre appartient a la surface libre
            if nb_non_free_faces[bulk.number] == 6:
               # on passe a l'element suivant
               continue
            # pour chaque face du tetraedre
            for i in range(0, 5):
               # si la face courante est libre
               if is_free_face[bulk.number][i]:
                  if (i == 0 or i == 3): 
                    # on recupere la connectivite de la face
                    conn_free_face=[ bulk.connectivity[faces_pri[i][0]], 
                                     bulk.connectivity[faces_pri[i][1]],
                                     bulk.connectivity[faces_pri[i][2]] ]
                    # on ajoute la face a la liste des faces de la surface libre
                    free_surface.addBulk( element(elem_dim=2, connectivity=conn_free_face) )
                  else:

                    #print i
                    #print faces_pri[i][:]
                    #print bulk.connectivity[faces_pri[i][0]],bulk.connectivity[faces_pri[i][1]],bulk.connectivity[faces_pri[i][2]],bulk.connectivity[faces_pri[i][3]]
                    # on recupere la connectivite de la face
                    conn_free_face=[ bulk.connectivity[faces_pri[i][0]], 
                                     bulk.connectivity[faces_pri[i][1]],
                                     bulk.connectivity[faces_pri[i][2]],
                                     bulk.connectivity[faces_pri[i][3]] ]
                    # on ajoute la face a la liste des faces de la surface libre
                    free_surface.addBulk( element(elem_dim=2, connectivity=conn_free_face) )
                      
   # recuperation des noeuds de la surface libre

   # on declare la liste des numeros de noeuds de la surface libre
   free_node_numbers=[]
   # pour chaque face de la surface libre
   for face in free_surface.bulks:
      # pour chaque noeud de la table de connectivite de la face
      for number in face.connectivity:
         # si le noeud n'a pas deja ete visite
         if not number in free_node_numbers:
            # on ajoute le numero du noeud courant a la liste des numeors de noeuds de la surface libre
            free_node_numbers.append(number)
            # on ajoute une copie du noeud au maillage de la surface libre
            free_surface.addNode(deepcopy(volumic_mesh.nodes[number]))

   # on renvoie le maillage de la surface libre
   return free_surface

# fonction qui reoriente les elements surfacique d'un maillage 3D, en se servant de l'orientation des elements volumiques
def reorientSurfacicElements(volumic_mesh): 
   """reorientSurfacicElements(volumic_mesh):

   this function reorient surfacic elements of a 3D mesh, using orientation of volumic elements

   N.B.: this function only handle tetrahedra

   parameters:

   - volumic_mesh: the given volumic mesh
   """
   # si le maillage volumique donne n'est pas un maillage
   if not isinstance(volumic_mesh, mesh):
      # on affiche un message d'erreur
      showError("the given volumic mesh is not a mesh")

   # si le maillage n'est pas defini avec de noeuds a trois dimensions
   if volumic_mesh.dimension != 3:
      # ca ne peut pas etre un maillage volumique!
      # on affiche un message d'erreur 
      showError("the given mesh is 2D!")

   # extraction de la surface libre
   free_surface=extractFreeSurface(volumic_mesh)

   # construction des listes d'adjacence de chaque noeud, dans le maillage de la surface libre

   # on definit un container pour stocker les listes d'adjacence de chaque 
   # noeud 
   l_node2ele = {}
   # pour chaque noeud du corps, on intialise une liste d'adjacence vide
   for nod in free_surface.nodes:
      l_node2ele[nod.number]=[]
   # pour chaque element de la surface libre
   for bulk in free_surface.bulks:
      # pour chaque noeud de l'element
      for n in bulk.connectivity:
         # on ajoute l'element courant dans la liste d'adjacence du noeud 
         # courant
         l_node2ele[n].append(bulk)

   # reorientation des elements surfaciques
   
   # pour chaque element du maillage
   for bulk in volumic_mesh.bulks:
      # si l'element est un element surfacique
      if bulk.etype in dimension2geoElement[2]:
         # on recupere la connectivite de l'element courant
         connectivity=bulk.connectivity
  
         #print "original connectivity: ", bulk.connectivity

         # pour chaque element adjacent au premier noeud de l'element courant (dans la surface libre)
         for free_bulk in l_node2ele[connectivity[0]]:
            # on suppose que l'element de la surface libre courant corespond a l'element surfacique courant
            is_found=True
            # pour chaque noeud de l'element de la surface libre courant
            for num in free_bulk.connectivity:
               # si le numero de noeud courant n'est pas support de l'element surfacique courant
               if not num in connectivity:
                  # l'element de la surface libre courant ne peut pas corespondre a l'element surfacique courant
                  is_found=False
                  # on sort de la boucle
                  break
            # si l'element de la surface libre courant est associe a l'element surfacique courant
            if is_found:
               # la connectivite de l'element de la surface libre remplace la connectivite de l'element surfacique courant
               # (et l'orientation de l'element surfacique courant suit l'orientation de l'element de la surface libre)
               bulk.connectivity=free_bulk.connectivity
               # on passe au prochain element surfacique
               break
         # si on n'a pas trouve l'element de la surface libre corespondant a l'element surfacique courant
         if not is_found:
            # il y a un (gros) probleme de coherence entre les elements surfaciques et la surface libre
            showError("inconsistency between volumic and the free surface!")
         
         #print "new connectivity: ", bulk.connectivity

# fonction qui construit un corps rigide a partir d'un maillage 3D, decrit comme un ensemble de noeuds et 
# un ensemble d'elements
def volumicMeshToRigid3D(volumic_mesh, model, material, color='BLUEx'):
   """volumicMeshToRigid3D(volumic_mesh, model, material, color='BLUEx'):

   this function builds a rigid body from a volumic mesh, by extracting the skin mesh and compute mass and inertia
   from volumic elements.

   N.B.: this function only handle tetrahedra

   parameters:

   - volumic_mesh: the given volumic mesh
   - model: a given model
   - material: a given material

   optional parameters:

   - color='BLUEx': color of the polyhedron contactor

   returned value: the built rigid body
   """
   # si le maillage volumique donne n'est pas un maillage
   if not isinstance(volumic_mesh, mesh):
      # on affiche un message d'erreur
      showError("the given volumic mesh is not a mesh")

   # si le maillage n'est pas defini avec de noeuds a trois dimensions
   if volumic_mesh.dimension != 3:
      # ca ne peut pas etre un maillage volumique!
      # on affiche un message d'erreur 
      showError("the given mesh is 2D!")

   # on verifie que le maillage ne contient bien des elements volumiques

   # on initialise le nombre d'elements volumiques a 0
   nb_volumic_bulks=0
   # pour chaque element
   for ele in volumic_mesh.bulks:
      # si l'element est un element volumique
      if ele.etype in dimension2geoElement[3]:
         # on incremente le nombre d'elements volumiques
         nb_volumic_bulks += 1

   # si le maillage ne contient aucun element volumique
   if nb_volumic_bulks == 0:
      # on affiche un message d'erreur
      showError("the given mesh contains no volumic element!")
 
   #
   # creation de l'avatar rigide
   #

   # on declare le nouvel avatar
   body=avatar(dimension=3)
   # on positionne le centre d'inertie du rigide a l'origine, pour pouvoir
   # donner les coordonnees des sommets du polyedre dans le repere global
   body.addNode( node( coor=numpy.zeros(3), number=1) )
   # on cree un comportement volumique pour le corps
   body.addBulk( rigid3d() )
   # on definit les groupes pour l'avatar
   body.defineGroups()
   # on affecte son modele a l'avatar
   body.defineModel(model=model)      
   # on affecte son materiau a l'avatar
   body.defineMaterial(material=material)

   #
   # construction du contacteur polyedre
   #

   # extraction de la surface libre
   free_surface=extractFreeSurface(volumic_mesh)

   # on renumerote des noeuds de la surface libre
   free_surface.rankRenumbering()

   # on stocke les coordonnees des noeuds de la surface libre dans une liste

   # on recupere la liste triee des numeros de noeuds de la surface libre
   sorted_node_numbers=free_surface.nodes.sortedKeys()
   l_free_coor=[]
   for num in sorted_node_numbers:
      l_free_coor.append(free_surface.nodes[num].coor)

   # on stocke les connectivites des faces des triangles de la surface libre dans une liste
   l_free_conn=[face.connectivity for face in free_surface.bulks] 

   # calcul du volume, de l'inertie et de la position du centre d'inertie du contacteur polyedre
   volume, I, OG=computeVolumeInertiaMesh(volumic_mesh)

   # on ajoute son contacteur polyedre a l'avatar
   body.addContactors(shape='POLYR', color=color, volume=volume, I=I, shift=OG,
      nb_vertices=len(l_free_coor), nb_faces=len(l_free_conn), 
      vertices=numpy.array(l_free_coor), connectivity=numpy.array(l_free_conn))

   # calcul du volume et de l'inertie du corps (a partir du volume et de l'inertie 
   # du polyedre)
   body.computeRigidProperties()
 
   # on renvoie le corps genere
   return body

# fonction qui affecte un numero d'entite aux elements d'un maillage de surface, en fonction de la composante connexe a laquelle ils appartiennent
def identifyEntitiesInSurfacicMesh(surfacic_mesh):
   """identifyEntitiesInSurfacicMesh(surfacic_mesh):

   this  function attributes an entity value to triangle elements, by computing 
   connected components. This new entity value replaces the geometricalEntity of
   triangle elements.

   N.B.: this function only handle triangles

   parameters:

   - surfacic_mesh: the given surfacic mesh

   returned value: None
   """

   # si le maillage surfacique donne n'est pas un maillage
   if not isinstance(surfacic_mesh, mesh):
      # on affiche un message d'erreur
      showError("the given surfacic mesh is not a mesh")

   # si le maillage n'est pas defini avec des noeuds a trois dimensions
   if surfacic_mesh.dimension != 3:
      # ca ne peut pas servir a la construction d'un avatar 3D!
      # on affiche un message d'erreur 
      showError("the given mesh is 2D!")

   # on verifie que le maillage ne contient que des triangles a trois noeuds
   # et on contruit la liste des numeros des elements T3xxx
 
   # on initialise la liste des numeros des elements T3xxx a vide
   ind_T3=[]
   # on initialise le nombre d'elements T3xxx a 0
   nb_bulks_T3=0
   # pour chaque element
   for i, ele in enumerate(surfacic_mesh.bulks):
      # si l'element est un element volumique
      if ele.etype in dimension2geoElement[3]:
         # on affiche un message d'erreur
         showError("the given mesh is not a surfacic mesh")
      # sinon, si l'element est un point ou une ligne
      if not ele.etype in dimension2geoElement[2]:
         # on passe au suivant
         continue
      # si l'element courant est un element surfacique, mais n'est pas un triangle a trois noeuds
      if ele.etype != 'T3xxx':
         # on affiche un message d'erreur
         showError("the given surfacic mesh contains unhandled elements!")
      # sinon, 
      else:
         # on ajoute le numero de l'element courant a la liste
         ind_T3.append(i)
         # on incremente le nombre d'elements triangle
         nb_bulks_T3 += 1

   # si le maillage ne contient aucun triangle
   if nb_bulks_T3 == 0:
      # on affiche un message d'erreur
      showError("the given surfacic mesh contains no T3!")
  
   # ici on est sur que le maillage ne contient que des triangles
   # (et eventuellement des lignes ou des points...)

   # on recupere le nombre de noeuds du maillage
   nb_nodes=len(surfacic_mesh.nodes)

   # on compte le nombre d'elements triangles adjacents a chaque noeud
   
   # on initialise a 0 un vecteur d'entiers pour stocker le nombre d'elements triangles adjacents a chaque noeud
   nb_ele_adj=numpy.zeros(nb_nodes, 'i') 
   # pour chaque triangle
   for i in range(0, nb_bulks_T3):
      # pour chaque noeud de l'element courant
      for i_nod in surfacic_mesh.bulks[ind_T3[i]].connectivity:
         # on incremente le nombre d'elements adjacents au noeud courant
         nb_ele_adj[i_nod - 1] += 1

   # on recupere le nombre maximal d'elements adjacents a un noeud
   max_nb_ele_adj=max(nb_ele_adj)
   # on le passe du format int32, utilise par numpy, au format int standard de Python
   max_nb_ele_adj=int(max_nb_ele_adj)

   # on recupere les connectivites de tous les triangles dans un vecteur

   # on declare un vecteur a la bonne taille
   connec=numpy.zeros(3*nb_bulks_T3, 'i')
   # on initialise l'indice de l'element triangle courant a 0
   i=0
   # pour chaque element
   for ele in surfacic_mesh.bulks:
      # si l'element courant est un triangle
      if ele.etype == 'T3xxx':
         # on stocke sa connectivite
         connec[3*i:3*(i + 1)]=ele.connectivity
         # on incremente l'indice de l'element triangle courant
         i += 1

   # on attribue une entite a chaque element par une recherche de composantes connexes
   ele2entity=lmgc90.surface_T3_identify_entities(nb_nodes, max_nb_ele_adj, connec, nb_bulks_T3)

   # pour chaque element triangle
   for i in range(0, nb_bulks_T3):
      # on remplace la valeur de l'entite geometrique par la valeur d'entite obtenue precedemment
      surfacic_mesh.bulks[ind_T3[i]].geometricalEntity=str(ele2entity[i])

# fonction qui construit un corps rigide a partir d'un maillage de surface 3D, decrit comme un ensemble de noeuds et 
# un ensemble d'elements
def surfacicMeshToRigid3D(surfacic_mesh, model, material, color='BLUEx', reverse=False, is_open=False, OG=None, I=None, volume=None):
   """
   This function builds a rigid body from a surfacic mesh, by computing mass and inertia
   from surfacic elements (amazing, isn't it ^^).

   N.B.: this function only handle triangles

   parameters:

   - surfacic_mesh: the given surfacic mesh
   - model: a given model
   - material: a given material

   optional parameters:

   - color='BLUEx': color of the polyhedron contactor
   - reverse=False: should reverse connectivity of contactor or not
   - is_open=False: if the meshe is closed (default) or opened

   returned value: the built rigid body
   """
   # si le maillage surfacique donne n'est pas un maillage
   if not isinstance(surfacic_mesh, mesh):
      # on affiche un message d'erreur
      showError("the given surfacic mesh is not a mesh")

   # si le maillage n'est pas defini avec de noeuds a trois dimensions
   if surfacic_mesh.dimension != 3:
      # ca ne peut pas servir a la construction d'un avatar 3D!
      # on affiche un message d'erreur 
      showError("the given mesh is 2D!")

   # on verifie que le maillage ne contient que des triangles a trois noeuds

   # on initialise le nombre d'elements triangle a 0
   nb_bulks_T3=0
   # pour chaque element
   for ele in surfacic_mesh.bulks:
      # si l'element est un element volumique
      if ele.etype in dimension2geoElement[3]:
         # on affiche un message d'erreur
         showError("the given mesh is not a surfacic mesh")
      # sinon, si l'element est un point ou une ligne
      if not ele.etype in dimension2geoElement[2]:
         # on passe au suivant
         continue
      # si l'element courant est un element surfacique, mais n'est pas un triangle a trois noeuds
      if ele.etype != 'T3xxx':
         # on affiche un message d'erreur
         showError("the given surfacic mesh contains unhandled elements!")
      # sinon, 
      else:
         # on incremente le nombre d'elements triangles
         nb_bulks_T3 += 1

   # si le maillage ne contient aucun triangle
   if nb_bulks_T3 == 0:
      # on affiche un message d'erreur
      showError("the given surfacic mesh contains no T3!")
  
   # ici on est sur que le maillage ne contient que des triangles
   # (et eventuellement des lignes ou des points...)

   #
   # creation de l'avatar rigide
   #

   # on declare le nouvel avatar
   body=avatar(dimension=3)
   # on positionne le centre d'inertie du rigide a l'origine, pour pouvoir
   # donner les coordonnees des sommets du polyedre dans le repere global
   body.addNode( node( coor=numpy.zeros(3), number=1) )
   # on cree un comportement volumique pour le corps
   body.addBulk( rigid3d() )
   # on definit les groupes pour l'avatar
   body.defineGroups()
   # on affecte son modele a l'avatar
   body.defineModel(model=model)      
   # on affecte son materiau a l'avatar
   body.defineMaterial(material=material)

   #
   # construction du contacteur polyedre
   #

   # on renumerote les noeuds du maillage en utilisant leur rang
   surfacic_mesh.rankRenumbering()

   # on recupere le nombre de noeuds
   nb_nodes=len(surfacic_mesh.nodes)
  
   # on stocke le maillage de surface dans deux vecteurs :
   #   * les coordonnees
   # on declare un vecteur a la bonne taille
   coor=numpy.zeros(3*nb_nodes, 'd')
   # on enumere les noeuds
   for nod in surfacic_mesh.nodes:
      # on stocke les coordonnees du noeud courant
      i = nod.number-1
      coor[3*i:3*(i + 1)]=nod.coor
   #   * les connectivites
   # on declare un vecteur a la bonne taille
   connec=numpy.zeros(3*nb_bulks_T3, 'i')
   # on initialise l'indice de l'element triangle courant a 0
   i=0
   # pour chaque element
   for ele in surfacic_mesh.bulks:
      # si l'element courant est un triangle
      if ele.etype == 'T3xxx':
         # on stocke sa connectivite
         connec[3*i:3*(i + 1)]=ele.connectivity
         # on incremente l'indice de l'element triangle courant
         i += 1

   # on calcule le volume de l'objet a partir de sa surface
   if OG is None or I is None or volume is None:
     OG_c, I_c, volume_c=lmgc90.surface_T3_compute_volume_inertia(coor, connec, 3, 9)

   if OG is None:
     OG = OG_c
   if I is None:
     # on rend a la matrice d'inertie sa forme de matrice 3x3
     I  = I_c.reshape([3, 3])
   if volume is None:
     volume = volume_c

   # on donne a la liste des coordonnees des noeuds et aux tables de connectivite la forme voulue
   # pour construire un polyedre 
   coor=coor.T.reshape([nb_nodes, 3])
   connec=connec.T.reshape([nb_bulks_T3, 3])

   # on ajoute son contacteur polyedre a l'avatar
   if is_open:
     shape = 'POLYO'
     t3_mesh = deepcopy(surfacic_mesh)
     t3_mesh.bulks = [ b for b in t3_mesh.bulks if b.etype=='T3xxx' ]
     t3_mesh.rankRenumbering
     body.addContactors(shape=shape, color=color, volume=volume, I=I, shift=OG,
                        nb_patch=1, patch=[t3_mesh], reverse=reverse)
   else:
     shape = 'POLYR'
     body.addContactors(shape=shape, color=color, volume=volume, I=I, shift=OG,
                        nb_vertices=nb_nodes, nb_faces=nb_bulks_T3, vertices=coor, connectivity=connec, reverse=reverse)

   # calcul du volume et de l'inertie du corps (a partir du volume et de l'inertie 
   # du polyedre)
   body.computeRigidProperties()

   # on renvoie le corps genere
   return body

# fonction qui construit un corps rigide a partir d'un ensemble de surface 3D,
# decrit comme un ensemble de noeuds et un ensemble d'elements
def surfacicMeshesToRigid3D(surfacic_meshes, model, material, color='BLUEx',reverse=False, is_open=False, OG=None, I=None, volume=None):
   """
   This function builds a rigid body from a list of surfacic mesh, by computing mass and inertia
   from surfacic elements 

   N.B.: this function only handle triangles

   parameters:

   - surfacic_mesh: the given list of surfacic meshes
   - model: a given model
   - material: a given material

   optional parameters:

   - color='BLUEx': color of the polyhedron contactor (aka POLYF)
   - reverse=False: should reverse connectivity of contactor or not
   - is_open=False: if the meshe is closed (default) or opened

   returned value: the built rigid body
   """

   # on declare le nouvel avatar
   body=avatar(dimension=3)
   # on positionne le centre d'inertie du rigide a l'origine, pour pouvoir
   # donner les coordonnees des sommets du polyedre dans le repere global
   body.addNode( node( coor=numpy.zeros(3), number=1) )
   # on cree un comportement volumique pour le corps
   body.addBulk( rigid3d() )
   # on definit les groupes pour l'avatar
   body.defineGroups()
   # on affecte son modele a l'avatar
   body.defineModel(model=model)      
   # on affecte son materiau a l'avatar
   body.defineMaterial(material=material)


   # si le maillage surfacique donne n'est pas un maillage
   if not isinstance(surfacic_meshes, list):
      showError("the given geometry is not a list of mesh")

   # on fait les verifs de conformites des structures de donnees
   # on compte le nombre de noeuds et d'elements de la liste de maillage
   nb_nodes=0
   nb_elements=0
   for surfacic_mesh in surfacic_meshes:
     #print surfacic_mesh
     if not isinstance(surfacic_mesh, mesh):
        # on affiche un message d'erreur
        showError("the given surfacic mesh is not a mesh")

     # si le maillage n'est pas defini avec de noeuds a trois dimensions
     if surfacic_mesh.dimension != 3:
        # ca ne peut pas servir a la construction d'un avatar 3D!
        # on affiche un message d'erreur 
        showError("the given mesh is 2D!")

     surfacic_mesh.rankRenumbering()
     nb_nodes += len(surfacic_mesh.nodes)
     # on verifie que le maillage ne contient que des triangles a trois noeuds

     # on initialise le nombre d'elements triangle a 0
     nb_bulks_T3=0
     # pour chaque element
     for ele in surfacic_mesh.bulks:
        # si l'element est un element volumique
        if ele.etype in dimension2geoElement[3]:
           # on affiche un message d'erreur
           showError("the given mesh is not a surfacic mesh")
        # sinon, si l'element est un point ou une ligne
        if not ele.etype in dimension2geoElement[2]:
           # on passe au suivant
           continue
        # si l'element courant est un element surfacique, mais n'est pas un triangle a trois noeuds
        if ele.etype != 'T3xxx':
           # on affiche un message d'erreur
           showError("the given surfacic mesh contains unhandled elements!")
        # sinon, 
        else:
           # on incremente le nombre d'elements triangles
           nb_bulks_T3 += 1

     # si le maillage ne contient aucun triangle
     if nb_bulks_T3 == 0:
        # on affiche un message d'erreur
        showError("the given surfacic mesh contains no T3!")
  
     # ici on est sur que le maillage ne contient que des triangles
     # (et eventuellement des lignes ou des points...)

     nb_elements += nb_bulks_T3 


   coor=numpy.zeros(3*nb_nodes, 'd')
   connec=numpy.zeros(3*nb_elements, 'i')

   nb_nodes=0
   nb_elements=0
   for k,surfacic_mesh in enumerate(surfacic_meshes):

     #print 'patch ',k

     #
     # construction du contacteur polyedre
     #
     # coordonnees
     for nod in surfacic_mesh.nodes:
        i = nod.number-1
        #print('ajout du noeud ',nb_nodes+i,' : ',nod.coor)
        coor[3*(nb_nodes+i):3*(nb_nodes+i+1)]=nod.coor

     if reverse:
        for bulk in surfacic_mesh.bulks:
          bulk.connectivity.reverse()

     # les connectivites
     i=0
     # pour chaque element
     for ele in surfacic_mesh.bulks:
        # si l'element courant est un triangle
        if ele.etype == 'T3xxx':
           #print('ajout de l element ',nb_elements+i,' : ', [x+nb_nodes for x in ele.connectivity])
           # on stocke sa connectivite
           connec[3*(nb_elements+i):3*(nb_elements+i+1)]=ele.connectivity
           connec[3*(nb_elements+i):3*(nb_elements+i+1)]+=nb_nodes
           # on incremente l'indice de l'element triangle courant
           i += 1

     nb_nodes+=len(surfacic_mesh.nodes)
     nb_elements += i

   # on calcule le volume de l'objet a partir de sa surface
   if OG is None or I is None or volume is None:
     OG_c, I_c, volume_c=lmgc90.surface_T3_compute_volume_inertia(coor, connec, 3, 9)

   if OG is None:
     OG = OG_c
   if I is None:
     # on rend a la matrice d'inertie sa forme de matrice 3x3
     I  = I_c.reshape([3, 3])
   if volume is None:
     volume = volume_c

   #print(volume)
   #print('OG ',OG)
   #print(I)

   # on ajoute son contacteur polyedre a l'avatar
   if is_open:
     shape = 'POLYO'
   else:
     shape = 'POLYF'

   body.addContactors(shape=shape, color=color, volume=volume, I=I, shift=OG,
                      nb_patch=len(surfacic_meshes), patch=surfacic_meshes)

   # calcul du volume et de l'inertie du corps (a partir du volume et de l'inertie 
   # du polyedre)
   body.computeRigidProperties()

   # on renvoie le corps genere
   return body

# fonction qui consrtuit le maillage en hexaedres d'un paralepipede rectangle
def buildMeshH8(x0, y0, z0, lx, ly, lz, nb_elem_x, nb_elem_y, nb_elem_z, surfacic_mesh_type='Q4'):
   """buildMeshH8(x0, y0, z0, lx, ly, lz, nb_elem_x, nb_elem_y, nb_elem_z):

   this function meshes a given box, and returns the generated mesh

   WARNING: this function automaticaly defines four groups of surfacic elements:
   'left' (y=y0), 'down' (z=z0), 'right' (y=y0 + lx), 'up' (z=z0 + lz), 
   'front' (x=x0 + lx), 'rear' (x=x0)

   parameters: 

   - (x0, y0, z0) is position of the rear lower left corner of the box
   - lx: dimension of the rectangle, following the axis Ox
   - ly: dimension of the rectangle, following the axis Oy
   - lz: dimension of the rectangle, following the axis Oy
   - nb_elem_x: number of elements, following the axis Ox
   - nb_elem_y: number of elements, following the axis Oy
   - nb_elem_z: number of elements, following the axis Oz

   optional parameters:

   - surfacic_mesh_type='Q4': sufacic mesh type:

     * "Q4": classic surfacic mesh involving Q4
     * "2T3": surfacic mesh involving T3, obtained by splitting one Q4 in two T3  
   """
   
   # fonction qui renvoie le numero d'un noeud a partir du triplet d'indice (i, j, k)
   def index(i, j, k):
      return k*(nb_elem_y + 1)*(nb_elem_x + 1) + j*(nb_elem_x + 1) + i + 1

   # on clacule la longueur d'un element suivant 
   #    * la direction 0x
   delta_x = lx/float(nb_elem_x)
   #    * la direction 0x
   delta_y = ly/float(nb_elem_y)
   #    * la direction 0x
   delta_z = lz/float(nb_elem_z)

   # on cree un nouveau maillage 3D
   volumic_mesh=mesh(dimension=3)

   # calcul des coordonnees des noeuds
 
   # on initialise le numero du noeud courant a 0
   num = 0
   # pour chaque noeud suivant Oz
   for k in range(0, nb_elem_z + 1):
      # pour chaque noeud suivant Oy
      for j in range(0, nb_elem_y + 1):
         # pour chaque noeud suivant Ox
         for i in range(0, nb_elem_x + 1):
            # on incremete le numero du noeud courant
            num += 1
            # on calcule les coordonnees du noeud courant
            coor = numpy.array([x0 + i*delta_x, y0 + j*delta_y, z0 + k*delta_z])
            # on ajoute le noeud courant au maillage
            volumic_mesh.addNode( node( coor=coor, number=num) )

   # calcul des connectivites des elements volumiques

   # pour chaque element suivant Oz
   for k in range(0, nb_elem_z):
      # pour chaque element suivant Oy
      for j in range(0, nb_elem_y):
         # pour chaque element suivant Ox
         for i in range(0, nb_elem_x):
            # on construit la connectivite de l'element courant
            connectivity=[index(i, j, k), index(i + 1, j, k), index(i + 1, j + 1, k), index(i, j + 1, k),
                          index(i, j, k + 1), index(i + 1, j, k + 1), index(i + 1, j + 1, k + 1), index(i, j + 1, k + 1)]
            # on ajoute l'element courant au maillage
            volumic_mesh.addBulk( element(elem_dim=3, connectivity=connectivity) )

   # calcul des connectivites des elements surfaciques

   #    * face du dessous (i.e. z=z0)
   k = 0
   # pour chaque element suivant Oy
   for j in range(0, nb_elem_y):
      # pour chaque element suivant Ox
      for i in range(0, nb_elem_x):
         # on construit la connectivite de l'element courant
         connectivity=[index(i, j, k), index(i, j + 1, k), index(i + 1, j + 1, k), index(i + 1, j, k)]
         # on ajoute l'element courant au maillage
         volumic_mesh.addBulk( element(elem_dim=2, connectivity=connectivity, physicalEntity='down') )

   #    * face du dessus (i.e. z=z0 + lz)
   k = nb_elem_z - 1
   # pour chaque element suivant Oy
   for j in range(0, nb_elem_y):
      # pour chaque element suivant Ox
      for i in range(0, nb_elem_x):
         # on construit la connectivite de l'element courant
         connectivity=[index(i, j, k + 1), index(i + 1, j, k + 1), index(i + 1, j + 1, k + 1), index(i, j + 1, k + 1)]
         # on ajoute l'element courant au maillage
         volumic_mesh.addBulk( element(elem_dim=2, connectivity=connectivity, physicalEntity='up') )

   #    * face de gauche (i.e. y=y0)
   j = 0
   # pour chaque element suivant Oz
   for k in range(0, nb_elem_z):
      # pour chaque element suivant Ox
      for i in range(0, nb_elem_x):
         # on construit la connectivite de l'element courant
         connectivity=[index(i, j, k), index(i + 1, j, k), index(i + 1, j, k + 1), index(i, j, k + 1)]
         # on ajoute l'element courant au maillage
         volumic_mesh.addBulk( element(elem_dim=2, connectivity=connectivity, physicalEntity='left') )

   #    * face de droite (i.e. y=y0 + ly)
   j = nb_elem_y - 1
   # pour chaque element suivant Oz
   for k in range(0, nb_elem_z):
      # pour chaque element suivant Ox
      for i in range(0, nb_elem_x):
         # on construit la connectivite de l'element courant
         connectivity=[index(i + 1, j + 1, k), index(i, j + 1, k), index(i, j + 1, k + 1), index(i + 1, j + 1, k + 1)]
         # on ajoute l'element courant au maillage
         volumic_mesh.addBulk( element(elem_dim=2, connectivity=connectivity, physicalEntity='right') )

   #    * face de derriere (i.e. x=x0)
   i = 0
   # pour chaque element suivant Oz
   for k in range(0, nb_elem_z):
      # pour chaque element suivant Oy
      for j in range(0, nb_elem_y):
         # on construit la connectivite de l'element courant
         connectivity=[index(i, j, k), index(i, j, k + 1), index(i, j + 1, k + 1), index(i, j + 1, k)]
         # on ajoute l'element courant au maillage
         volumic_mesh.addBulk( element(elem_dim=2, connectivity=connectivity, physicalEntity='rear') )

   #    * face de devant (i.e. x=x0 + lx)
   i = nb_elem_x - 1
   # pour chaque element suivant Oz
   for k in range(0, nb_elem_z):
      # pour chaque element suivant Oy
      for j in range(0, nb_elem_y):
         # on construit la connectivite de l'element courant
         connectivity=[index(i + 1, j, k), index(i + 1, j + 1, k), index(i + 1, j + 1, k + 1), index(i + 1, j, k + 1)]
         # on ajoute l'element courant au maillage
         volumic_mesh.addBulk( element(elem_dim=2, connectivity=connectivity, physicalEntity='front') )

   # si l'utilisateur a demande autre chose que le maillage surfacique en Q4 classique
   if surfacic_mesh_type != "Q4":
      # selon le type de maillage surfacique demande
      if surfacic_mesh_type == "2T3": # cas du Q4 coupe en deux T3
         # pour chaque element
         for i_ele in range(0, len(volumic_mesh.bulks)):
            # on recupere l'element courant
            ele=volumic_mesh.bulks[i_ele]
            # si l'element n'est pas un Q4
            if ele.etype != 'Q4xxx':
               # on passe au suivant
               continue
           
            # ici, on est sur que l'element courant est un Q4

            # on recupere la connectivite du Q4 courant
            connectivity_Q4=ele.connectivity
            # on recupere son entite physique
            physicalEntity_Q4=ele.physicalEntity
            # on construit deux T3 pour remplacer le Q4 courant
            #    * premier element :
            # on construit la connectivite du premier element
            connectivity_1=[connectivity_Q4[0], connectivity_Q4[1], connectivity_Q4[3]]
            # on cree le premier element
            ele_T3_1=element(elem_dim=2, connectivity=connectivity_1, physicalEntity=physicalEntity_Q4)
            #    * deuxieme element :
            # on construit la connectivite du deuxieme element
            connectivity_2=[connectivity_Q4[2], connectivity_Q4[3], connectivity_Q4[1]]
            # on cree le deuxieme element
            ele_T3_2=element(elem_dim=2, connectivity=connectivity_2, physicalEntity=physicalEntity_Q4)

            # on remplace l'element courant par le premier T3
            ele_T3_1.number=i_ele
            volumic_mesh.bulks[i_ele]=ele_T3_1
            # on ajoute le deuxieme T3 a la fin de la liste des elements
            volumic_mesh.addBulk(ele_T3_2)

      # cas par defaut
      else:
         # on affiche un message d'erreur
         showError("unknown surfacic mesh type!")

   # on renvoie le maillage ainsi contruit
   return volumic_mesh

# fonction qui consrtuit le maillage en hexaedres quadratiques d'un paralepipede rectangle
def buildMeshH20(x0, y0, z0, lx, ly, lz, nb_elem_x, nb_elem_y, nb_elem_z):
   """buildMeshH20(x0, y0, z0, lx, ly, lz, nb_elem_x, nb_elem_y, nb_elem_z):

   this function meshes a given box, and returns the generated mesh and set Q4 contactors on sides.

   WARNING: this function automaticaly defines four groups of surfacic elements:
   'left' (y=y0), 'down' (z=z0), 'right' (y=y0 + lx), 'up' (z=z0 + lz), 
   'front' (x=x0 + lx), 'rear' (x=x0)

   parameters: 

   - (x0, y0, z0) is position of the rear lower left corner of the box
   - lx: dimension of the rectangle, following the axis Ox
   - ly: dimension of the rectangle, following the axis Oy
   - lz: dimension of the rectangle, following the axis Oy
   - nb_elem_x: number of elements, following the axis Ox
   - nb_elem_y: number of elements, following the axis Oy
   - nb_elem_z: number of elements, following the axis Oz

   """
   
   # fonction qui renvoie le numero d'un noeud a partir du triplet d'indice (i, j, k)
   def index(i, j, k, mix=0, miy=0, miz=0):

      assert ( miz==1 and  miy==0 and mix==0 ) or (miz==0)
      assert ( (miy==1 and mix==0) or miy==0 )

      return  k * ( (2*nb_elem_x+1)*(nb_elem_y+1) + (nb_elem_x+1)*nb_elem_y + (nb_elem_x+1)*(nb_elem_y+1) ) \
             +miz*( (2*nb_elem_x+1)*(nb_elem_y+1) + (nb_elem_x+1)*nb_elem_y ) \
             +j * (  (1-miz)*(2*nb_elem_x+1) + nb_elem_x+1 ) + miy*(2*nb_elem_x+1) \
             +i * (  2-miy-miz ) + 1 + mix

   # length to next element along each axis
   delta_x = lx/float(nb_elem_x)
   delta_y = ly/float(nb_elem_y)
   delta_z = lz/float(nb_elem_z)

   # on cree un nouveau maillage 3D
   volumic_mesh=mesh(dimension=3)

   # compute nodes coordinates
   ddz = delta_z/2.
 
   num = 0 # for node numbering
   for k in range(0, 2*nb_elem_z + 1):
      if k%2 == 0: #for a layer with 8 nodes per square
         nby = 2*nb_elem_y
         ddy = delta_y/2.
      else: #for a layer with 4 nodes per square
         nby = nb_elem_y
         ddy = delta_y
      for j in range(0, nby + 1):
         if k%2 == 0 and j%2 == 0:
            nbx = 2*nb_elem_x
            ddx = delta_x/2.
         else:
            nbx = nb_elem_x
            ddx = delta_x
         for i in range(0, nbx + 1):
            num += 1
            # on calcule les coordonnees du noeud courant
            coor = numpy.array([x0 + i*ddx, y0 + j*ddy, z0 + k*ddz])
            # on ajoute le noeud courant au maillage
            volumic_mesh.addNode( node( coor=coor, number=num) )

   # calcul des connectivites des elements volumiques

   # pour chaque element suivant Oz
   for k in range(0, nb_elem_z):
      # pour chaque element suivant Oy
      for j in range(0, nb_elem_y):
         # pour chaque element suivant Ox
         for i in range(0, nb_elem_x):
            # on construit la connectivite de l'element courant
            connectivity=[index(i, j, k      ),   index(i+1, j, k        ), index(i+1, j+1, k      ),   index(i, j+1, k        ),
                          index(i, j, k+1    ),   index(i+1, j, k+1      ), index(i+1, j+1, k+1    ),   index(i, j+1, k+1      ),
                          index(i, j, k  ,1,0,0), index(i+1, j, k  ,0,1,0), index(i  , j+1, k  ,1,0,0), index(i, j  , k  ,0,1,0),
                          index(i, j, k+1,1,0,0), index(i+1, j, k+1,0,1,0), index(i  , j+1, k+1,1,0,0), index(i, j  , k+1,0,1,0),
                          index(i, j, k  ,0,0,1), index(i+1, j, k  ,0,0,1), index(i+1, j+1, k  ,0,0,1), index(i, j+1, k  ,0,0,1)
                         ]
            # on ajoute l'element courant au maillage
            volumic_mesh.addBulk( element(elem_dim=3, connectivity=connectivity) )

   # copy past from above...

   # surfacic elements
   # down (i.e. z=z0)
   k = 0
   for j in range(0, nb_elem_y):
      for i in range(0, nb_elem_x):
         connectivity=[index(i,j,k), index(i,j+1,k), index(i+1,j+1,k), index(i+1,j,k), 
                       index(i,j,k,0,1,0), index(i,j+1,k,1,0,0), index(i+1,j,k,0,1,0), index(i,j,k,1,0,0)]
         volumic_mesh.addBulk( element(elem_dim=2, connectivity=connectivity, physicalEntity='down') )

   # up (i.e. z=z0 + lz)
   k = nb_elem_z - 1
   for j in range(0, nb_elem_y):
      for i in range(0, nb_elem_x):
         connectivity=[index(i,j,k+1), index(i+1,j,k+1), index(i+1,j+1,k+1), index(i,j+1,k+1),
                       index(i,j,k+1,1,0,0), index(i+1,j,k+1,0,1,0), index(i,j+1,k+1,1,0,0), index(i,j,k+1,0,1,0)]
         volumic_mesh.addBulk( element(elem_dim=2, connectivity=connectivity, physicalEntity='up') )

   # left (i.e. y=y0)
   j = 0
   for k in range(0, nb_elem_z):
      for i in range(0, nb_elem_x):
         connectivity=[index(i,j,k), index(i+1,j,k), index(i+1,j,k+1), index(i,j,k+1),
                       index(i,j,k,1,0,0), index(i+1,j,k,0,0,1), index(i,j,k+1,1,0,0), index(i,j,k,0,0,1)]
         volumic_mesh.addBulk( element(elem_dim=2, connectivity=connectivity, physicalEntity='left') )

   # right (i.e. y=y0 + ly)
   j = nb_elem_y - 1
   for k in range(0, nb_elem_z):
      for i in range(0, nb_elem_x):
         connectivity=[index(i+1,j+1,k), index(i,j+1,k), index(i,j+1,k+1), index(i+1,j+1,k+1),
                       index(i,j+1,k,1,0,0), index(i,j+1,k,0,0,1), index(i,j+1,k+1,1,0,0), index(i+1,j+1,k,0,0,1)]
         volumic_mesh.addBulk( element(elem_dim=2, connectivity=connectivity, physicalEntity='right') )

   # rear (i.e. x=x0)
   i = 0
   for k in range(0, nb_elem_z):
      for j in range(0, nb_elem_y):
         connectivity=[index(i,j,k), index(i,j,k + 1), index(i,j+1,k+1), index(i,j+1,k),
                       index(i,j,k,0,0,1), index(i,j,k+1,0,1,0), index(i,j+1,k,0,0,1), index(i,j,k,0,1,0)]
         volumic_mesh.addBulk( element(elem_dim=2, connectivity=connectivity, physicalEntity='rear') )

   # front (i.e. x=x0 + lx)
   i = nb_elem_x - 1
   for k in range(0, nb_elem_z):
      for j in range(0, nb_elem_y):
         connectivity=[index(i+1,j,k), index(i+1,j+1,k), index(i+1,j+1,k+1), index(i+1,j,k+1), 
                       index(i+1,j,k,0,1,0), index(i+1,j+1,k,0,0,1), index(i+1,j,k+1,0,1,0), index(i+1,j,k,0,0,1)]
         volumic_mesh.addBulk( element(elem_dim=2, connectivity=connectivity, physicalEntity='front') )

   return volumic_mesh
   ##new_mesh = mesh(3)
   ### first build Q8 mesh
   ##m2 = buildMesh2D('Q8', x0, y0, lx, ly, nb_elem_x, nb_elem_y)
   ### second extrude on all the layers
   ##nb_nodes = len(m2.nodes)
   ##for nb_z in range(nb_elem_z):
   ##  # new intermediate nodes
   ##  for n in m2.nodes[::2]:
   ##    new_coor = np.array([n.coor[0],n.coor[1],z0+nb_z*lz/(2*nb_elem_z)])
   ##    new_mesh.addNode(node(coor=new_coor,number=l*nb_nodes+n.number))
   ##  # new layer
   ##  for n in m2.nodes:
   ##    new_coor = np.array([n.coor[0],n.coor[1],z0+nb_z*lz/nb_elem_z])
   ##    new_mesh.addNode(node(coor=new_coor,number=l*nb_nodes+n.number))

# fonction qui prend un maillage 3D et l'eclate en rigides (polygones)
def rigidsFromMesh3D(volumic_mesh, model, material, color='BLUEx'):
   """rigidsFromMesh3D(volumic_mesh, model, material, color='BLUEx'):

   this function build a set of rigids from a 3D mesh, each rigid
   is a polygon made from an element of the given mesh

   parameters:

   - volumic_mesh: a 3D mesh
   - model: a given model
   - material: a given material

   optional parameter:

   - color='BLUEx': color of the polygon contactors
   """

   # on verifie que l'utilisateur a bien donne un maillage
   if not isinstance(volumic_mesh, mesh):
      # si ce n'est pas le cas, on affiche un message d'erreur
      showError("the given volumic mesh is not a mesh!")

   # on verifie que le maillage est bien 3D
   if volumic_mesh.dimension != 3:
      # si ce n'est pas le cas, on affiche un message d'erreur
      showError("the given mesh is not a volumic mesh")

   # on declare un container d'avatars pour stocker les rigides
   bodies=avatars()

   # pour chaque element du maillage
   for bulk in volumic_mesh.bulks:
      # si l'element n'est pas un volume simple
      if bulk.etype not in ['TE4xx', 'PRI6x', 'H8xxx']:
         # on passe au suivant
         continue

      vertices = []
      for num in bulk.connectivity:
          vertices.append( volumic_mesh.nodes[num].coor )
      body = rigidPolyhedron(model, material, color=color, generation_type='vertices', vertices=numpy.array(vertices))

      # on ajoute le corps rigide a la liste des corps
      bodies.addAvatar(body)

   # on renvoie la liste des corps generee
   return bodies

# Fonction qui oriente la face(i,j,k) d un element TE4xx 
def oriente_surf(body,ele,i,j,k):
    
    if not ele.etype=='TE4xx':
        showError('This function is designed for TE4xx elements')

    if not (i in ele.connectivity and j in ele.connectivity and k in ele.connectivity):
        showError('The face (i,j,k) does not belong to ele')

    for m in ele.connectivity:
        if not (m == i or m == j or m == k):
            l = m
            break

    for ic in range(0, ele.nbNodes, 1):
        if i == ele.connectivity[ic]:
            break

    ni = body.nodes[i]
    nj = body.nodes[j]
    nk = body.nodes[k]
    nl = body.nodes[l]

    v1 = nj.coor - ni.coor
    v2 = nk.coor - ni.coor
    v3 = nl.coor - ni.coor
    v4 = np.cross(v1,v2)

    if np.dot(v3,v4) > 0:
        connec0 = [(ic + 2) % ele.nbNodes + 1,(ic + 1) % ele.nbNodes + 1,ic + 1]
    else:
        connec0 = [(ic + 2) % ele.nbNodes + 1,ic + 1,(ic + 1) % ele.nbNodes + 1]

    return connec0

# fonction qui eclate un objet maille
def explodeMeshedAvatar3D(body, color='BLEUx', quadrature=0, color_dict=None):
   '''bodies=explodeMeshedAvatar3D(body, color='BLEUx', w=None):
        
   this function "explodes" a given 3D meshed avatar, i.e. gets a meshed avatar and returns a
   list of bodies, where each body is a cell of the given meshed avatar. Each new body
   have a list of contactor inherited from the connectivity of the given meshed avatar.
   
   parameters:
        
   - body: a 3D meshed avatar
   
   optional parameters:
   
   - color: default color of the contactors
   - quadrature : quadrature option for contactors
   - color_dict: a dictionnary associating a color to the physical entity of the element
   '''
   
   # on verifie que l'objet est bien un maillage :
   if not isinstance(body, avatar):
      showError('this object is not a body!')
   if body.atype != 'MAILx':
      showError('this body is not a MAILx!')
                            
   # on verifie sa dimension
   if body.dimension != 3:
      showError('this is function is designed for 3D bodies!')
                                    
   # on verifie que l'objet soit maille avec des elements d'ordre 1
   for ele in body.bulks:
      if ele.etype != 'TE4xx' and ele.etype != 'T3xxx':
         showError('this function is designed for TE4xx elements!')
                                                
   # on definit une liste d'avatar, qui va contenir la liste des elements
   # devenus corps independants
   bodies = avatars()
   
   # on definit :
   #   *  la table qui associe un numero d'element du maillage a l'indice du nouveau corps
   #      qui lui correspond
   ele2bodyIndex={}
   #   *  la table qui associe un numero d'element d'un nouveau corps au numero d'element du maillage
   #      qui lui correspond
   body2eleIndex={}
                                                        
   if not color_dict:
      color_dict = {}
          
   # on cree un corps pour chaque element fini
                                                                
   bodyIndex=0
   # pour chaque element fini
   for nbe,ele in enumerate(body.bulks):
      # si l'element n'est pas un element de volume
      if not ele.etype in dimension2geoElement[3]:
         # on saute l'element
         continue
      # on ajoute le nouveau corps dans la table qui associe un numero d'element du maillage
      # a l'indice du nouveau corps
      ele2bodyIndex[ele.number]=bodyIndex
      # et dans la table qui associe l'indice du nouveau corps au numero d'element du maillage
      body2eleIndex[bodyIndex]=ele.number
                                                                                    
      bodyIndex += 1
                                                                                        
      # on cree un nouveau corps maille 3D
      new_body = avatar(dimension=3)
      # on ajoute au nouveau corps un nouvel element fini, du meme type que
      # l'element courant mais dont la connectivite est triviale
      # N.B.: on conserve la physical entity, i.e. le groupe de l'element
      new_body.addBulk( element(elem_dim=geoElement2dimension[ele.etype],
                                connectivity=list(range(1, ele.nbNodes + 1, 1)),
                                physicalEntity=ele.physicalEntity) )
                                                                                            
      # on ajoute au corps les seuls noeuds dont il a besoin
      
      # on initialise le nombre de noeuds du nouvel element a 0
      nbNodes=0
      # pour chaque noeud de l'element courant
      for i in ele.connectivity:
         # on incremente le nombre de noeuds du nouvel element
         nbNodes += 1
         # on recupere l'objet node associe
         n = body.nodes[i]
         # on peut alors construire le nouveau noeud, connaissant ses
         # coordonnees
         new_body.addNode( node( coor=n.coor, number=nbNodes) )
      
      # on ajoute le corps genere au container
      bodies += new_body

   # on definit un container pour stocker les listes d'adjacence de chaque
   # noeud
   l_node2ele = []
                                                                                                                
   # pour chaque noeud du corps, on initialise une liste d'adjacence vide
   for n in body.nodes:
      l_node2ele.append([])
                                                                                                                            
   # pour chaque element du maillage
   for ele in body.bulks:
      # si l'element n'est pas un element de volume
      if not ele.etype in dimension2geoElement[3]:
         # on saute l'element
         continue
      # pour chaque noeud de l'element
      for n in ele.connectivity:
         # on ajoute l'element courant dans la liste d'adjacence du noeud
         # courant
         l_node2ele[n - 1].append(ele)
   # ici, la liste d'adjacence de chaque noeud est remplie
                                                                                                                                                
   # on s'en sert par la suite pour placer des contacteurs sur les elements de
   # la surface libre

   # pour chaque element
   for ele in body.bulks:

       # si l'element n'est pas un element de volume
       if not ele.etype in dimension2geoElement[3]:
          # on saute l'element
          continue
   
       # pour chaque noeud de l'element
       for ic in range(0, ele.nbNodes, 1):
          # On recupere le numero du noeud
          i = ele.connectivity[ic]
          # et les deux noeuds suivants, dans la table de connectivite de
          # l'element
          j = ele.connectivity[(ic + 1) % ele.nbNodes]
          k = ele.connectivity[(ic + 2) % ele.nbNodes]
          # (i,j,k) definit une face de l'element courant
          
          # on cherche maintenant l'element adjacent a l'element courant par
          # cette face
          
          # on indique qu'on ne l'a pas encore trouve
          is_found = 0
          found_ele = None
          # pour chaque element adjacent au noeud i
          for adj_ele in l_node2ele[i - 1]:
             # si on a trouve un element adjacent a la face (i, j, k)
             if is_found:
                # on sort de la boucle
                break
             # si c'est l'element courant
             if ele == adj_ele:
                # on passe au suivant
                continue
             # pour chaque noeud de l'element adjacent courant
             comp0 = 0
             for m in adj_ele.connectivity:
                 if m == j or m == k:
                    comp0 += 1
                 if comp0 == 2:
                    # on a trouve l'element adjacent a l'element courant, par
                    # la face (i, j, k)
                    is_found = 1
                    found_ele = adj_ele
                    # on sort de la boucle
                    break
          
          # si on a trouve l'element adjacent a l'element courant par
          # la face (i, j, k)
          if is_found:
             # la face (i, j, k) est dans le volume, et doit porter des
             # contacteur candidat et antagoniste en vis-a-vis

             # on recupere le nouveau corps associe a l'element courant
             new_body = bodies[ele2bodyIndex[ele.number]]
             # on construit l'element qui supporte le contacteur : un triangle
             # a trois noeuds
             connec0 = oriente_surf(body,ele,i,j,k)
             surf = element(elem_dim=2, connectivity=connec0)
             # on positionne un contacteur sur l'element courant, en fonction
             # de son numero
             try:
                col = color_dict[ele.physicalEntity]
             except KeyError:
                col = color
                
             if ele.number > found_ele.number:
                # si son numero est plus grand que celui de l'element adjacent
                # il porte des noeuds candidats
                # on cree un contacteur candidat
                cd = contactorFactory(elements=[surf], shape='CSpxx', color=col, quadrature = quadrature)
                # on l'ajoute au nouveau corps correspondant a l'element
                # courant
                # N.B.: on utilise ici une methode privee de la classe avatar
                #       a dessin! Cette methode est declaree privee pour qu'elle
                #       ne soit pas utilisee dans les scripts utilisateurs. Ici,
                #       on sait ce qu'on fait en ajoutant un contacteur a la main!
                new_body._addContactor(cd)
                
             else:
                # sinon, il porte une surface antagoniste
                
                # on cree un contacteur antagoniste
                an = contactorFactory(elements=[surf], shape='ASpxx', color=col)
                # on l'ajoute au nouveau corps correspondant a l'element
                # courant
                # N.B.: on utilise ici une methode privee de la classe avatar
                #       a dessin! Cette methode est declaree privee pour qu'elle
                #       ne soit pas utilisee dans les scripts utilisateurs. Ici,
                #       on sait ce qu'on fait en ajoutant un contacteur a la main!
                new_body._addContactor(an)
          else:
             # sinon, le triangle appartient a la surface libre et on doit recuperer
             # l'element surface associe, s'il existe
                      
             # on recupere le nouveau corps associe a l'element courant
             new_body = bodies[ele2bodyIndex[ele.number]]
             # on definit un objet pour recevoir l'element surface associe
             found_surf = None
             # pour chaque element
             for ele_surf in body.bulks:
                # si l'element n'est pas un triangle
                if ele_surf.etype != 'T3xxx':
                   # on passe au suivant
                   continue
                   # si la connectivite correspond
                if (i in ele_surf.connectivity and j in ele_surf.connectivity and k in ele_surf.connectivity):
                   # on a trouve l'element surface
                   found_surf = ele_surf
                   # on sort de la boucle
                   break
                      
             # si l'element surface associe existe
             if found_surf != None:
                # on cree l'element surface a ajouter au corps
                connec0 = oriente_surf(body,ele,i,j,k)
                surf = element(elem_dim=2, connectivity=connec0, physicalEntity=found_surf.physicalEntity)
                    
                # on l'ajoute au nouveau corps
                new_body.addBulk(surf)
                         
   # on enumere les nouveau avatars
   for num, new_body in enumerate(bodies):
      # on definit la liste des groupes du nouveau corps
      new_body.defineGroups()
      # on recupere l'element de l'avatar continu, auquel est associe le
      # nouveau corps courant
      bulk=body.bulks[body2eleIndex[num]]
      # on attribue au nouveau corps le modele de l'element de l'avatar
      # continu qui lui associe
      new_body.defineModel(model=bulk.model)
      # on attribue au nouveau corps le materiau de l'element de l'avatar
      # continu qui lui associe
      new_body.defineMaterial(material=bulk.material)
   # on renvoie le container de corps genere
   return bodies



