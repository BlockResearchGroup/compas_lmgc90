# module qui fournit des macros pour construire et/ou manipuler des maillages 2D

import numpy
from copy import deepcopy

from ..avatar.avatar import *
from ..avatar.bulk.element import *
from ..avatar.contactor.contactor import *
from ..avatar.group.group import *
from ..avatars import *

from .mesh import *

from ..utilities.error    import *

# import du module permettant de savoir si on pourra importer les pre_tools
from ..utilities.check_compiled_modules import *

from ..avatar.contactor.contactorFactory import contactorFactory

# si on peut essayer d'importer le module pre_tools sans tout faire planter
if import_lmgc90():
   # on essaye
   try:
      from ...chipy import lmgc90
   except ImportError:
      print('Unable to import wrapped part of the 2D mesh generator module!')
      print('You must build 2D meshes using an external tool')
   except:
      raise

# fonction qui construit un corps deformable a partir en maillant un rectangle :
#   - mesh_type : type de maillage :
#        - 'Q4' : maillage en Q4
#        - '2T3' : maillage en T3, obtenus en coupant en deux des Q4
#        - '4T3' : maillage en T3, obtenus en coupant en quatre des Q4
#        - 'Q8' : maillage en Q8
#   - (x0, y0) : position du coin inferieur gauche du recatngle
#   - lx : dimension du rectangle, suivant l'axe x
#   - ly : dimension du rectangle, suivant l'axe y
#   - nb_elem_x : nombre d'elements, suivant l'axe x
#   - nb_elem_y: nombre d'elements, suivant l'axe y
#   - material : materiau
#   - model : modele
#   - number : numero du corps
def buildMesh2D(mesh_type, x0, y0, lx, ly, nb_elem_x, nb_elem_y, vertices=None, number=None):
   '''buildMesh2D=buildMesh2D(mesh_type, x0, y0, lx, ly, nb_elem_x, nb_elem_y, vertices=None, number=None):

   this function meshes a given rectangle, and returns the generated mesh

   WARNING: this function automaticaly defines four groups of surfacic elements:
   'left', 'down', 'right', 'up'

   parameters: 

   - mesh_type: type of mesh:

     - 'Q4': mesh with elements Q4
     - '2T3': mesh with elements T3, obtained by spitting one Q4 in two T3
     - '4T3': mesh with elements T3, obtained by spitting one Q4 in four T3 
     - 'Q8': mesh with elements Q8

   - (x, y) is position of the lower left corner of the rectangle 
   - lx: dimension of the rectangle, following the axis Ox
   - ly: dimension of the rectangle, following the axis Oy
   - nb_elem_x: number of elements, following the axis Ox
   - nb_elem_y: number of elements, following the axis Oy

   optional parameters:

   - vertices=None: a given list of x,y-coordinates, following a suitable Q4-mesh node ordering
   - number=None: index of the avatar (still present to ensure compatibility)'''
   # test de coherence

   # si on donne une liste de sommets
   if ( vertices ):
      # le maillage doit etre en Q4, partir du point (0, 0), et verifier
      # lx = nb_elem_x et ly = nb_elem_y
      if mesh_type != 'Q4':
         showError('only a Q4 mesh accepts a list of vertices!')
      if x0 != 0. or y0 != 0.:
         showError('when list of vertices is given, (x0, y0) must be (0., 0.)!' )
      if lx != float(nb_elem_x) or ly != float(nb_elem_y):
         showError('when list of vertices is given, (lx, ly) must be (float(nb_elem_x), float(nb_elem_y))!')

   # calcul du maillage, selon son type :

   if mesh_type == 'Q4': # cas du maillage en Q4 
      # dimensionnement des vecteurs pour stocker le maillage
      [size_nodes, size_nb_node_per_ele_vol, size_conn_vol]=lmgc90.mesh2D_SizeMeshQ4(nb_elem_x, 
         nb_elem_y)
      # calcul du maillage
      [nodes, nb_node_per_ele_vol, conn_vol]=lmgc90.mesh2D_MeshQ4(x0, y0, lx, ly, 
         nb_elem_x, nb_elem_y, size_nodes, size_nb_node_per_ele_vol, 
         size_conn_vol)
   elif mesh_type == '2T3': # cas du maillage en T3, obtenus en coupant en deux 
      # des Q4
      [size_nodes, size_nb_node_per_ele_vol, size_conn_vol]=lmgc90.mesh2D_SizeMesh2T3(nb_elem_x, 
         nb_elem_y)
      # calcul du maillage
      [nodes, nb_node_per_ele_vol, conn_vol]=lmgc90.mesh2D_Mesh2T3(x0, y0, lx, ly, 
         nb_elem_x, nb_elem_y, size_nodes, size_nb_node_per_ele_vol, 
         size_conn_vol)
   elif mesh_type == '4T3': # cas du maillage en T3, obtenus en coupant en 
      # quatre des Q4
      [size_nodes, size_nb_node_per_ele_vol, size_conn_vol]=lmgc90.mesh2D_SizeMesh4T3(nb_elem_x, 
         nb_elem_y)
      # calcul du maillage
      [nodes, nb_node_per_ele_vol, conn_vol]=lmgc90.mesh2D_Mesh4T3(x0, y0, lx, ly, 
         nb_elem_x, nb_elem_y, size_nodes, size_nb_node_per_ele_vol, 
         size_conn_vol)
   elif mesh_type == 'Q8': # cas du maillage en Q8 
      # dimensionnement des vecteurs pour stocker le maillage
      [size_nodes, size_nb_node_per_ele_vol, size_conn_vol]=lmgc90.mesh2D_SizeMeshQ8(nb_elem_x, 
         nb_elem_y)
      # calcul du maillage
      [nodes, nb_node_per_ele_vol, conn_vol]=lmgc90.mesh2D_MeshQ8(x0, y0, lx, ly, 
         nb_elem_x, nb_elem_y, size_nodes, size_nb_node_per_ele_vol, 
         size_conn_vol)

   # si on donne une liste de sommets
   if ( vertices ):
     # on l'utilise pour recuperer les coordonnees des noeuds
      nodes = vertices

   # on cree un nouveau maillage 2D
   surfacic_mesh=mesh(dimension=2)

   # on initilialise l'indice de debut de lecture dans le vecteur stockant la
   # table de connectivite des elements volumiques a 0
   beg_conn = 0
   # pour chaque element
   for i in range(0, len(nb_node_per_ele_vol), 1):

      # on recupere la connectivite de l'element, sous la forme d'une liste
      conn=conn_vol[beg_conn:beg_conn + nb_node_per_ele_vol[i]].tolist()

      # si on a donne une liste de sommets
      if ( vertices ):
         # on verifie l'orientation de l'element 

         # on recupere les coordonnees des trois premiers noeuds de l'element
         #   * le premier noeud
         k = conn[0] - 1
         coor_1 = nodes[2*k: 2*(k + 1)]
         #   * le deuxieme noeud
         k = conn[1] - 1
         coor_2 = nodes[2*k: 2*(k + 1)]
         #   * le troisieme noeud
         k = conn[2] - 1
         coor_3 = nodes[2*k: 2*(k + 1)]

         # si (x2 - x1)*(y3 - y2) - (y2 - y1)*(x3 - x2) < 0
         if (coor_2[0] - coor_1[0])*(coor_3[1] - coor_2[1]) - (coor_2[1] - coor_1[1])*(coor_3[0] - coor_2[0]) < 0.:
            # l'element n'est pas dans le bon sens
            # on le retourne
            conn.reverse()

      # on ajoute l'element au maillage
      surfacic_mesh.addBulk( element(elem_dim=2, connectivity=conn) )
      
      # on met a jour l'indice de debut de lecture
      beg_conn = beg_conn + nb_node_per_ele_vol[i]

   # pour chaque noeud
   for i in range(0, len(nodes)//2, 1):
      # on ajoute le noeud au maillage
      surfacic_mesh.addNode( node( coor=nodes[2*i: 2*(i + 1)], 
              number=i + 1) )

   # on definit la tolerance pour la recherche de l'appartenance des noeuds
   # aux groupes ('left', 'down', 'right', ou 'up')
   tol = 0.01*min(lx/nb_elem_x, ly/nb_elem_y) 
   # on calcule les coordonnees du noeud en haut a droite du rectangle
   x_max = x0 + lx
   y_max = y0 + ly  

   # recherche de la surface libre, pour donner un groupe aux lignes la 
   # composant
   
   # on definit un container pour stocker les listes d'adjacence de chaque 
   # noeud 
   l_node2ele = []
   
   # pour chaque noeud du corps, on intialise une liste d'adjacence vide
   #for n in body.nodes:
   # pour chaque noeud du maillage, on intialise une liste d'adjacence vide
   for n in surfacic_mesh.nodes:
      l_node2ele.append([])
   
   # pour chaque element du maillage 
   #for ele in body.bulks:
   # pour chaque element du maillage 
   for ele in surfacic_mesh.bulks:
      # pour chaque noeud de l'element
      for n in ele.connectivity:
         # on ajoute l'element courant dans la liste d'adjacence du noeud 
         # courant
         l_node2ele[n - 1].append(ele)
   
   # ici, la liste d'adjacence de chaque noeud est remplie
   
   # on s'en sert par la suite pour placer des contacteurs sur les elements de
   # la surface libre 
   
   # pour chaque element
   #for ele in body.bulks:
   for ele in surfacic_mesh.bulks:
      # pour chaque noeud de l'element
      for ic in range(0, ele.nbNodes, 1): 
         # si l'element est un Q8
         if ele.etype == 'Q8xxx' and ic >= ele.nbNodes//2:
            # on laisse tomber les noeuds aux centres des lignes
            continue
         # on recupere le numero du noeud
         i = ele.connectivity[ic]
         # et le numero du noeud suivant, dans la table de connectivite de
         # l'element
         if ele.etype == 'Q8xxx': # cas particulier du Q8
            # on recupere le noeud sommet suivant
            j = ele.connectivity[(ic + 1) % (ele.nbNodes//2)]
         else: # cas general
            j = ele.connectivity[(ic + 1) % ele.nbNodes]
         # (i, j) definit une arete de l'element courant
    
         # on cehrche maintenant l'element adjacent a l'element courant par 
         # cette arete 
   
         # on indique qu'on ne l'a pas encore trouve
         is_found = None
         # pour chaque element ajdacent au noeud i
         for adj_ele in l_node2ele[i - 1]:
            # si on a trouve l'element adjacent
            if is_found:
               # on sort de la boucle
               break
            # si c'est l'element courant
            if ele == adj_ele:
               # on passe au suivant
               continue
            # pour chaque noeud de l'element adjacent courant
            for k in adj_ele.connectivity:
               # si c'est le noeud j
               if k == j:
                  # on a trouve l'element adjacent a l'element courant, par
                  # l'arete (i, j)  
                  is_found = 1
                  # on sort de la boucle
                  break

         # si on n'a pas trouve l'element adjacent a l'element courant par
         # l'arete (i, j)
         if not is_found:
            # alors l'arete (i, j) fait partie de la surface libre et on 
            # doit lui ajouter un ou plusieurs contacteurs
              
            # on cherche a quel groupe appartient l'element
            # on recupere les coordonnees des noeuds i et j
            ni = surfacic_mesh.nodes[i]
            [xi, yi] = ni.coor
            nj = surfacic_mesh.nodes[j]
            [xj, yj] = nj.coor
            # on determine le groupe de l'element
            # si le maillage est en Q4
            if mesh_type == 'Q4':
               # on utilise les inidces corespondant aux noeuds

               # on recuepre les indices pour le noeud i
               u_i, v_i = lmgc90.mesh2D_GetIndicesMeshQ4(int(i))
               # on recuepre les indices pour le noeud j
               u_j, v_j = lmgc90.mesh2D_GetIndicesMeshQ4(int(j))

               if u_i == 1 and u_j == 1:
                  # cas d'un element sur le bord gauche : x=x0
                  pE='left'
               if v_i == 1 and v_j == 1:
                  # cas d'un element sur le bord bas : y=y0
                  pE='down'
               if u_i == nb_elem_x + 1 and u_j == nb_elem_x + 1:
                  # cas d'un element sur le bord droit : x=x0 + lx
                  pE='right'
               if v_i == nb_elem_y + 1 and v_j == nb_elem_y + 1:
                  # cas d'un element sur le bord bas : y=y0 + ly
                  pE='up'
            else:
               # on utlise la position des deux neouds 
               if abs(xi - x0) < tol and abs(xj - x0) < tol:
                  # cas d'un element sur le bord gauche : x=x0
                  pE='left'
               if abs(yi - y0) < tol and abs(yj - y0) < tol:
                  # cas d'un element sur le bord bas : y=y0
                  pE='down'
               if abs(xi - x_max) < tol and abs(xj - x_max) < tol:
                  # cas d'un element sur le bord droit : x=x0 + lx
                  pE='right'
               if abs(yi - y_max) < tol and abs(yj - y_max) < tol:
                  # cas d'un element sur le bord bas : y=y0 + ly
                  pE='up'

            # on construit le nouvel element sur la surface libre : 
            #   * cas particulier du Q8 : une ligne a trois noeuds
            if ele.etype == 'Q8xxx':
               # on recupere le numero nu noeud au centre de l'arete
               k = ele.connectivity[(ic + (ele.nbNodes//2)) % ele.nbNodes] 
               # ATTENTION: on conserve le sens trigonometrique de la
               #    description de la connectivite des Q8, pour definir la
               #    ligne a trois noeuds 
               surf = element(elem_dim=1, connectivity=[i, k, j], physicalEntity=pE)
            #   * cas general : une ligne a deux noeuds
            else:
               # ATTENTION : la connectivite d'une ligne supportant un 
               #    contacteur est definie dans le sens anti-trigonometrique
               surf = element(elem_dim=1, connectivity=[j, i], physicalEntity=pE) 
   
            # on ajoute l'element au maillage
            surfacic_mesh.addBulk(surf)

   # on renvoie le maillage genere
   return surfacic_mesh

# fonction qui eclate un objet maille
def explodeMeshedAvatar2D(body, nbPoints=2, color='BLUEx', w=None, color_dict=None):
   '''bodies=explodeMeshedAvatar2D(body, nbPoints=2, color='BLUEx', w=None):

   this function "explodes" a given 2D meshed avatar, i.e. gets a meshed avatar and returns a
   list of bodies, where each body is a cell of the given meshed avatar. Each new body
   have a list of contactor inherited from the connectivity of the given meshed avatar.

   parameters:

   - body: a 2D meshed avatar

   optional parameters:

   - nbPoints: number of points on the contactors candidate
   - color: default color of the contactors
   - w: vector of CLxxx positions 
   - color_dict: a dictionnary associating a color to the physical entity of the element
   '''

   # on verifie que l'objet est bien un maillage :
   if not isinstance(body, avatar):
      showError('this object is not a body!')
   if body.atype != 'MAILx':
      showError('this body is not a MAILx!')

   # on verifie sa dimension
   if body.dimension != 2:
      showError('this is function is designed for 2D bodies!')

   # on verifie que l'objet soit maille avec des elements d'ordre 1
   for ele in body.bulks:
      if ele.etype == 'Q8xxx' or ele.etype == 'T6xxx': 
         showError('this function is designed for linear elements!')

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
   for ele in body.bulks:
      # si l'element n'est pas un element de surface
      if not ele.etype in dimension2geoElement[2]:
         # on saute l'element
         continue

      # on ajoute le nouveau corps dans la table qui associe un numero d'element du maillage
      # a l'indice du nouveau corps
      ele2bodyIndex[ele.number]=bodyIndex
      # et dans la table qui associe l'indice du nouveau corps au numero d'element du maillage
      body2eleIndex[bodyIndex]=ele.number

      bodyIndex += 1

      # on cree un nouveau corps maille 2D
      new_body = avatar(dimension=2)
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

   # pour chaque noeud du corps, on intialise une liste d'adjacence vide
   for n in body.nodes:
      l_node2ele.append([])

   # pour chaque element du maillage 
   for ele in body.bulks:
      # si l'element n'est pas un element de surface
      if not ele.etype in dimension2geoElement[2]:
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

      # si l'element n'est pas un element de surface
      if not ele.etype in dimension2geoElement[2]:
         # on saute l'element
         continue

      if not (ele.model.physics == 'MECAx' ):
         continue

      # pour chaque noeud de l'element
      for ic in range(0, ele.nbNodes, 1): 
         # on recupere le numero du noeud
         i = ele.connectivity[ic]
         # et le numero du noeud suivant, dans la table de connectivite de
         # l'element
         j = ele.connectivity[(ic + 1) % ele.nbNodes]
         # (i, j) definit une arete de l'element courant

         # on cherche maintenant l'element adjacent a l'element courant par 
         # cette arete 

         # on indique qu'on ne l'a pas encore trouve
         is_found = None
         found_ele = None
         # pour chaque element ajdacent au noeud i
         for adj_ele in l_node2ele[i - 1]:
            # si on a trouve l'element adjacent
            if is_found:
               # on sort de la boucle
               break
            # si c'est l'element courant
            if ele == adj_ele:
               # on passe au suivant
               continue
            # pour chaque noeud de l'element adjacent courant
            for k in adj_ele.connectivity:
               # si c'est le noeud j
               if k == j:
                  # on a trouve l'element adjacent a l'element courant, par
                  # l'arete (i, j)  
                  is_found = 1
                  found_ele = adj_ele
                  # on sort de la boucle
                  break

         # si on a trouve l'element adjacent a l'element courant par
         # l'arete (i, j)
         if is_found:
            # l'arete (i, j) est dans le volume, et on doit porter des
            # contacteur candidat et antagoniste en vis-a-vis

            # on recupere le nouveau corps associe a l'element courant
            new_body = bodies[ele2bodyIndex[ele.number]]
            # on construit l'element qui supporte le contacteur : une ligne
            # a deux noeuds
            # ATTENTION : la connectivite d'une ligne supportant un contacteur
            #    est definie dans le sens anti-trigonometrique
            surf = element(elem_dim=1, connectivity=[(ic + 1) % ele.nbNodes + 1, ic + 1])
            # on positionne un contacteur sur l'element courant, en fonction 
            # de son numero
            try:
              col = color_dict[ele.physicalEntity]
            except KeyError:
              col = color

            if ele.number > found_ele.number:
               # si son numero est plus grand que celui de l'element adjacent
               # il porte des noeuds candidats

               # on initialise a vide la liste des poids associes aux points
               # candidats
               weights=numpy.zeros(nbPoints, 'd')
               # Est ce que les poids sont fournis en argument
               if ( w ) and ( len( w ) == nbPoints ):
                  # On prend ceux la 
                  weights=w
               # sinon on les calcul
               else:
                  # pour chaque noeud candidat
                  for i in range(0, nbPoints, 1):
                     # on calcule le poids associe au point courant
                     weights[i]=(0.5 + i)/nbPoints
               # on cree un contacteur candidat
               cd = contactorFactory(elements=[surf], shape='CLxxx', color=col, weights=weights)
               # on l'ajoute au nouveau corps corespondant a l'element 
               # courant
               # N.B.: on utilise ici une methode privee de la classe avatar
               #       a dessein! Cette methode est declaree privee pour qu'elle
               #       ne soit pas utilisee dans les scripts utilisateurs. Ici,
               #       on sait ce qu'on fait en ajoutant un contacteur a la main!
               new_body._addContactor(cd)
            else:
               # sinon, il porte une ligne antagoniste            
                
               # on cree un contacteur antagoniste
               an = contactorFactory(elements=[surf], shape='ALpxx', color=col)
               # on l'ajoute au nouveau corps corespondant a l'element 
               # courant
               # N.B.: on utilise ici une methode privee de la classe avatar
               #       a dessein! Cette methode est declaree privee pour qu'elle
               #       ne soit pas utilisee dans les scripts utilisateurs. Ici,
               #       on sait ce qu'on fait en ajoutant un contacteur a la main!
               new_body._addContactor(an)
         else:
            # sinon, l'arete appartient a la surface libre et doit recuperer
            # l'element ligne associe, s'il existe

            # on definit un objet pour recevoir l'element ligne associe
            found_line = None 
            # pour chaque element
            for ele_surf in body.bulks:
               # si l'element n'est pas une ligne
               if ele_surf.etype != 'S2xxx':
                  # on passe au suivant
                  continue
               # si la connectivite correspond
               if ele_surf.connectivity == [i, j] or ele_surf.connectivity == [j, i]:
                  # on a trouve l'element ligne 
                  found_line = ele_surf
                  # on sort de la boucle
                  break

            # si l'element ligne associe existe
            if found_line != None:
               # on recupere le nouveau corps associe a l'element courant
               new_body = bodies[ele2bodyIndex[ele.number]]
               # on cree l'element ligne a ajouter au corps
               # ATTENTION : la connectivite d'une ligne supportant un 
               #    contacteur est definie dans le sens anti-trigonometrique
               surf = element(elem_dim=1,
                              connectivity=[(ic + 1) % ele.nbNodes + 1, ic + 1],
                              physicalEntity=found_line.physicalEntity)
               # on l'ajoute au nouveau corps
               new_body.addBulk(surf)

            # else:
            #    print('wtf')
            #    print(ele.etype)
            #    print(ele.physicalEntity)
            #    print(i,j)

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


# fonction qui insert des fissures dans un objet maille
def crackMeshedAvatar2D(body, crackgroup):
   '''newbody,newgroups = crackMeshedAvatar2D(body, crackgroup):

   this function had a set of "cracks" to a given 2D meshed avatar
   it adds nodes and 1D elements along the crack 

   parameters:

   - (I) body: a 2D meshed avatar
   - (I) crackgroup: the group name of 1D elements concerned by cracking
   - (O) newbody : a new 2D meshed with additional nodes and 1D elements along crackgroup
   - (O) newgroups : a list of groups created while adding new nodes and 1D elements
   '''

   bavard=0
   
   # on verifie que l'objet est bien un maillage :
   if not isinstance(body, avatar):
      showError('crackMeshedAvatar2D::this object is not a body!')
   if body.atype != 'MAILx':
      showError('crackMeshedAvatar2D::this body is not a MAILx!')

   # on verifie sa dimension
   if body.dimension != 2:
      showError('crackMeshedAvatar2D::this is function is designed for 2D bodies!')

   # on verifie que l'objet soit maille avec des elements d'ordre 1
   for ele in body.bulks:
      if ele.etype == 'Q8xxx' or ele.etype == 'T6xxx': 
         showError('crackMeshedAvatar2D::this function is designed for linear elements!')

   # on construit la map noeud -> elements 2D 
   node2ele_ = {}
   # pour chaque element du maillage 
   for ele_ in body.bulks:
      # si l'element est 2D
      if ele_.etype in dimension2geoElement[2]:
        # pour chaque noeud de l'element
        for n_ in ele_.connectivity:
           # on ajoute l'element dans la liste d'adjacence du noeud
           node2ele_.setdefault(n_,[]).append(ele_)

   # on reconstruit la topo des lignes (liste des elements 1D portes par une ligne)
   lines_ = {}
   for bulk_ in body.groups[crackgroup].bulks :
     if bulk_.nbNodes != 2 :
       showError('crackMeshedAvatar2D::strange this element should have only 2 nodes')
     if int(bulk_.geometricalEntity) > 9999 :
      showError('crackMeshedAvatar2D::geometricalEntity rank greater than 9999 which is incompatible with new groups numbering (4 characters)')        
     lines_.setdefault(bulk_.geometricalEntity,[]).append(bulk_.number)

   if bavard : print('number of crack lines ',len(lines_)) 
   
   # looking for lines extremities
   if bavard : print("--lines extremity and internal nodes--")
   
   lines_extremity_nodes_ = {}
   lines_internal_nodes_={}
   
   for (g_,line_) in lines_.items():
     beg_=[]
     end_=[]
     for ele_ in line_:
       beg_.append(body.bulks[ele_].connectivity[0])
       end_.append(body.bulks[ele_].connectivity[1])
     
     lines_extremity_nodes_[g_] = set(beg_).symmetric_difference(end_)
     lines_internal_nodes_[g_] = set(beg_).intersection(end_)
     
     if len(lines_extremity_nodes_[g_]) == 0:
       print('line ',g_,'is a closed loop')
       showError('crackMeshedAvatar2D::closed loop not managed yet')

     if len(lines_extremity_nodes_[g_]) > 2:
       print('line ',g_,'has more than 2 extremities')
       showError('crackMeshedAvatar2D::strange loop not managed yet')

   if bavard :
     for g_,v_ in lines_extremity_nodes_.items() :
        print('line ',g_,' extremities ',v_)
       
   # looking for corners
   if bavard : print("--corners--")
   
   # pour chaque coin on veut stocker un set de ligne
   corners_={}
   # fd dit "recherche n^2 de merde" (xg_|yg_ index ligne xx_|yy_ noeuds extermites) 
   for (xg_,xx_) in lines_extremity_nodes_.items(): 
      for (yg_,yy_) in lines_extremity_nodes_.items():
         if yg_ == xg_ : continue
         c_ = set(xx_).intersection(yy_)
         if len(c_) == 0 : continue
         # if bavard : print('possible corner node ',c_,' between line ',xg_,' and line ',yg_)
         if len(c_) == 1:
           k_=c_.pop()
           # si deja stocke on oublie
           if k_ in corners_.keys() and (xg_ in corners_[k_] and yg_ in corners_[k_]):
             continue 
           else :
             # if bavard : print('keep corner node ',k_,' between line ',xg_,' and line ',yg_)
             if k_ not in corners_.keys() :
               corners_[k_] = set()
             corners_[k_] = set([xg_,yg_]).union(corners_[k_])                
             # corners_.setdefault(k_,set()).set([xg_,yg_]).union(corners_[k_]) 
         if len(c_) > 1 :
           showError('crackMeshedAvatar2D::more than 1 corner node between line ',xg_,' and line ',yg_,' impossible ')

   if bavard :       
     for (corner_,e_) in corners_.items():
        print(corner_,' is corner of lines ',e_)
    
   if bavard : print("--adding nodes along lines--")
   # numero de noeud max avant de toucher (attention on commence a 1)
   nbn_= len(body.nodes)
   # on va dupliquer les noeuds
   # map ancien -> nouveau
   n2nn_={}
   # map nouveau -> ancien
   nn2n_={}
   # boucle sur les lignes
   for (g_,line_) in lines_.items():
     # print('line',g_,' internal nodes ',lines_internal_nodes_[g_])       
     # on duplique les noeuds
     for n_ in lines_internal_nodes_[g_]:
       coor_ = body.nodes[n_].coor
       nb_=len(body.nodes)
       body.addNode(node(coor=coor_,number=nb_+1))
       nn_ = len(body.nodes)
       # print(n_,' -> ',nn_)
       n2nn_[n_]=nn_
       nn2n_[nn_]=n_       
       body.nodes[nn_].dof = body.nodes[n_].dof
       
   if bavard : print("--modifying elements along lines--")
   algrp_=[]
   for (g_,line_) in lines_.items():
     if bavard : print('line',g_,' nb segments ',len(line_))

     # on cree un nom de groupe pour les AL
     name_=list('*****')
     grp_ = str(g_)
     name_[-len(grp_):]=grp_
     grp_="".join(name_)
     algrp_.append(grp_)

     if bavard : print('creating group ',grp_,' for elements in front of line ',str(g_))
     
     # pour chaque segment de la ligne on cherche les elements d'appui
     # et suivant l'orientation du cote on change par les noeuds dupliques
     # adj_={} 
     for ele_ in line_:
        
       i_=body.bulks[ele_].connectivity[0]  
       j_=body.bulks[ele_].connectivity[1]
       if bavard : print('segment ',i_,j_)
       
       # les nouveaux noeuds
       ni_ = i_ 
       nj_ = j_

       # recherche elements de chaque cote du segment
       inter_ = set(node2ele_[i_]).intersection(node2ele_[j_])

       if len(inter_) == 0 :
         showError('crackMeshedAvatar2D::edge ',i_,' and ',i_,' without supporting element is impossible ')

       if len(inter_) > 2 :
         showError('crackMeshedAvatar2D::edge ',i_,' and ',i_,' with more than 2 supporting elements is impossible ')

       if bavard :  
         print('edge ',i_,' -> ',j_,' shared by ',len(inter_),' elements ')
         for e_ in inter_:
           print(e_.number,' connectivity ', e_.connectivity[:])

       # on duplique si bien 2 elements en vis a vis  
       if len(inter_) == 2 :
         e1_ = inter_.pop()
         e2_ = inter_.pop()

         if bavard :
           print('initial elements')
           print('e1_',e1_.connectivity)
           print('e2_',e2_.connectivity)
         
         try :          
           idi_= e1_.connectivity.index(i_)
         except: 
           idi_= e1_.connectivity.index(n2nn_[i_])
           
         # si i_,j_ se suivent dans e1_ on change e2_ (sens j_,i_)
         if j_ == e1_.connectivity[(idi_+1)%len(e1_.connectivity)] :
           if bavard : print('modifying e2_') 

           if i_ in lines_internal_nodes_[g_] :
             # peut avoir ete change ... 
             try :
               idi_ =  e2_.connectivity.index(i_)
               e2_.connectivity[idi_] = n2nn_[i_]
               ni_ = n2nn_[i_]
             except :
               idi_ = e2_.connectivity.index(n2nn_[i_])
               # on ne touche pas ni_ puisque deja modifie
             
             # on cherche l element voisins de e2_ par bord i_-> i_+1
             ii_ = e2_.connectivity[(idi_+1)%len(e2_.connectivity)]
             # si noeud ii_ deja modifie on recupere le precedent
             if ii_ > nbn_ : ii_ = nn2n_[ii_]
             inter_ = set(node2ele_[ii_]).intersection(node2ele_[i_])
             # len(inter)==1 c est e2
             if len(inter_) == 2 :
               e3_= inter_.pop()
               if e3_.number == e2_.number : e3_ = inter_.pop()
               if bavard : print('neighbour before ',e3_.connectivity)               
               # on fait le try car le noeud pourrait avoir deja ete change
               try :
                 idi_ = e3_.connectivity.index(i_)
                 e3_.connectivity[idi_] = n2nn_[i_]
               except:
                 if bavard : print(i_,' already changed in ', n2nn_[i_]) 
                 pass
               if bavard : print('neighbour after',e3_.connectivity)                             
             elif len(inter_) > 2 or len(inter) == 0:
               showError('crackMeshedAvatar2D::burp e2_ i_-> i_+1')
               
           if j_ in lines_internal_nodes_[g_] :
             # peut avoir ete change ... 
             try :
               idj_ =  e2_.connectivity.index(j_)                            
               e2_.connectivity[idj_] = n2nn_[j_]
               nj_ = n2nn_[j_]               
             except:
               idj_ = e2_.connectivity.index(n2nn_[j_])
               # on ne touche pas ni_ puisque deja modifie
               
             # on cherche l element voisins de e2_ : j_-1 -> j_
             jj_ = e2_.connectivity[(idj_-1)%len(e2_.connectivity)]
             # si noeud deja modifie on recupere le precedent
             if jj_ > nbn_ : jj_ = nn2n_[jj_]
             inter_ = set(node2ele_[jj_]).intersection(node2ele_[j_])
             # len(inter)==1 c est e2             
             if len(inter_) == 2 :
               e3_= inter_.pop()
               if e3_.number == e2_.number : e3_ = inter_.pop()
               if bavard : print('neighbour before',e3_.connectivity)               
               try : 
                 idj_ = e3_.connectivity.index(j_)
                 e3_.connectivity[idj_] = n2nn_[j_]
               except :
                 if bavard : print(j_,' already changed in ', n2nn_[j_])                   
                 pass
               if bavard : print('neighbour after',e3_.connectivity)               
             elif len(inter_) > 2 or len(inter) == 0 :
               showError('crackMeshedAvatar2D::burp e2_ j-1 -> j_') 
             
         # sinon on change e1_  
         else :
           if bavard : print('modifying e1_')             

           if i_ in lines_internal_nodes_[g_] :
             e1_.connectivity[idi_] = n2nn_[i_]
             ni_=n2nn_[i_]
             
             # on cherche l element voisins de e1_ : i_-1 -> i_
             ii_ = e1_.connectivity[(idi_+1)%len(e1_.connectivity)]
             # si noeud deja modifie on recupere le precedent
             if ii_ > nbn_ : ii_ = nn2n_[ii_]
             inter_ = set(node2ele_[ii_]).intersection(node2ele_[i_])
             if len(inter_) == 2 :
               e3_= inter_.pop()
               if e3_.number == e1_.number : e3_ = inter_.pop()
               if bavard : print('neighbour before ',e3_.connectivity)               
               try :
                 idi_ = e3_.connectivity.index(i_)
                 e3_.connectivity[idi_] = n2nn_[i_]
               except :
                 if bavard : print(i_,' already changed in ', n2nn_[i_])                   
                 pass
               if bavard : print('neighbour after ',e3_.connectivity)                             
             elif len(inter_) > 2 or len(inter) == 0 :
               showError('crackMeshedAvatar2D::burp e1_ i_-1 -> i_') 
           
           if j_ in lines_internal_nodes_[g_] :
             try: 
               idj_ =  e1_.connectivity.index(j_)
               e1_.connectivity[idj_] = n2nn_[j_]
               nj_=n2nn_[j_]
             except:
               idj_ = e1_.connectivity.index(n2nn_[j_])
               # on ne touche pas nj_ car deja change
               
             # on cherche l element voisins de e2_ : j_ -> j_+1
             jj_ = e1_.connectivity[(idj_-1)%len(e1_.connectivity)]
             # si noeud deja modifie on recupere le precedent
             if jj_ > nbn_ : jj_ = nn2n_[jj_]
             inter_ = set(node2ele_[jj_]).intersection(node2ele_[j_])
             if len(inter_) == 2 :
               e3_= inter_.pop()
               if e3_.number == e1_.number : e3_ = inter_.pop()
               if bavard : print('neighbour before ',e3_.connectivity)
               try : 
                 idj_ = e3_.connectivity.index(j_)
                 e3_.connectivity[idj_] = n2nn_[j_]
               except :
                 if bavard :  print(j_,' already changed in ', n2nn_[j_])                                     
                 pass
               if bavard : print('neighbour after ',e3_.connectivity)                             
             elif len(inter_) > 2 or len(inter) == 0 :
               showError('crackMeshedAvatar2D::burp e1_ j_->j+1') 

         seg = element(elem_dim=1,
                       connectivity=[nj_,ni_],
                       physicalEntity=grp_)
         body.addBulk(seg)

         if bavard :
           print('after')
           print('e1_',e1_.connectivity)
           print('e2_',e2_.connectivity)
           print('-----')

   # histoire que les nouveaux noeuds appartiennent a un groupe
   body.defineGroups()
         
   # pour stocker les elements antagonistes
   allines_={}
   for grp_ in algrp_ :
     if grp_ in body.groups.keys():  
       for bulk_ in body.groups[grp_].bulks:
          if bulk_.nbNodes != 2 :
            showError('crackMeshedAvatar2D::dtc') 
          allines_.setdefault(grp_,[]).append(bulk_.number)
   if bavard :     
     for k_,v_ in allines_.items():
        print('group ',k_,' contains ',v_)
      
   if bavard : print('--- managing corner nodes ---')
   # on construit en chaque coin les ensembles de noeuds connexes (les parts_)
   for corner_,clines_ in corners_.items():
     if bavard : print('corner node ',corner_) 
     nbe_ = len(node2ele_[corner_])
     parts_={}           
     # pour savoir qui on a deja garde
     etag_= numpy.zeros(nbe_)
     # un set de recherche
     tosearch_=set()
     # on recherche un noeuveau point de depart
     for id1_,e1_ in enumerate(node2ele_[corner_]):
       if etag_[id1_] != 0 : continue
       # celui la est vu
       etag_[id1_] = 1
       # il devient la clef d'une part
       parts_[e1_] = []
       # il devient le point de depart de la recherche
       tosearch_.add(e1_)
       
       # on cherche ses voisins ou les voisins de ses voisins
       while len(tosearch_) != 0 :
         e_actif_ = tosearch_.pop()
         # recherche parmis tous
         for id2_,e2_ in enumerate(node2ele_[corner_]) :
            # si deja vu on passe
            if etag_[id2_] != 0 : continue
            # combien de noeuds communs ?
            inter_ = set(e_actif_.connectivity).intersection(e2_.connectivity)
                 
            # cas pourri
            if len(inter_) == 0 or len(inter_) > 2 :
              showError('crackMeshedAvatar2D:: in corner node element should share 1 or 2 nodes')
                 
            # partage un bord on ajoute a part_ et on le met dans le set de recherche
            if len(inter_) == 2 :
              parts_[e1_].append(e2_)
              # declare vu
              etag_[id2_] = 1
              # point de depart de recherche
              tosearch_.add(e2_)

     if bavard : print('number of parts ',len(parts_))

     # on cherche les elements cl et al qui rayonnent sur le coin
     #... cl
     if bavard : print('initial 1D elements')
     adje_=[]
     for line_ in clines_:
        for ele_ in lines_[line_]:
          if body.bulks[ele_].connectivity[0] == corner_ or body.bulks[ele_].connectivity[1] == corner_ :
             adje_.append(ele_)
     if bavard :
       print(adje_)
       for seg_ in adje_:
          print(body.bulks[seg_].connectivity)
      
     #... al
     if bavard : print('opposite 1D elements ')     
     aladje_=[]
     for grp_ in algrp_:
        if grp_ in allines_.keys():
          for ele_ in allines_[grp_]:
            if body.bulks[ele_].connectivity[0] == corner_ or body.bulks[ele_].connectivity[1] == corner_ :
               aladje_.append(ele_)
     if bavard :         
       print(aladje_)
       for seg_ in aladje_:
          print(body.bulks[seg_].connectivity)
        
     nbp_=0
     for k_,v_ in parts_.items():
        nbp_+=1
        if bavard : print('part',nbp_,'has root element ',k_.number,'which is linked to ',len(v_),' elements')
        if nbp_ == 1 : continue
        coor_ = body.nodes[corner_].coor
        nb_=len(body.nodes)
        body.addNode(node(coor=coor_,number=nb_+1))
        nb_+=1
        body.nodes[nb_].dof = body.nodes[corner_].dof

        # on modifie les segments
        for seg_ in adje_ :
          # avec root 
          inter_ = set(k_.connectivity).intersection(body.bulks[seg_].connectivity)
          # le segment est bien appuye a cet element
          if len(inter_) == 2 :
            id_ = body.bulks[seg_].connectivity.index(corner_)             
            body.bulks[seg_].connectivity[id_] = nb_
          # avec les autres
          for e_ in v_:
            inter_ = set(e_.connectivity).intersection(body.bulks[seg_].connectivity)
            # le segment est bien appuye a cet element
            if len(inter_) == 2 :
              id_ = body.bulks[seg_].connectivity.index(corner_)             
              body.bulks[seg_].connectivity[id_] = nb_
            
        # on modifie les segments antagonistes
        for seg_ in aladje_ :
          inter_ = set(k_.connectivity).intersection(body.bulks[seg_].connectivity)
          # print(k_.connectivity,len(inter_))
          # le segment est bien appuye a cet element
          if len(inter_) == 2 :
            id_ = body.bulks[seg_].connectivity.index(corner_)             
            body.bulks[seg_].connectivity[id_] = nb_
          # avec les autres
          for e_ in v_:
            inter_ = set(e_.connectivity).intersection(body.bulks[seg_].connectivity)
            # print(e_.connectivity,len(inter_))            
            # le segment est bien appuye a cet element
            if len(inter_) == 2 :
              id_ = body.bulks[seg_].connectivity.index(corner_)             
              body.bulks[seg_].connectivity[id_] = nb_

        # on modifie les elements     
        # on modifie root         
        id_ = k_.connectivity.index(corner_)
        k_.connectivity[id_] = nb_
        # puis les autres elements de parts_
        for e_ in v_:
          id_ = e_.connectivity.index(corner_)
          e_.connectivity[id_] = nb_

          
   # histoire que les nouveaux noeuds appartiennent a un groupe
   body.defineGroups()
          

   # body.addContactors(group=crackgroup,shape='CLxxx',color=color,weights=w, reverse='yes')

   # roloc = list(color)
   # roloc = "".join(roloc[::-1])
   # for grp_ in algrp_:
   #   body.addContactors(group=grp_,shape='ALpxx',color=roloc,reverse='yes') 
   
   return body,algrp_

# fonction qui prend un maillage 2D et l'eclate en rigides (polygones)
def rigidsFromMesh2D(surfacic_mesh, model, material, color='BLUEx', reverse=False, shrink=0.):
   """rigidsFromMesh2D(surfacic_mesh, model, material, color='BLUEx', reverse=False, shrink=0.):

   this function build a set of rigids from a 2D mesh, each rigid
   is a polygon made from an element of the given mesh

   parameters:

   - surfacic_mesh: a 2D mesh
   - model: a given model
   - material: a given material

   optional parameter:

   - color='BLUEx': color of the polygon contactors
   - reverse=False: reverse=True iff the elements need to be reversed
   - shrink=0.: 
   """
   # on verifie que l'utilisateur a bien donne un maillage
   if not isinstance(surfacic_mesh, mesh):
      # si ce n'est pas le cas, on affiche un message d'erreur
      showError("the given surfacic mesh is not a mesh!")

   # on verifie que le maillage est bien 2D
   if surfacic_mesh.dimension != 2:
      # si ce n'est pas le cas, on affiche un message d'erreur
      showError("the given mesh is not a surfacic mesh")

   # on declare un container d'avatars pour stocker les rigides
   bodies=avatars()

   # pour chaque element du maillage
   for bulk in surfacic_mesh.bulks:
      # si l'element n'est pas un triangle a 3 noeuds ou un quadrangle a quatre noeuds
      if bulk.etype != 'T3xxx' and bulk.etype != 'Q4xxx':
         # on passe au suivant
         continue

      # on cree un avatar pour le nouveau rigide
      body=avatar(dimension=2)
      # on cree un comportement volumique de type rigide
      body.addBulk( rigid2d() )
      # on place le centre d'inertie a l'origine (recalcul de la position par la suite)
      body.addNode( node( coor=numpy.zeros(2), number=1) )
      # on definit les groupes sur le corps
      body.defineGroups()
      # on affecte son modele au corps
      body.defineModel(model=model) 
      # on affecte son materiau au corps
      body.defineMaterial(material=material)
      
      # definition du contacteur polygone
 
      # on cree une liste pour stocker les coordonnees des neouds de l'element courant
      l_coor=[]
      if shrink <= 0.:
        # pour chaque noeud de l'element
        for num in bulk.connectivity:
           # on ajoute la position du noeud a la liste
           l_coor.append(surfacic_mesh.nodes[num].coor)

      else :
        # pour chaque cote de l'element on met 2 noeuds
        for i,num in enumerate(bulk.connectivity):
           ib = bulk.connectivity[i]
           ie = bulk.connectivity[(i+1)%len(bulk.connectivity)]
           #print ib,ie
           coorb = surfacic_mesh.nodes[ib].coor
           coore = surfacic_mesh.nodes[ie].coor
           #print coorb,coore
           v = coore - coorb
           #print v

           shrk = (numpy.random.random()*(1.-0.95) + 0.95)*shrink  
           # on ajoute la position du noeud a la liste
           coor = coorb + shrk*v
           #print coor
           l_coor.append(coor)
           # on ajoute la position du noeud a la liste
           coor = coorb + (1.-shrk)*v
           #print coor
           l_coor.append(coor)
              
      # si on doit retourner l'element pour que son orientation soit correcte
      if reverse:
         # on inverse la liste des coordonnees des noeuds de l'element
         l_coor.reverse()
      # on ajoute son contacteur polygone au corps
      body.addContactors(shape='POLYG', color=color, nb_vertices=len(l_coor), vertices=numpy.array(l_coor))

      # on calcule de la surface et de l'inertie du corps
      body.computeRigidProperties()

      # on ajoute le corps rigide a la liste des corps
      bodies.addAvatar(body)

   # on renvoie la liste des corps generee
   return bodies

# fonction qui prend un maillage 2D et cree un corps rigide comme un cluster de polygones (chaque maille devenant un contacteur)
def rigidFromMesh2D(surfacic_mesh, model, material, color='BLUEx', reverse=False):
   """rigidFromMesh2D(surfacic_mesh, model, material, color='BLUEx', reverse=False):

   this function build a rigid from a 2D mesh, each contactor
   is a polygon made from an element of the given mesh

   parameters:

   - surfacic_mesh: a 2D mesh
   - model: a given model
   - material: a given material

   optional parameter:

   - color='BLUEx': color of the polygon contactors
   - reverse=False: reverse=True iff the elements need to be reversed
   """
   # on verifie que l'utilisateur a bien donne un maillage
   if not isinstance(surfacic_mesh, mesh):
      # si ce n'est pas le cas, on affiche un message d'erreur
      showError("the given surfacic mesh is not a mesh!")

   # on verifie que le maillage est bien 2D
   if surfacic_mesh.dimension != 2:
      # si ce n'est pas le cas, on affiche un message d'erreur
      showError("the given mesh is not a surfacic mesh")

   # on cree un avatar pour le nouveau rigide
   body=avatar(dimension=2)
   # on cree un comportement volumique de type rigide
   body.addBulk( rigid2d() )
   # on place le centre d'inertie a l'origine (recalcul de la position par la suite)
   body.addNode( node( coor=numpy.zeros(2), number=1) )
   # on definit les groupes sur le corps
   body.defineGroups()
   # on affecte son modele au corps
   body.defineModel(model=model) 
   # on affecte son materiau au corps
   body.defineMaterial(material=material)
 
   # pour chaque element du maillage
   for bulk in surfacic_mesh.bulks:
      # si l'element n'est pas un triangle a 3 noeuds ou un quadrangle a quatre noeuds
      if bulk.etype != 'T3xxx' and bulk.etype != 'Q4xxx':
         # on passe au suivant
         continue
     
      # definition d'un nouveau contacteur polygone
 
      # on cree une liste pour stocker les coordonnees des neouds de l'element courant
      l_coor=[]
      # pour chaque noeud de l'element
      for num in bulk.connectivity:
         # on ajoute la position du noeud a la liste
         l_coor.append(surfacic_mesh.nodes[num].coor)
      # si on doit retourner l'element pour que son orientation soit correcte
      if reverse:
         # on inverse la liste des coordonnees des noeuds de l'element
         l_coor.reverse()
      # on ajoute son contacteur polygone au corps
      body.addContactors(shape='POLYG', color=color, nb_vertices=len(l_coor), vertices=numpy.array(l_coor))

   # on calcule de la surface et de l'inertie du corps
   body.computeRigidProperties()

   # on renvoie le corps generee
   return body

# fonction qui extrait le contour libre d'un maillage 2D surfacique, sous la forme d'un maillage 2D lineique
# N.B.: les numeros des noeuds du maillage surfacique ne sont pas renumerotes
def extractContour(given_mesh):
   """extractContour(given_m):

   this function computes and returns the contour of a surfacic mesh, as a
   lineic mesh.

   N.B.: this function handles triangles and quadrilaterals

   parameters:

   - given_mesh: the given mesh

   returned value: the built lineic mesh
   """
   # si le maillage donne n'est pas un maillage
   if not isinstance(given_mesh, mesh):
      # on affiche un message d'erreur
      showError("the given mesh is not a mesh")

   # si le maillage n'est pas defini avec des noeuds a deux dimensions
   if given_mesh.dimension != 2:
      # ca ne peut pas etre un maillage surfacique !
      # on affiche un message d'erreur 
      showError("the given mesh is not 2D !")

   # numerotation des elements

   # on enumere les elements
   for num, bulk in enumerate(given_mesh.bulks):
      # on affecte l'indice courant a l'element courant
      bulk.number=num

   # construction des listes d'adjacence de chaque noeud

   # on definit un container pour stocker les listes d'adjacence de chaque noeud 
   l_node2ele = {}
   # pour chaque noeud du corps, on intialise une liste d'adjacence vide
   for nod in given_mesh.nodes:
      l_node2ele[nod.number]=[]
   # pour chaque element du maillage 
   for bulk in given_mesh.bulks:
      # pour chaque noeud de l'element
      for n in bulk.connectivity:
         # on ajoute l'element courant dans la liste d'adjacence du noeud courant
         l_node2ele[n].append(bulk)
 
   ###for nod in given_mesh.nodes:
   ###  print 'noeuds ',nod.number,' nombre de voisins ',len(l_node2ele[nod.number])

       
   # extraction de la surface libre
   
   # on definit la liste du nombre de bords n'appartenant pas au contour pour chaque element 
   nb_non_free_faces=numpy.zeros(len(given_mesh.bulks), 'i')
   # on definit le dictionnaire qui indique pour chaque element, identifie par son numero, quel bord est libre
   is_free_face={}
   # pour chaque element
   for bulk in given_mesh.bulks:
      # si l'element est un element surfacique
      if bulk.etype in dimension2geoElement[2]:
         # si l'element est un triangle
         if bulk.etype == 'T3xxx': # ca pourrait etre T6xxx aussi ...
            # on suppose que tous les bords sont sur le contour
            is_free_face[bulk.number]=[True, True, True]

            # on recupere les numeros des noeuds du tetraedre
            i1=bulk.connectivity[0]
            i2=bulk.connectivity[1]
            i3=bulk.connectivity[2]

            ###print 'on teste ele',bulk.number,' noeuds ',i1,i2,i3

            # on teste d'abord les faces qui portent le noeud i1 de l'element courant
            # pour chaque element adjacent au noeud i1
            for adj_bulk in l_node2ele[i1]:
               ###print adj_bulk.number, adj_bulk.type, adj_bulk.connectivity

               # si l'element adjacent courant est l'element courant
               if adj_bulk.number == bulk.number:
                  # on passe au suivant
                  continue

               # si l'element courant n'est pas un element surfacique
               if not adj_bulk.etype in dimension2geoElement[2]:
                  # on passe au suivant
                  continue

               ###print  'voisin ',adj_bulk.number,' noeuds ',adj_bulk.connectivity
               # si le bord 0 {i1, i2} fait partie de l'adjacent
               if i2 in adj_bulk.connectivity :
                  # on vire 
                  is_free_face[bulk.number][0]=False
                  # on incremente le nombre de bords de l'ele qui n'appartiennent pas au contour
                  nb_non_free_faces[bulk.number] += 1

               # si le bord 2 {i3,i1} fait partie de l'adjacent
               if i3 in adj_bulk.connectivity:
                  # on vire 
                  is_free_face[bulk.number][2]=False
                  # on incremente le nombre de bords de l'ele qui n'appartiennent pas au contour 
                  nb_non_free_faces[bulk.number] += 1

            # pour chaque element adjacent au noeud i2
            for adj_bulk in l_node2ele[i2]:
               # si l'element adjacent courant est l'element courant
               if adj_bulk.number == bulk.number:
                  # on passe au suivant
                  continue
               # si l'element courant n'est pas un element surfacique
               if not adj_bulk.etype in dimension2geoElement[2]:
                  # on passe au suivant
                  continue

               # si le bord 1 {i2, i3} fait partie de l'adjacent
               if i3 in adj_bulk.connectivity:
                  # on vire 
                  is_free_face[bulk.number][1]=False
                  # on incremente le nombre de bords de l'ele qui n'appartiennent pas au contour
                  nb_non_free_faces[bulk.number] += 1

         elif bulk.etype == 'Q4xxx':

            # on suppose que tous les bords sont sur le contour
            is_free_face[bulk.number]=[True]*4
            # on recupere les numeros des noeuds
            i1=bulk.connectivity[0]
            i2=bulk.connectivity[1]
            i3=bulk.connectivity[2]
            i4=bulk.connectivity[3]

            # pour chaque element adjacent au noeud i1
            for adj_bulk in l_node2ele[i1]:
               # si l'element adjacent courant est l'element courant
               if adj_bulk.number == bulk.number:
                  # on passe au suivant
                  continue
                              
               # si l'element courant n'est pas un element surfacique
               if not adj_bulk.etype in dimension2geoElement[2] :
                  continue

               # si le bord 0 {i1, i2} fait partie de l'adjacent
               if i2 in adj_bulk.connectivity:
                  # on vire
                  is_free_face[bulk.number][0]=False
                  # on incremente le nombre de bords de l'ele qui n'appartiennent pas au contour
                  nb_non_free_faces[bulk.number] += 1
                  
               # si le bord 3 {i4, i1} fait partie de l'adjacent
               if i4 in adj_bulk.connectivity :
                  # on vire
                  is_free_face[bulk.number][3]=False
                  # on incremente le nombre de bords de l'ele qui n'appartiennent pas au contour
                  nb_non_free_faces[bulk.number] += 1

            # pour chaque element adjacent au noeud i3
            for adj_bulk in l_node2ele[i3]:
               # si l'element adjacent courant est l'element courant
               if adj_bulk.number == bulk.number:
                  # on passe au suivant
                  continue

               # si l'element courant n'est pas un tetraedre
               if not adj_bulk.etype in dimension2geoElement[2]:
                  # on passe au suivant
                  continue

               # si le bord 1 {i2, i3} fait partie de l'adjacent
               if i2 in adj_bulk.connectivity :
                  # on vire
                  is_free_face[bulk.number][1]=False
                  # on incremente le nombre de bords de l'ele qui n'appartiennent pas au contour
                  nb_non_free_faces[bulk.number] += 1

               # si le bord 2 {i3, i4} fait partie de l'adjacent courant
               if i4 in adj_bulk.connectivity:
                  # on vire
                  is_free_face[bulk.number][2]=False
                  # on incremente le nombre de bords de l'ele qui n'appartiennent pas au contour
                  nb_non_free_faces[bulk.number] += 1

         # si l'element n'est pas gere
         else:
            ## on quitte le pgm
            msg  = "no contour can be built from this mesh, since it involves unhandled elements!"
            msg += bulk.etype + ' is not supported'
            showError(msg)

   # ici, on est sur que tous les elements du maillage sont pris en charge

   # on declare un maillage pour stocker le maillage du contour
   # N.B.: le maillage est defini avec des noeuds a deux dimensions, meme s'il ne contient que des elements lineiques

   contour=mesh(dimension=2)

   # recuperation des elements du contour

   # pour chaque element
   for bulk in given_mesh.bulks:
      # si element surfacique 
      if bulk.etype in dimension2geoElement[2]:
         ###print nb_non_free_faces[bulk.number]
         # si l'element est un tetraedre
         if bulk.etype == 'T3xxx':
            nbn=3 # 0->2
            # si aucun bord libre
            if nb_non_free_faces[bulk.number] == 3:
               # on passe a l'element suivant
               continue
         elif bulk.etype == 'Q4xxx':
            nbn=4 # 0->3
            # si aucun bord libre
            if nb_non_free_faces[bulk.number] == 4:
               # on passe a l'element suivant
               continue
         else:
            # connait pas
            # on passe a l'element suivant
            continue

         for i in range(0, len(is_free_face[bulk.number])):                       
           # si la face courante est libre
           if is_free_face[bulk.number][i]:
             ###print bulk.number,bulk.etype,nbn,' bord ',i,(i+1)%nbn
             # on recupere la connectivite de la face
             bord=[ bulk.connectivity[i] , bulk.connectivity[(i+1)%nbn]]
             # on ajoute le bord au contour
             contour.addBulk( element(elem_dim=1, connectivity=bord) )
                      
   # recuperation des noeuds du contour

   # on declare la liste des numeros de noeuds du contour
   free_node_numbers=[]
   # pour chaque bord du contour
   for bord in contour.bulks:
      # pour chaque noeud de la table de connectivite de la face
      for number in bord.connectivity:
         # si le noeud n'a pas deja ete visite
         if not number in free_node_numbers:
            # on ajoute le numero du noeud courant a la liste des numeors de noeuds de la surface libre
            free_node_numbers.append(number)
            # on ajoute une copie du noeud au maillage de la surface libre
            contour.addNode(deepcopy(given_mesh.nodes[number]))

   ###sys.exit()
   # on renvoie le maillage du contour
   return contour

