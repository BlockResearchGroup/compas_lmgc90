# module definissant une classe permettant de manipuler un maillage 2D ou 3D
import numpy as np
from copy import deepcopy
from ..avatar.bulks import *
from ..avatar.nodes import *
from ..avatar.bulk.element import *

from ..utilities.error import *

# classe maillage
class mesh():
   """class mesh():

   this class defines a mesh object, as a couple of a nodes set and a bulk set.
   """

   # constructeur
   def __init__(self, dimension):
      """__init__(self, dimension):

      this function initializes a new mesh

      parameters:

      - dimension: spatial dimension (2, in 2D, 3 in 3D); used to know the number of coordinates for a node
      """
      # on stocke la dimension consideree
      self.dimension = dimension
      # on initialise l'ensemble de noeuds du maillage
      self.nodes = nodes() 
      # on initialise l'ensemble d'elements du maillage
      self.bulks = bulks() 

   # fonction qui ajoute un noeud au maillage
   def addNode(self, noeud):
       """addNode(noeud):

       this function add a node to the mesh

       parameters:

       - self: the mesh itself
       - noeud: a given node
       """
       # test paranoiaque de la dimension 
       if np.size(noeud.coor) == self.dimension:
          self.nodes.addNode(noeud)
       else:
          showError('incompatible size between coor array and dimension')

   # fonction qui ajoute un element au maillage
   def addBulk(self, ele):
       """addBulk(ele):

       this function add an element to the mesh

       parameters:

       - self: the mesh itself
       - ele: a given element
       """
       # on ajoute dans l'element le container de bulks du maillage
 
       self.bulks.addBulk(ele)

   def hasGroup(self, group):
     """
     Check if at least one element to input group name
     """
     for b in self.bulks:
       if group == b.physicalEntity:
         return True
     return False
       

   # remove nodes not attached to elements
   def removeFreeNodes(self):
      """ removeFreeNodes(self)
        removes nodes not attached to an element
      """ 
      
      busynodes = {}

      # pour chaque element
      for bulk in self.bulks:
         # pour chaque noeud de la table de connectivite on cree une entree
         for i in range(0, len(bulk.connectivity)):
           if bulk.connectivity[i] not in busynodes:
             busynodes[bulk.connectivity[i]]=1
      
      # # pour chaque noeud
      # for nod in self.nodes:
      #    # on vire ceux non attaches a un element
      #    if nod.number not in busynodes:
      #      del self.nodes[nod.number]

      burk=[]
      # pour chaque noeud
      for nod in self.nodes:
         # on fait une liste des noeuds a virer
         if nod.number not in busynodes:
           burk.append(nod.number)  

      for idn in burk:
         # on vire ceux non attaches a un element
         del self.nodes[idn]  
      

   # methode qui renumerote les noeuds du maillage, pour que les numeros utilises soient [1, ..., nbNodes]
   # rq de fd: l'objectif du renumbering est double: 
   #  - il se peut que tous les noeuds charges soient necessaires donc faut juste tasser les num dans les ele
   #  - il se peut aussi que certains noeuds soient inutiles (car on a lu un maillage lineaire 
   #    dans un maillage quadratique par exemple) dans ce cas il faut virer les noeuds inutiles et tasser la numerotation 

   def rankRenumbering(self):
      """rankRenumbering(self):

      this function renumbers the nodes of the mesh, in order to avoid holes in the numbering, i.e.
      nodes number are in [1, nbNodes]

      parameters:

      - self: the mesh itself
      """

      self.removeFreeNodes()

      # on construit le dictionnaire des rangs, attribuant a chaque numero de noeud, son rang dans l'ensemble des 
      # noeuds
      
      # on declare le dictionnaire des rangs
      ranks={}
      # on recupere la liste ordonnee des numeros de noeuds
      sorted_node_numbers=self.nodes.sortedKeys()
      # on enumere les noeuds
      for rank, num in enumerate(sorted_node_numbers):
         # on affecte l'indice courant, incremente de 1 (pour commencer la 
         # numerotaion a 1), au noeud courant

         # print(rank,num)         
         
         ranks[num]=rank + 1
   
      # on renumerote les noeuds, dans l'ensemble des noeuds
      # N.B.: on recree un nouveau container de noeud pour assurer que la clef du container
      #       de noeuds du maillage reste le numero du noeud, apres la renumerotation      
 
      # on cree un nouveau container de noeuds
      new_nodes=nodes()
      # pour chaque noeud
      for nod in self.nodes:
         # le numero du noeud courant devient son rang
         nod.number=ranks[nod.number]
         # on ajoute le noeud courant dans le nouveau container de noeuds
         new_nodes.addNode(nod)  
      # on attache le nouveau container de noeuds a l'objet maillage
      self.nodes=new_nodes

      # on renumerote les noeuds, dans l'ensemble des elements
   
      # pour chaque element
      for bulk in self.bulks:
         # pour chaque noeud de la table de connectivite
         for i in range(0, len(bulk.connectivity)):
            # le numero du noeud devient son rang
            # print(i,bulk.connectivity[i])
            # print(len(ranks))
            # print(ranks)
            # print(ranks[bulk.connectivity[i]])
            bulk.connectivity[i]=ranks[bulk.connectivity[i]]


   # fonction qui donne acces a des sous-maillage d'un maillage, suivant la valeur d'une entite
   def getSubMeshes(self, entity_type="geometricalEntity"): 
      """getSubMeshes(self, entity_type="geometricalEntity"):

      this function computes handles to sub-meshes of the given mesh and returns the 
      computed meshes. Elements of a mesh share a same physical or geometrical entity.
      N.B.: nodes and elements of the generated meshes are references of nodes and elements
      of the original mesh and not deep copies!

      parameters:

      - self: a given mesh

      optional parameters:

      - entity_type="geometricalEntity": give the entity type to consider to separate meshes, i.e.
        entity_type="geometricalEntity", if geometrical entities have to be used and 
        entity_type="physicalEntity", if physical entities have to be used

      returned values: a dictionnary mapping enities value on separated meshes.
      """
      # on delcare le dictionnaire utilise pour associer un maillage a chaque entite
      entity2subMesh={}

      # tri des elements suivant le type d'entite choisi (geometrique ou physique)
   
      # on declare la liste utilisee pour stocker les clefs du dictionnaire precedent
      known_entities=[]
      # pour chaque element
      for bulk in self.bulks:
         # on recupere l'entite consideree (physique ou geometrique) de l'element courant
         entity=getattr(bulk, entity_type)
         # si l'entite n'a pas encore ete rencontree
         if not entity in known_entities:
            # on cree un nouveau maillage associe a l'entite courante
            entity2subMesh[entity]=mesh(self.dimension)
            # on ajoute l'entite courante a la la liste des entites deja rencontrees
            known_entities.append(entity)
         # on ajoute une reference a l'element courant au maillage associe a l'entite courante
         entity2subMesh[entity].addBulk(bulk)
      # on trie la liste des entites
      known_entities.sort()
   
      # tri des noeuds, a partir des elements tries
   
      # on declare le dictionnaire utilise pour associer l'ensemble des numeros de noeuds a une entite
      entity2node_numbers={}
      # pour chaque entite
      for entity in known_entities:
         # on cree une liste de numeros de noeuds a associer a l'entite courante
         entity2node_numbers[entity]=[]
         # pour chaque element associe a l'entite courante
         for bulk in entity2subMesh[entity].bulks:
            # pour chaque noeud de la table de connectivite de l'element
            for number in bulk.connectivity:
               # si le noeud n'a pas deja ete associe a l'entite courante
               if not number in entity2node_numbers[entity]:
                  # on ajoute le numero du noeud courant a la liste des numeors de noeuds associes a l'entite
                  entity2node_numbers[entity].append(number)
                  # on ajoute une reference au noeud du maillage associe a l'entite courante
                  entity2subMesh[entity].addNode(self.nodes[number])
   
      # on renvoie le dictionnaire associant un maillage a chaque entite
      return entity2subMesh

   # fonction qui separe les differents maillages lus dans un meme fichier
   def separateMeshes(self, dim, entity_type="geometricalEntity", keep_all_elements=True): 
      """separateMeshes(self, dim, entity_type="geometricalEntity"):

      this function separates several meshes, stored in a single one (read from a mesh),
      and returns the extracted meshes, as meshes. Elements of a mesh share a same 
      physical or geometrical entity.

      parameters:

      - self: a given mesh
      - dim: dim=1 for linear mesh; dim=2 for surfacic meshes; dim=3 for volumic meshes

      optional parameters:

      - entity_type="geometricalEntity": give the entity type to consider to separate meshes, i.e.
        entity_type="geometricalEntity", if geometrical entities have to be used and 
        entity_type="physicalEntity", if physical entities have to be used
      - keep_all_elements=True: if keep_all_elements=True, all elements have to be sorted and returned,
        else only the considered elements (i.e. surfacic if dim=2 or volumic if dim=3) have to be sorted
        and returned.
        N.B.: elements of greater dimension than the considered one are ignored, e.g. if dim=2, volumic elements
        are neither sorted nor returned

      returned values: a dictionnary mapping entities value on separated meshes.
      """
      # on delcare le dictionnaire utilise pour associer un maillage a chaque entite
      entity2mesh={}

      print("Meshes separating:")
   
      # tri des elements de la dimension choisie (surfacique ou volumique) suivant le type d'entite
      # choisi (geometrique ou physique)
      print("   * Elements sorting")
   
      # on declare la liste utilisee pour stocker les clefs du dictionnaire precedent
      known_entities=[]
      # pour chaque element
      for bulk in self.bulks:
         # si l'element est de la dimension condideree
         if bulk.etype in dimension2geoElement[dim]:
            # on recupere l'entite consideree (physique ou geometrique) de l'element courant
            entity=getattr(bulk, entity_type)
            # si l'entite n'a pas encore ete rencontree
            if not entity in known_entities:
               # on cree un nouveau maillage associe a l'entite courante
               entity2mesh[entity]=mesh(self.dimension)
               # on ajoute l'entite courante a la la liste des entites deja rencontrees
               known_entities.append(entity)
            # on ajoute une copie de l'element courant au maillage associe a l'entite courante
            entity2mesh[entity].addBulk(deepcopy(bulk))
      # on trie la liste des entites
      known_entities.sort()
   
      # tri des noeuds, a partir des elements tries
      print("   * Nodes sorting")
   
      # on declare le dictionnaire utilise pour associer l'ensemble des numeros de noeuds a une entite
      entity2node_numbers={}
      # pour chaque entite
      for entity in known_entities:
         # on cree une liste de numeros de noeuds a associer a l'entite courante
         entity2node_numbers[entity]=[]
         # pour chaque element associe a l'entite courante
         for bulk in entity2mesh[entity].bulks:
            # pour chaque noeud de la table de connectivite de l'element
            for number in bulk.connectivity:
               # si le noeud n'a pas deja ete associe a l'entite courante
               if not number in entity2node_numbers[entity]:
                  # on ajoute le numero du noeud courant a la liste des numeors de noeuds associes a l'entite
                  entity2node_numbers[entity].append(number)
                  # on ajoute une copie du noeud au maillage associe a l'entite courante
                  entity2mesh[entity].addNode(deepcopy(self.nodes[number]))

         # now sorting nodes by keys
         tmp_nodes = entity2mesh[entity].nodes
         entity2mesh[entity].nodes = nodes()
         for k in sorted(tmp_nodes.keys()):
            entity2mesh[entity].addNode(tmp_nodes[k])

   
      # tri des elements restants a partir des noeuds tries
   
      # si tous les elements doivent etre tries
      if keep_all_elements:
         # on trie des elements restants a partir des noeuds tries
         print("   * Remaining elements sorting")
         
         # construction d'une map donnant la liste des entites associees a un noeud
         
         # on delcare le dictionnaire utilise pour associer a un numero de noeud la liste des entites auxquelles il appartient
         node_number2entities={}
         # on delcare la liste des numeros de noeuds deja parcourus
         known_node_numbers=[]
         # pour chaque entite
         for entity in known_entities:
            # pour chaque numero de noeud associe a l'entite
            for number in entity2node_numbers[entity]:
               # si on a pas encore associe une entite au numero de noeud courant
               if not number in known_node_numbers:
                  # on associe une dictionnaire vide au numero de noeud courant
                  node_number2entities[number]=[]
                  # on indique que le numero de noeud courant a ete parcouru
                  known_node_numbers.append(number)
               # on associe l'entite courante au numero de de noeud courant
               node_number2entities[number].append(entity)
   
         # tri des elements restants
   
         # pour chaque element
         for bulk in self.bulks:
            # si l'element courant est de dimension inferieure a celle consideree
            if geoElement2dimension[bulk.etype] < dim:
               # on recupere la connectivite de l'element courant
               connectivity=bulk.connectivity
               # on recupere le numero du premier noeud
               first_num=connectivity[0]
               # si le premier noeud de l'element n'est associe a aucune entite
               if not first_num in known_node_numbers:
                  # l'element ne pourra etre associe a aucune entite
                  # on affiche un warning et on passe au suivant
                  showWarning("current element cannot be associated to any entity!")
                  # et on passe au suivant
                  continue
               # pour chaque entite associee au premier noeud de l'element
               for entity in node_number2entities[first_num]:
                  # on suppose que l'element est associe a l'entite courante
                  is_associated=True
                  # pour chaque noeud de la connectivite de l'element courant, autre que le premier
                  for i in range(1, len(connectivity)):
                     # on recupere le numero du noeud courant
                     num=connectivity[i]
                     # si le noeud courant n'est associe a aucune entite
                     if not num in known_node_numbers:
                        # l'element ne pourra etre associe a aucune entite
                        is_associated=False
                        # on affiche un warning et on passe au suivant
                        showWarning("current element cannot be associated to any entity!")
                        # on sort de la boucle
                        break
                     # si l'entite courante n'est pas dans la liste des entites associees au numero de noeud courant
                     if not entity in node_number2entities[num]:
                        # l'element courant ne peut etre associe a l'entite courante
                        is_associated=False
                        # on sort de la boucle
                        break
                  # si l'element courant est associe a l'entite courante
                  if is_associated:
                     # on ajoute une copie de l'element courant au maillage associe a l'entite courante
                     entity2mesh[entity].addBulk(deepcopy(bulk)) 
   
      # on renvoie le dictionnaire associant un maillage a chaque entite
      return entity2mesh


   def computeNormal(self, n, e, reverse):
     """
     Compute normal of a node of an element
     """

     if self.bulks[e].etype == 'S2xxx':
       normal = np.zeros([self.dimension])
       if reverse:
         normal[0] =-self.nodes[self.bulks[e].connectivity[0]].coor[1]+self.nodes[self.bulks[e].connectivity[1]].coor[1]
         normal[1] = self.nodes[self.bulks[e].connectivity[0]].coor[0]-self.nodes[self.bulks[e].connectivity[1]].coor[0]
       else:
         normal[0] =-self.nodes[self.bulks[e].connectivity[1]].coor[1]+self.nodes[self.bulks[e].connectivity[0]].coor[1]
         normal[1] = self.nodes[self.bulks[e].connectivity[1]].coor[0]-self.nodes[self.bulks[e].connectivity[0]].coor[0]
     elif self.bulks[e].etype == 'T3xxx':
       if reverse:
         normal = np.cross( self.nodes[self.bulks[e].connectivity[1]].coor-self.nodes[self.bulks[e].connectivity[0]].coor,
                            self.nodes[self.bulks[e].connectivity[2]].coor-self.nodes[self.bulks[e].connectivity[0]].coor
                          )
       else:
         normal = np.cross( self.nodes[self.bulks[e].connectivity[2]].coor-self.nodes[self.bulks[e].connectivity[0]].coor,
                            self.nodes[self.bulks[e].connectivity[1]].coor-self.nodes[self.bulks[e].connectivity[0]].coor
                          )
     else:
       i = self.bulks[e].connectivity.index(n)
       k = len(self.bulks[e].connectivity)
       if reverse:
         normal = np.cross( self.nodes[self.bulks[e].connectivity[i-1]].coor-self.nodes[self.bulks[e].connectivity[i]].coor,
                            self.nodes[self.bulks[e].connectivity[(i+1)%k]].coor-self.nodes[self.bulks[e].connectivity[i]].coor
                          )
       else:
         normal = np.cross( self.nodes[self.bulks[e].connectivity[(i+1)%k]].coor-self.nodes[self.bulks[e].connectivity[i]].coor,
                            self.nodes[self.bulks[e].connectivity[i-1]].coor-self.nodes[self.bulks[e].connectivity[i]].coor
                          )

     return normal

   def extrudePhysicalEntity(self, pE, length, reverse=False):
      """
      Extrude a layer of elements.

      This function uses the nodes of a physical entity (line in 2D, surface en 3D)
      to create by extrusion the corresponding nodes and elements of the mesh. 
      Extrusion is performed along the normal to the initial mesh.
      Only linear elements are supported. 
      New physical entities are created with prefix 'E' for the extruded new elements and 'P'
      for the extruded new nodes.

      To work, the rankRenumbering function must have been called beforehand,
      and the elements correctly oriented. At this time only 'S2xxx','T3xxx' and 'Q4xxx'
      elements are supported.

      parameters:
      - pE : physical entity name to extrude from
      - length : size of extrusion along the computed normals
      """

      extr_map = { 'S2xxx':'Q4xxx',# 'S3xxx', 'Q8xxx',
                   'T3xxx':'PRI6x',# 'T6xxx', 'PRI15',
                   'Q4xxx':'H8xxx',# 'Q8xxx', 'H20xx'
                 }

      # list nodes and elements of the desired physical entity
      el_list = []; no_list = []
      for e, el in enumerate(self.bulks):
        if pE == el.physicalEntity and el.etype in dimension2geoElement[self.dimension-1]:
          el_list.append(e)
          for n in el.connectivity:
            if n not in no_list:
              no_list.append(n)

      # store node to elements maps
      n2b = []
      for nod in no_list:
         n2b.append([])
         for bul in el_list:
           if nod in self.bulks[bul].connectivity and self.bulks[bul].etype in list(extr_map.keys()) :
             n2b[-1].append(bul)

      # keeps the last node number to be able to create elements
      # connectivity... assumes rankRenumbering has been used
      node_shift = len(self.nodes)+1

      # extrude each node
      for i, no in enumerate(no_list):
        n = np.zeros(self.dimension)
        s = 0.
        for e in n2b[i]:
          normal = self.computeNormal( no, e , reverse)
          n += normal[:]
          s += np.linalg.norm(normal)
        n /= s
        self.addNode( node(coor=length*n+self.nodes[no].coor,
                           number=node_shift+no) )

      # create new elements:
      for el in el_list:
        elem = self.bulks[el]
        new_c = deepcopy(elem.connectivity)
        #perm top layer when extruding S2->Q4
        if len(new_c) == 2:
          new_c = new_c + [x+node_shift for x in new_c[::-1]]
        else:
          new_c = new_c + [x+node_shift for x in new_c]

        # new extruded element
        self.addBulk( element( geoElement2dimension[extr_map[elem.etype]], new_c,
                               'E'+elem.physicalEntity,
                               elem.geometricalEntity)
                    )
        # the projection of the original element along the normals
        self.addBulk( element( geoElement2dimension[elem.etype],
                               [x+node_shift for x in elem.connectivity],
                               'P'+elem.physicalEntity,
                               elem.geometricalEntity )
                    )

      self.rankRenumbering()

