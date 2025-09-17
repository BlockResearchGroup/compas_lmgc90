# MODULE lecture
# 
# Module permettant la lecture des differents formats
# des fichiers de maillages :
#     1: Gmsh
# Une fois le fichier lu, l ensemble des donnees est
# stockes dans des objets (Noeud,iterNoeuds,element,
# iterElement) qui sont definis dans 'nodes,bulks'.
#
# Pour Ajouter un format de lecture, il faut ajouter
# dans la fonction 'lecture' le type de fichier
# + creer la fonction d interface.
# 
# MODULES IMPORTES:
#     - os,string,sys
#     - gmsh2lmgc
# FONCTIONS:
#     - lecture(nom,repertoire_travail) => nodes_set,bulks_set
#     - lectureFichier(nom,extension,repertoire_travail) => lignes
#     - gmsh(nom,repertoire_travail) => nodes_set,bulks_set

import collections
import numpy
from copy import deepcopy

from ..config.readMeshFormat import *
from ..config.lmgc90dicts import *

from ..avatar.nodes        import *
from ..avatar.node.node    import *
from ..avatar.bulks        import *
from ..avatar.bulk.element import *

from .mesh import *

from ..utilities.error    import *

## routine de lecture du fichier maillage
def readMesh(name, dim, keep_elements=None, scale_factor=None, incomplete_elements=True):
    """readMesh(name, dim):

    this function builds sets of nodes and elements by reading a file
    wherein a mesh is stored

    parameters:

    - mesh_file_name: a string giving the nameof the file in which the mesh is stored
      supported file extensions are:

      * '.msh': for gmsh outputs
      * '.txt': for sysweld outputs
      * '.vtu': for vtk file
      * '.inp': for abaqus file

    - dim: dim=2, in 2D and dim=3, in 3D

    optional parameters:

    - keep_elements: this parameter is used to filter elements, if
      keep_elements=None, no filter used, else keep_elements is the list
      of elements types that can be stored in the built mesh, others will be
      forgotten
    - scale_factor=None: this parameter is used to rescale the read mesh
    - incomplete_elements: used to reduce the order of an element

    returned value:    

    - read_mesh: the built mesh

    """
   
    # filtre sur le type d'elements
    if keep_elements is None:
       keep_elements=[]                  
    else:
       # on recupere la liste des elements geometriques
       l_geo = list(geo2element.keys())
       # on enumere les elements de la liste des elements a conserver
       for i_ele, ele in enumerate(keep_elements):
          # si l'element n'est pas un element geometrique connu 
          if not ele in l_geo:
             # on affiche un warning
             showWarning('unhandled element cannot be considered!')
             # on supprime l'element de la liste
             keep_elements.pop(i_ele)

    # on choisit la methode pour le lire en fonction de sont type
    #   * cas du maillage maillage gmsh
    if name.endswith('msh'): 
        # check file format

        mesh_file = open(name, 'r')

        line = mesh_file.readline()

        # "MeshFormat" not on first line... then reopening the file for v1 reading
        if '$MeshFormat' not in line:
            mesh_file.close()
            mesh_file = open(name, 'r')
            read_mesh = readGmshv1(mesh_file, dim)
        else:
            # checking on next line if v2 or v4:
            line = mesh_file.readline()

            version, binary, dsize = line.split()

            if binary == '1' :
              showError('[readMesh]: gmsh binary files not supported')

            if version.startswith("2"):
                read_mesh = readGmshv2(mesh_file, dim, keep_elements, incomplete_elements)
                mesh_file.close() # beurk
            elif version.startswith("4"):
                major_minor = version.split('.')
                minor = int(major_minor[1]) if len( major_minor ) > 1 else 0
                read_mesh = readGmshv4(mesh_file, dim, minor, keep_elements, incomplete_elements)
                mesh_file.close() # beurk
            else:
                mesh_file.close() # beurk
                showError('[readMesh]: unknown version of gmsh file format')


    #   * cas du maillage maillage sysweld
    elif name.endswith('txt'):
         read_mesh = readSysweld(name, dim)
    #   * cas du maillage maillage au format MED
    #   * cas du maillage maillage au format VTK
    elif name.endswith('vtu'):
         read_mesh = readVtu(name, dim, keep_elements)
    elif name.endswith('inp'):
         read_mesh = readInp(name, dim, keep_elements)

    # si on a donne un facteur d'echelle
    if scale_factor != None:
       # pour chaque noeud du maillage lu
       for nod in read_mesh.nodes:
          # on met a l'echelle les coordonnees
          nod.coor *= scale_factor
      
    # on renvoie le maillage lu dans le fichier
    return read_mesh

def lecture(*args, **kwargs):
    msg = "[WARNING:DeprecationWarning] 'lecture' must be replaced by 'readMes'.\n"
    msg+= "                              This will raise an error in a later release"
    print(msg)
    return readMesh(*args, **kwargs)

def readGmshv1(mesh_file, dim):
      """readGmshv1(mesh_file, dim):

      this function builds sets of nodes and elements from a mesh
      built by gmsh, and using format 1.0

      parameters:

      - mesh_file: a file object coresponding to the file wherein the mesh is stored
        N.B.: the file is supposed to be open for reading
      - dim: dim=2, in 2D and dim=3, in 3D

      returned value:

      - read_mesh: the built mesh
      """
      
      # on declare la liste des mots clefs, qu'on s'attend a trouver dans le fichier
      motCleMesh = ('$NOD', '$ENDNOD', '$ELM', '$ENDELM')

      # on definit le maillage
      read_mesh = mesh(dim)

      # pour chaque ligne du fichier
      for ligne in mesh_file:
         # on separe les colonnes de la ligne courante
         res = ligne.split()
         # si la chaine de la premiere colonne est un mot-clef
         if res[0] in motCleMesh:
            # on le stocke 
            motcle=res[0]
         # si le dernier mot-clef lu est le debut de la section des noeuds,
         # et qu'on a pas atteint la fin de cette section
         # N.B.: le test 'len(res) > 1' permet de sauter la ligne donnant le nombre de noeuds
         if motcle == '$NOD' and res[0]!='$NOD' and len(res) > 1:
            # si on est en 2D
            if dim == 2:
               coor = numpy.array( [ float(res[1]), float(res[2]) ] )
            # si on est en 3D
            elif dim == 3:
               coor = numpy.array( [ float(res[1]), float(res[2]), float(res[3]) ] )
            # on ajoute le noeud lu au maillage
            read_mesh.addNode(node(coor=coor,number=int(res[0])))
         # si le dernier mot-clef lu est le debut de la section des elements,
         # et qu'on a pas atteint la fin de cette section
         # N.B.: le test 'len(res) > 1' permet de sauter la ligne donnant le nombre d'elements
         if motcle == '$ELM' and res[0]!='$ELM' and len(res) > 1 :
            # on lit le nouvel element et on l'ajoute au maillage
            if res[1] in gmshElementPoint:
              read_mesh.addBulk(element(elem_dim=0, connectivity=list(map(int,res[5:])), physicalEntity=res[2], number=int(res[0])))
            elif res[1] in gmshElementLine:
              read_mesh.addBulk(element(elem_dim=1, connectivity=list(map(int,res[5:])), physicalEntity=res[2], number=int(res[0])))
            elif res[1] in gmshElementSurface:
              read_mesh.addBulk(element(elem_dim=2, connectivity=list(map(int,res[5:])), physicalEntity=res[2], number=int(res[0])))
            elif res[1] in gmshElementVolume:
              read_mesh.addBulk(element(elem_dim=3, connectivity=list(map(int,res[5:])), physicalEntity=res[2], number=int(res[0])))
            else:
              showError("[readMesh::gmsh] : unknown gmsh element type: "+res[1])

      # on renvoie le maillage ainsi construit
      return read_mesh

def readGmshv2(mesh_file, dim, keep_elements, incomplete_elements):
      """readGmshv2(mesh_file, dim, keep_elements, incomplete_elements):

      this function builds sets of nodes and elements from a mesh
      built by gmsh, and using format 2.x

      parameters:

      - mesh_file: a file object coresponding to the file in wherein the mesh is stored
        N.B.: the file is supposed to be open for reading
      - dim: dim=2, in 2D and dim=3, in 3D

      optional parameters:

      - keep_elements: this parameter is used to filter elements, if
        keep_elements=None, no filter used, else keep_elements is the list
        of elements types that can be stored in the built mesh, other will be
        forgotten
      - incomplete_elements: used to reduce the order of an element

      returned value:

      - read_mesh: the built mesh
      """
      
      # on declare la liste des mots clefs, qu'on s'attend a trouver dans le fichier
      motCleMesh = ('$MeshFormat', '$Nodes', '$EndNodes', '$Elements', '$EndElements', \
                    '$PhysicalNames', '$EndPhysicalNames', '$EndMeshFormat')

      # on definit le dictionnaire associant un nom a certaines entites physiques
      physicalEntity2physicalName={}

      # on definit le maillage
      read_mesh = mesh(dimension=dim)

      # pour chaque ligne du fichier
      for ligne in mesh_file:
         # on separe les colonnes de la ligne courante
         res = ligne.split()
         # si la chaine de la premiere colonne est un mot-clef
         if res[0] in motCleMesh:
            # on le stocke 
            motcle=res[0]
         # si le dernier mot-clef lu est le debut de la section des noeuds,
         # et qu'on a pas atteint la fin de cette section
         # N.B.: le test 'len(res) > 1' permet de sauter la ligne donnant le nombre de noeuds
         if motcle == '$Nodes' and res[0]!='$Nodes' and len(res) > 1:
            # si on est en 2D
            if dim == 2:
               coor = numpy.array( [ float(res[1]), float(res[2]) ] )
            # si on est en 3D
            elif dim == 3:
               coor = numpy.array( [ float(res[1]), float(res[2]), float(res[3]) ] )
            # on ajoute le noeud lu au maillage
            read_mesh.addNode(node(coor=coor,number=int(res[0])))
         # si le dernier mot-clef lu est le debut de la section des elements,
         # et qu'on a pas atteint la fin de cette section
         # N.B.: le test 'len(res) > 1' permet de sauter la ligne donnant le nombre d'elements
         if motcle == '$Elements' and res[0]!='$Elements' and len(res) > 1 :
            # on lit le nouvel element et on l'ajoute a la liste des elements
 
            if res[1] in gmshv2ElementPoint:
              elem_dim = 0
            elif res[1] in gmshv2ElementLine:
              elem_dim = 1
            elif res[1] in gmshv2ElementSurface:
              elem_dim = 2
            elif res[1] in gmshv2ElementVolume:
              elem_dim = 3
            else:
              showError("[readMesh::readGmshv2] : unknown gmsh element type: "+res[1])

            # on recupere :
            #   * le nombre de tags (et on le passe en entier) 
            nb_tags = int(res[2])
            # s'il y a au moins un tag
            if nb_tags >= 1:
               #   * on peut recuperer le numero de "physical entity"
               pE = res[3]
            # s'il y a au moins deux tags
            if nb_tags >= 2:
               #   * on peut recuperer le numero de "geometrical entity"
               gE = res[4]
            #   * la connectivite de l'element (et on les numeros de noeuds en entiers)
            conn = list(map(int, res[3 + nb_tags:]))

            # si le filtrage des elements est active et que l'element ne fait pas
            # partie des elements conserves
            if keep_elements != [] and not elem_dim in keep_elements:
               # on passe au suivant
               continue

            if res[1] == '11': # TE10
               # on permute la numerotation des noeuds de l'element pour retrouver l'ordre choisi dans LMGC90 
               conn_TE10 = [conn[0], conn[1], conn[2], conn[3], conn[4],
                            conn[5], conn[6], conn[7], conn[9], conn[8]]

               # on construit l'element et on l'ajoute au maillage
               read_mesh.addBulk(element(elem_dim=elem_dim, connectivity=conn_TE10, physicalEntity=pE, geometricalEntity=gE, number=int(res[0])))

            if res[1] == '13' or (res[1]=='18' and incomplete_elements) :
               # on permute la numerotation des noeuds de l'element pour retrouver l'ordre choisi dans LMGC90
               conn_PRI15 = [conn[0] , conn[1] , conn[2], conn[3] , conn[4] ,
                             conn[5] , conn[6] , conn[9], conn[7] , conn[12],
                             conn[14], conn[13], conn[8], conn[10], conn[11]]

               # on construit l'element et on l'ajoute au maillage
               read_mesh.addBulk(element(elem_dim=elem_dim, connectivity=conn_PRI15, physicalEntity=pE, geometricalEntity=gE, number=int(res[0])))

            elif res[1] == '17' or (res[1]=='12' and incomplete_elements) :
               # on permute la numerotation des noeuds de l'element pour retrouver l'ordre choisi dans LMGC90 
               conn_H20 = [conn[0] , conn[1] , conn[2] , conn[3] , conn[4] ,
                           conn[5] , conn[6] , conn[7] , conn[8] , conn[11],
                           conn[13], conn[9] , conn[16], conn[18], conn[19],
                           conn[17], conn[10], conn[12], conn[14], conn[15]]

               # on construit l'element et on l'ajoute au maillage
               read_mesh.addBulk(element(elem_dim=elem_dim, connectivity=conn_H20, physicalEntity=pE, geometricalEntity=gE, number=int(res[0])))

            elif res[1] == '10' and incomplete_elements:

               # si l'element est un Q9 on lit un Q8
               conn_Q8 = [conn[0],  conn[1],  conn[2],  conn[3],  conn[4],  conn[5],  conn[6],  conn[7]]

               # on construit l'element et on l'ajoute au maillage
               read_mesh.addBulk(element(elem_dim=elem_dim, connectivity=conn_Q8, physicalEntity=pE, geometricalEntity=gE, number=int(res[0])))

               
            else:
               # si pas cas particulier
               # on utilise la connectivite de l'element telle qu'on l'a lue

               # pour le 2D on verifie que l'element est oriente suivant z sinon on retourne
               if dim == 2:
                 if len(conn) > 2:
                   #print conn
                   v1 = read_mesh.nodes[conn[1]].coor - read_mesh.nodes[conn[0]].coor
                   v2 = read_mesh.nodes[conn[-1]].coor - read_mesh.nodes[conn[0]].coor
                   #print v1
                   #print v2

                   xx =  v1[0]*v2[1] - v1[1]*v2[0]
                   #print xx 

                   if xx < 0.:
                     if len(conn)==6 :
                       conn0=deepcopy(conn)
                       conn = [conn0[0],conn0[2],conn0[1],conn0[5],conn0[4],conn[3]] 
                     else:
                       #print 'silence on tourne '+str(conn) 
                       conn.reverse()

                   #print '+',conn

               # on construit l'element et on l'ajoute au maillage

               read_mesh.addBulk(element(elem_dim=elem_dim, connectivity=conn, physicalEntity=pE, geometricalEntity=gE, number=int(res[0])))

         # si le dernier mot-clef lu est le debut de la section des noms des entites physiques,
         # et qu'on a pas atteint la fin de cette section
         # N.B.: le test 'len(res) > 1' permet de sauter la ligne donnant le nombre de noms d'entites physiques
         if motcle == '$PhysicalNames' and res[0]!='$PhysicalNames' and len(res) > 1 :
            # on lit le nom d'une nouvelle entite physique et on l'ajoute au dictionnaire associant un nom a
            # certaines entites physiques
 
            # on recupere :
            #   * la dimension de l'entite physique (inutilisee pour l'instant)
            physicalDim = int(res[0])
            #   * le numero d'entite physique
            physicalEntity = res[1]
            #   * le nom associe a cette entite physique
            # N.B.:  si le nom de l'entite physique contient des espaces, on doit concatener plusieurs colonnes
            physicalName = res[2]
            for i in range(3, len(res)):
               physicalName += " " + res[i]
            # on retire les guillemets au debut et a la fin de la chaine
            physicalName = physicalName[1:-1]

            # on ajoute l'entite physique au dictionnaire
            physicalEntity2physicalName[physicalEntity]=physicalName

      # si certaines entites physiques possedent un nom
      if len(physicalEntity2physicalName) != 0:
         # le nom remplace le numero d'entite physique pour les elements concernes
         
         # on recupere la liste des numeros d'entite physiques concernes
         named_physicalEntities=list(physicalEntity2physicalName.keys())
         # pour chaque element
         for ele in read_mesh.bulks:
            # si un nom est associe a son numero d'entite physique
            if ele.physicalEntity in named_physicalEntities:
               # on remplace le numero par le nom
               ele.physicalEntity = physicalEntity2physicalName[ele.physicalEntity]

      # on renvoie le maillage ainsi construit
      return read_mesh
  
def readGmshv4(mesh_file, dim, minor, keep_elements, incomplete_elements):
      """readGmshv4(mesh_file, dim, minor, keep_elements, incomplete_elements):

      this function builds sets of nodes and elements from a mesh
      built by gmsh, and using format 4.x

      parameters:

      - mesh_file: a file object coresponding to the file in wherein the mesh is stored
        N.B.: the file is supposed to be open for reading
      - dim: dim=2, in 2D and dim=3, in 3D
      - minor: the x of format 4.x

      optional parameters:

      - keep_elements: this parameter is used to filter elements, if
        keep_elements=None, no filter used, else keep_elements is the list
        of elements types that can be stored in the built mesh, other will be
        forgotten
      - incomplete_elements: used to reduce the order of an element

      returned value:

      - read_mesh: the built mesh
      """
      
      # on declare la liste des mots clefs, qu'on s'attend a trouver dans le fichier
      motCleMesh = ('$MeshFormat', '$EndMeshFormat', '$Entities','$EndEntities','$Nodes', '$EndNodes', '$Elements', '$EndElements', \
                    '$PhysicalNames', '$EndPhysicalNames', '$PartitionedEntities','$EndPartitionedEntities','$Periodic','$EndPeriodic',\
                    '$GhostElements','$EndGhostElements','$NodeData','$EndNodeData','$ElementData','$EndElementData', \
                    '$ElementNodeData','$EndElementNodeData', '$InterpolationScheme', '$EndInterpolationScheme')
      skipKeyWord = ('$PartitionedEntities', '$EndPartitionedEntities',
                     '$Periodic'           , '$EndPeriodic'           ,
                     '$GhostElements'      , '$EndGhostElements'      ,
                     '$NodeData'           , '$EndNodeData'           ,
                     '$ElementData'        , '$EndElementData'        ,
                     '$InterpolationScheme', '$EndInterpolationScheme',
                    )

      # on definit le dictionnaire associant un nom a certaines entites physiques
      physicalEntity2physicalName = collections.defaultdict( dict )
      
      Entity2PhysicalGroup = {}

      # on definit le maillage
      read_mesh = mesh(dimension=dim)

      motcle=''
      skip=False   
      # pour chaque ligne du fichier
      for ligne in mesh_file:
         # on separe les colonnes de la ligne courante
         res = ligne.split()
         # si la chaine de la premiere colonne est un mot-clef
         if res[0] in motCleMesh:
            # on le stocke 
            motcle=res[0]
      
         # les mots clefs non traites
         if motcle in skipKeyWord:
           skip=True  
 
         if skip : continue             

         if motcle == '$Entities' and res[0] =='$Entities':

            # next line contains : numPoints numCurves numSurfaces numVolumes             
            nextline=next(mesh_file)
            res_= nextline.split()
            dim_list = [ int(i_dim) for i_dim in res_ ]

            for i_dim, nb_idim in enumerate( dim_list ) :

              Entity2PhysicalGroup[i_dim] = collections.defaultdict( list )

              for i in range(nb_idim):

                # if minor > 0 and entity dim = 0, then next line is:
                # tag  X  Y  Z  numPhysicals physicalTag ... numBoundingPoints tagPoint ... 
                # otherwise
                # tag boxMinX boxMinY boxMinZ boxMaxX boxMaxY boxMaxZ numPhysicals physicalTag ... numBoundingPoints tagPoint ... 

                nextline=next(mesh_file) 
                res_= nextline.split()
                if i_dim == 0 and minor > 0 :
                  # python 3 only...
                  #tag, minx, miny, minz, *phy_tags = res_
                  tag      = res_[0]
                  minx     = res_[1]
                  miny     = res_[2]
                  minz     = res_[3]
                  phy_tags = res_[4:]
                else:
                  # python 3 only...
                  #tag, minx, maxx, miny, maxy, minz, maxz, *phy_tags = res_
                  tag      = res_[0]
                  minx     = res_[1]
                  maxx     = res_[2]
                  miny     = res_[3]
                  maxy     = res_[4]
                  minz     = res_[5]
                  maxz     = res_[6]
                  phy_tags = res_[7:]

                nb_tags = int(phy_tags[0])
                if nb_tags > 0 :
                  if nb_tags > 1 : showWarning("Multiple physical group by entity ("+str(i_dim)+"D) not managed yet, keep first")
                  Entity2PhysicalGroup[i_dim][int(tag)].append(int(phy_tags[1]))

            continue

         # si le dernier mot-clef lu est le debut de la section des noeuds,
         # et qu'on a pas atteint la fin de cette section
         if motcle == '$Nodes' and res[0] =='$Nodes':

           # next line contains: numEntityBlocks numNodes and for 4.1 and above minNodeTag and maxNodeTag
           nextline=next(mesh_file) 

           # python 3 only
           #nb_blocks, nb_nodes, *_ = ( int(v) for v in nextline.split() )
           nb_blocks, nb_nodes = ( int(v) for v in nextline.split()[:2] )

           # reading nodes by block
           for i in range( nb_blocks ) :

             if minor:
                 # next line contains: entDim, entTag, parametric, numNodesInBloc
                 nextline = next(mesh_file)
                 edim, blockTag, param, nodesInBlock = ( int(r) for r in nextline.split() )

                 node_list = []
                 for j in range(nodesInBlock):
                   nextline=next(mesh_file)
                   node_list.append( int(nextline) )

                 for node_tag in node_list:
                   nextline=next(mesh_file)
                   x, y , z = ( float(v) for v in nextline.split()[:3] )
                   if dim == 2:
                     coor = numpy.array( [ x, y ] )
                   elif dim == 3:
                     coor = numpy.array( [ x, y, z ] )
                   read_mesh.addNode( node(coor=coor,number=node_tag) )

             else:

                 nextline = next(mesh_file)
                 blockTag, bockDim, param, nodesInBlock = ( int(r) for r in nextline.split() )

                 for j in range( nodesInBlock ):

                   # next line contains: tag(int) x(double) y(double) z(double)
                   nextline = next(mesh_file)
                   res_= nextline.split()
                   node_tag = int(res_[0])
                   x, y , z = ( float(v) for v in res_[1:4] )
                   # si on est en 2D
                   if dim == 2:
                     coor = numpy.array( [ x, y ] )
                   # si on est en 3D
                   elif dim == 3:
                     coor = numpy.array( [ x, y, z ] )
                   # on ajoute le noeud lu au maillage
                   read_mesh.addNode(node(coor=coor,number=node_tag))

           # next line
           continue

     
         # si le dernier mot-clef lu est le debut de la section des elements,
         # et qu'on a pas atteint la fin de cette section
         if motcle == '$Elements' and res[0] =='$Elements':

            # next line contains: numEntityBlocks numElements minElemTag maxElemTag
            nextline=next(mesh_file) 
            # python 3 only...
            #nb_blocks, nb_elems, *_ = ( int(v) for v in nextline.split() )
            nb_blocks, nb_elems = ( int(v) for v in nextline.split()[:2] )
            print("Number of entity blocks for element: ", nb_blocks, " Number of elements: ", nb_elems)

            for i in range( nb_blocks ):

              # next line contains : tagBlock dimEle typeEle numElementsInBlock
              nextline = next(mesh_file) 
              res_= nextline.split()

              # if _4_1:
              if minor :
                  dimE, geoE, typeE, elemsInBlock = ( int(r) for r in res_ )
              else:
                  geoE, dimE, typeE, elemsInBlock = ( int(r) for r in res_ )

              # rm : seriously...
              typeEle = str(typeE)

              if typeEle in gmshv2ElementPoint:
                elem_dim = 0
              elif typeEle in gmshv2ElementLine:
                elem_dim = 1
              elif typeEle in gmshv2ElementSurface:
                elem_dim = 2
              elif typeEle in gmshv2ElementVolume:
                elem_dim = 3
              else:
                showError( "[readGmshv4]: unknown gmsh element type: "+str(typeE) )

              if elem_dim != dimE :
                 showError('[readGmshv4]: unexpected data in $Elements')

              # be carefull geometrical entity can support several physical entity
              pE=str(0)
              if dimE in Entity2PhysicalGroup.keys():
                if geoE in Entity2PhysicalGroup[dimE].keys():
                  pE = Entity2PhysicalGroup[dimE][geoE][0]
                  #print( f'element of dim {dimE} tagged {geoE} is in group {pE}' )

              # on lit les elements et on les ajoute a la liste des elements
              for j in range( elemsInBlock ):

                # next line contains: tag numVert ... 
                nextline = next(mesh_file) 
                res_ = nextline.split()

                elemTag =  int(res_[0])
                #   * the element connectivity
                conn = [ int(r) for r in res_[1:] ]

                # si le filtrage des elements est active et que l'element ne fait pas
                # partie des elements conserves
                if keep_elements != [] and not elem_dim in keep_elements:
                  # on passe au suivant
                  continue

                #print("element type:",tE," connectivity: ",conn)

                # connectivity permutation in some particular case:
                if typeE == 11: # TE10
                  # on permute la numerotation des noeuds de l'element pour retrouver l'ordre choisi dans LMGC90 
                  conn[8], conn[9] = conn[9], conn[8]
                elif typeE == 13 or (typeE==18 and incomplete_elements) :
                  # on permute la numerotation des noeuds de l'element pour retrouver l'ordre choisi dans LMGC90
                  conn[7], conn[8], conn[9] , conn[10], conn[11], conn[12], conn[13], conn[14] = \
                  conn[9], conn[7], conn[12], conn[14], conn[13], conn[8] , conn[10], conn[11]
                elif typeE == 17 or (typeE==12 and incomplete_elements) :
                  # on permute la numerotation des noeuds de l'element pour retrouver l'ordre choisi dans LMGC90 
                  conn= [conn[0] , conn[1] , conn[2] , conn[3] , conn[4] ,
                         conn[5] , conn[6] , conn[7] , conn[8] , conn[11],
                         conn[13], conn[9] , conn[16], conn[18], conn[19],
                         conn[17], conn[10], conn[12], conn[14], conn[15]]
                elif typeE == 10 and incomplete_elements:
                  # si l'element est un Q9 on lit un Q8
                  conn = conn[:8]

                # si pas cas particulier
                # on utilise la connectivite de l'element telle qu'on l'a lue

                # pour le 2D on verifie que l'element est oriente suivant z sinon on retourne
                if dim == 2:
                  if len(conn) > 2:
                    v1 = read_mesh.nodes[conn[ 1]].coor - read_mesh.nodes[conn[0]].coor
                    v2 = read_mesh.nodes[conn[-1]].coor - read_mesh.nodes[conn[0]].coor
 
                    xx =  v1[0]*v2[1] - v1[1]*v2[0]

                    if xx < 0.:
                      if len(conn)==6 :
                        conn0=deepcopy(conn)
                        conn = [conn0[0],conn0[2],conn0[1],conn0[5],conn0[4],conn[3]] 
                      else:
                        conn.reverse()

                # on construit l'element et on l'ajoute au maillage
                #print( f"add bulk of dim {elem_dim}, of conn {conn}, with tags {pE}/{gE} and id {res_[0]}")
                read_mesh.addBulk( element(elem_dim=elem_dim, connectivity=conn, \
                                           physicalEntity=str(pE), geometricalEntity=str(geoE), number=elemTag))
            continue

         # si le dernier mot-clef lu est le debut de la section des noms des entites physiques,
         # et qu'on a pas atteint la fin de cette section
         if motcle == '$PhysicalNames' and res[0]=='$PhysicalNames':
            # next line contains: numPhysicalNames
            nextline=next(mesh_file) 
            res_= nextline.split()
            print("Number of named physical group: ",res_[0])

            for i in range(int(res_[0])):
              # next line contains: dimension tag name
              nextline=next(mesh_file) 
              res_= nextline.split()
 
              # on recupere :
              #   * la dimension de l'entite physique 
              #   * le numero d'entite physique
              physicalDim, physicalEntity = ( int(r) for r in res_[:2] )
              #   * le nom associe a cette entite physique
              # N.B.:  si le nom de l'entite physique contient des espaces, on doit concatener plusieurs colonnes
              physicalName = " ".join(res_[2:])
              # on retire les guillemets au debut et a la fin de la chaine
              physicalName = physicalName[1:-1]

              # on ajoute l'entite physique au dictionnaire
              # la clef etant la combinaison (dimension, id) de la physical entity
              physicalEntity2physicalName[physicalDim][physicalEntity] = physicalName
            continue
        
      # si certaines entites physiques possedent un nom
      if len(physicalEntity2physicalName) != 0:
         # le nom remplace le numero d'entite physique pour les elements concernes
         
         # on recupere la liste des numeros d'entite physiques concernes
         named_physicalEntities=list(physicalEntity2physicalName.keys())
         # pour chaque element
         for ele in read_mesh.bulks:
            # si un nom est associe a son numero d'entite physique
            elem_dim = geoElement2dimension[ele.etype]
            if int(ele.physicalEntity) in physicalEntity2physicalName[elem_dim].keys() :
               # on remplace le numero par le nom
               ele.physicalEntity = physicalEntity2physicalName[elem_dim][int(ele.physicalEntity)]

      # on renvoie le maillage ainsi construit
      return read_mesh

  
  
def readSysweld(fname,dim):
      """readSysweld(fname, dim):

      this function builds sets of nodes and elements from a mesh
      built by sysweld

      parameters:

      - fname: a file in which the mesh is stored
      - dim: dim=2, in 2D and dim=3, in 3D

      returned value:

      - read_mesh: the built mesh
      """
      
      # on declare la liste des mots clefs, qu'on s'attend a trouver dans le fichier
      motCleMesh = ('NOEUD', 'ELEMENT')

      # on declare le dictionnaire utilise pour passer de l'information NOEUD PRIMAIRE,
      # a l'information NOEUD
      dic = {'Y=':'Y= ','X=' : 'X= ','Z=':'Z= '}
      # on stocke les clefs de ce dictionnaire
      dic_keys = list(dic.keys())

      # on definit le maillage
      read_mesh = mesh(dimension=dim)

      with open(fname,'r') as mesh_file : 
          # pour chaque ligne du fichier
          for ligne in mesh_file:
             # On supprime l information NOEUD PRIMAIRE du fichier qui crera un conflit avec NOEUD
             if ligne.find('PRIMAIRE') < 0:
                for j in dic_keys:
                   ligne=ligne.replace(j, dic[j])

             # on separe les colonnes de la ligne courante
             res = ligne.split()
             # si la chaine de la premiere colonne est un mot-clef
             if res[0] in motCleMesh:
                # on le stocke 
                motcle=res[0]
             # si la ligne courante decrit un noeud
             if motcle == 'NOEUD' and res[0]=='NOEUD':
                # si on est en 2D
                if dim == 2:
                   coor  = numpy.array( [ float(res[5]), float(res[7]) ] )
                # si on est en 3D
                elif dim == 3:
                   coor  = numpy.array( [ float(res[5]), float(res[7]), float(res[9]) ] )
                read_mesh.addNode(node(coor=coor,number=int(res[0])))
             # si la ligne courante est la premiere des deux lignes decrivant un element
             if motcle == 'ELEMENT' and res[0]=='ELEMENT':
                # on recupere le type, le numero et le groupe du nouvel element
                if res[3] not in sysweldElementSurface:
                  showError("[readMesh::readSysweld] : unknown sysweld element type: "+res[3])
                num   = res[1]
                group = res[6]
             # si la ligne courante est la seconde des deux lignes decrivant un element
             if motcle == 'ELEMENT' and res[0]!='ELEMENT':
                # on complete la description du nouvel element et on l'ajoute au maillage
                read_mesh.addBulk(element(elem_dim=2, connectivity=res[1:], physicalEntity=group))

      # on renvoie le maillage ainsi construit
      return read_mesh

def readMail(fname, dim):
      """readMail(fname, dim):

      this function builds a mesh by reading a file using the 'mail' format

      parameters:

      - fname: a file in which the mesh is stored
        N.B.: the file is supposed to be open for reading
      - dim: dim=2, in 2D and dim=3, in 3D

      returned value:

      - read_mesh: the built mesh
      """
      
      # on declare la liste des mots clefs, qu'on s'attend a trouver dans le fichier
      motCleMesh = ('COOR_3D', # liste de noeuds en avec des coordonnees en 3D
                    'GROUP_MA', # groupe d'elements
                    'GROUP_NO', # groupe de noeuds
                    'FINSF', # fin de section
                    'FIN') # fin du fichier

      # on definit le maillage
      read_mesh = mesh(dim)

      # on initialise a vide le dictionnaire qui associe l'identifiant d'un noeud a son numero
      node_id2num = {}
      # on initialise a 0 le numero du noeud courant
      num = 0

      # on initialise le type d'element de la liste d'elements courante a vide
      type_ele=None
      # on initialise le type d'element corespondant, dans la nomencalture de LMGC90
      type_ele_lmgc=None
      # on initialise a vide le dictionnaire qui associe l'identifiant d'un element a l'objet bulk qui lui
      # corespond
      ele_id2bulk = {}

      # on indique que le nom du groupe n'a pas encore ete lu
      is_read=False
      # on initialise le nom du groupe courant a vide
      group_name=None

      with open(fname,'r') as mesh_file : 
          # pour chaque ligne du fichier
          for ligne in mesh_file:
             # on separe les colonnes de la ligne courante
             res = ligne.split()
             # si la chaine de la premiere colonne est un mot-clef
             if res[0] in motCleMesh:
                # on verifie la validite du mot-clef :
                #    * si le mot-clef est 'COOR_3D' et que le maillage est 2D
                if res[0] == 'COOR_3D' and dim == 2:
                   # on affiche un message d'erreur
                   showError("[readMesh::readMail] : The file decribes 3D coordinates while you want to build a 2D mesh!")

                # si le mot clef est valide, on le stocke 
                motcle=res[0]
             # si la chaine de la premiere colonne est un nom d'element
             if res[0] in mailElementSurface:
                # on stocke le type d'element decrit dans la prochaine liste d'elements
                type_ele=res[0]
                # on le stocke comme un mot clef
                motcle=res[0]
             else:
                showError("[readMesh::readMail] : unknown mail element type: "+res[0])
             # si le dernier mot-clef lu est le debut d'une de noeuds, avec des coordonnees 3D,
             # et qu'on a pas atteint la fin de cette section
             # N.B.: le test 'len(res) > 1' permet de sauter la ligne qui contient le mot-clef
             if motcle == 'COOR_3D' and res[0] != 'FINSF' and len(res) > 1 :
                # on incremente le numero du noeud courant
                num += 1
                # on associe l'identifiant du noeud courant a son numero
                node_id2num[res[0]] = num
                coor  = numpy.array( [ float(res[1]), float(res[2]), float(res[3]) ] )
                # on ajoute le noeud lu au maillage
                read_mesh.addNode(node( coor=coor, number=num))
             # si le dernier mot-clef lu est un type d'element
             # et qu'on a pas atteint la fin de la liste des elements de ce type
             # N.B.: le test 'len(res) > 1' permet de sauter la ligne qui contient le type de l'element
             if motcle == type_ele and res[0] != 'FINSF' and len(res) > 1 :
                # on recupere la connectivite de l'element, decrite comme la liste des identifiants
                # des noeuds
                conn_id=res[1:]
                # on construit la connectivite du nouvel element, a partir des la liste des identifiants
                # des noeuds

                # on initialise la connectivite a vide
                conn=[]
                # pour chaque identifiant de noeud
                for id_node in conn_id:
                   # on ajoute le numero du noeud associe a cet identifiant a la connectivite de l'element
                   conn.append(node_id2num[id_node])

                # on peut alors construire le nouvel element
                ele=element(elem_dim=2, connectivity=conn)
                # on l'ajoute au maillage
                read_mesh.addBulk(ele)
                # on associe le nouvel element a son identifiant
                ele_id2bulk[res[0]] = ele
             # si le dernier mot-clef lu est le debut d'un groupe d'elements
             # et qu'on a pas atteint la fin de cette section
             if motcle == 'GROUP_MA' and res[0] != 'FINSF':
                # si on est toujours sur la ligne declarant le nouveau groupe
                if res[0] == 'GROUP_MA':
                   # on indique que le nom du groupe n'a pas encore ete lu
                   is_read=False
                   # on passe a la ligne suivante
                   continue
                # sinon, si on a pas encore lu le nom du groupe
                elif not is_read:
                   # on le stocke
                   group_name=res[0]
                   # on indique que le nom de groupe a ete lu
                   is_read=True
                   # on passe a la ligne suivante
                   continue
                # sinon,
                else:
                   # on est en train de parcourir la liste des identifiants d'elements
                   # et on indique que l'element associe a l'identifiant courant appartient
                   # au groupe courant

                   # si on a deja associe un groupe a l'element
                   if ele_id2bulk[res[0]].physicalEntity != '1':
                      # on affiche un message d'erreur
                      showError("[readMesh::readMail] : A group have been already added to this element!")
                   # sinon,
                   else:
                      # on associe le groupe courant l'element identifie par l'identifiant courant
                      ele_id2bulk[res[0]].physicalEntity=group_name 
             # si le dernier mot-clef lu est le debut d'un groupe de noeuds
             # et qu'on a pas atteint la fin de cette section
             if motcle == 'GROUP_NO' and res[0] != 'FINSF':
                # si on est toujours sur la ligne declarant le nouveau groupe
                if res[0] == 'GROUP_NO':
                   # on indique que le nom du groupe n'a pas encore ete lu
                   is_read=False
                   # on passe a la ligne suivante
                   continue
                # sinon, si on a pas encore lu le nom du groupe
                elif not is_read:
                   # on le stocke
                   group_name=res[0]
                   # on indique que le nom de groupe a ete lu
                   is_read=True
                   # on passe a la ligne suivante
                   continue
                # sinon,
                else:
                   # on est en train de parcourir la liste des identifiants de noeuds
                   # et on cree un element point, appuye sur ce noeud, et qui appartient
                   # a ce groupe

                   # on ajoute un element point, appuye sur ce noeud, au maillage
                   read_mesh.addBulk( element(elem_dim=0, connectivity=[node_id2num[res[0]]], physicalEntity=group_name) )
             # si le dernier mot-clef lu indique la fin du fichier
             if motcle == 'FIN':
                # on arrete la lecture du fichier
                break

      # on renvoie le maillage ainsi construit
      return read_mesh

def readVtu(fname, dim, keep_elements):
      """readVtu(fname, dim):

      this function builds sets of nodes and elements from a mesh
      built by vtk, and using format vtu

      parameters:

      - mesh_file: a file in which the mesh is stored
      - dim: dim=2, in 2D and dim=3, in 3D
      
      optional parameters:

      - keep_elements: this parameter is used to filter elements, if
        keep_elements=None, no filter used, else keep_elements is the list
        of elements types that can be stored in the built mesh, other will be
        forgotten
        
      returned value:

      - read_mesh: the built mesh
      """

      import vtk
      
      # on utilise le reader vtk des maillages non structure
      # Read the source file.
      try :
        reader = vtk.vtkXMLUnstructuredGridReader()
      except:
        reader = vtk.vtkUnstructuredGridReader()
      reader.SetFileName(fname)
      reader.Update()
      datavtu = reader.GetOutput()
     
      # Recuperation du maillage
      nb_nodes = int(datavtu.GetNumberOfPoints())
      nb_cells = int(datavtu.GetNumberOfCells())

      # on definit le dictionnaire associant un nom a certaines entites physiques
      physicalEntity2physicalName={}

      # on definit le maillage au format LMGC90
      read_mesh = mesh(dimension=dim)
      
      # pour chaque noeud du maillage vtu
      for i in range(nb_nodes):
         coor = numpy.asarray(datavtu.GetPoints().GetPoint(i))[:dim]
         read_mesh.addNode(node(coor=coor,number=int(i)))
      
      # on charge les entites physiques et geometriques par un array nomme PhysicalGroup
      PhysicalGroup = datavtu.GetCellData().GetArray('PhysicalGroup')
      if PhysicalGroup is None : 
          showWarning('No physical groups defined on this mesh!')
      
      # pour chaque element du maillage vtu
      for i in range(nb_cells):
         vtkid = str(datavtu.GetCellType(i))
         if vtkid in vtkElementPoint:
           elem_dim = 0
         elif vtkid in vtkElementLine:
           elem_dim = 1
         elif vtkid in vtkElementSurface:
           elem_dim = 2
         elif vtkid in vtkElementVolume:
           elem_dim = 3
         else:
           print(int(datavtu.GetCell(i).GetNumberOfPoints()))
           showError("[readMesh::readVtu] : unknown vtk element type: "+vtkid)

         # filtre des elements non interressant
         if keep_elements != [] and not elem_dim in keep_elements:
            # on passe au suivant
            continue
         # lecture d'un element interressant
         conn = []
         nb_points = int(datavtu.GetCell(i).GetNumberOfPoints())
         for j in range(nb_points):
            conn.append(int(datavtu.GetCell(i).GetPointIds().GetId(j)))

         if vtkid == '13':
           conn_PRI6 = [conn[0], conn[2], conn[1], conn[3], conn[5], conn[4]]
           conn = conn_PRI6
         elif vtkid == '26':
           conn_PRI15 = [conn[0] , conn[2], conn[1] , conn[3] , conn[5] ,
                         conn[4] , conn[8], conn[7] , conn[6] , conn[11],
                         conn[10], conn[9], conn[12], conn[14], conn[13]]
           conn = conn_PRI15

         if PhysicalGroup != None :
            # on construit les entites physiques et geometriques par un array nomme PhysicalGroup
            pE = str(int(PhysicalGroup.GetValue(i)))
            gE = str(int(PhysicalGroup.GetValue(i)))
         else :
            pE = '1'
            gE = '1'
 
         # on construit l'element et on l'ajoute au maillage
         read_mesh.addBulk(element(elem_dim=elem_dim, connectivity=conn, physicalEntity=pE, geometricalEntity=gE, number=int(i)))
      
      # si certaines entites physiques possedent un nom
      if len(physicalEntity2physicalName) != 0:
         # le nom remplace le numero d'entite physique pour les elements concernes
         # on recupere la liste des numeros d'entite physiques concernes
         named_physicalEntities=list(physicalEntity2physicalName.keys())
         # pour chaque element
         for ele in read_mesh.bulks:
            # si un nom est associe a son numero d'entite physique
            if ele.physicalEntity in named_physicalEntities:
               # on remplace le numero par le nom
               ele.physicalEntity = physicalEntity2physicalName[ele.physicalEntity]
      
      # on renvoie le maillage ainsi construit
      return read_mesh


def readInp(fname, dim, keep_elements):
      """readInp(fname, dim):

      Build a mesh from an Abaqus file

      parameters:

      - fname : a file name in which the mesh is stored
      - dim: dim=2, in 2D and dim=3, in 3D
      - keep_elements: this parameter is used to filter elements, if
        keep_elements=None, no filter used, else keep_elements is the list
        of elements types that can be stored in the built mesh, other will be
        forgotten
      returned value:

      - read_mesh: the built mesh
      """

      read_mesh = mesh(dimension=dim)

      with open(fname,'r') as mesh_file :

          line = mesh_file.readline()
          while line:

            # node reading
            while line and line.strip() != "*NODE" and line.split(',')[0] != "*ELEMENT" :
              line = mesh_file.readline()

            if line.strip() == "*NODE":
              line = mesh_file.readline()
              while line[0] != '*':
                line = line.split(',')
                coor = np.array( [float(line[1]),float(line[2]),float(line[3])] )
                no = node( coor=coor[:dim], number=int(line[0]) )
                read_mesh.addNode( no )
                line = mesh_file.readline()

            # element reading
            if line.split(',')[0] == "*ELEMENT":

              line=line.split(',')

              etype = line[1].split('=')[-1]

              if etype in inpElementPoint:
                elem_dim = 0
              elif etype in inpElementLine:
                elem_dim = 1
              elif etype in inpElementSurface:
                elem_dim = 2
              elif etype in inpElementVolume:
                elem_dim = 3
              else:
                showError("[readMesh::readInp] : unknown abaqus element type: "+etype)


              gr = line[-1].split('=')[-1].strip()
              line = mesh_file.readline()

              to_skip = False
              if keep_elements != [] and not elem_dim in keep_elements:
                 to_skip = True

              while line[0] != '*' and not to_skip:
                line = line.split(',')
                conn = list(map(int,line[1:]))

                elem = element(elem_dim=elem_dim, connectivity=conn,
                               physicalEntity=gr, number=int(line[0])
                              )

                read_mesh.addBulk( elem )

                line = mesh_file.readline()

      return read_mesh

