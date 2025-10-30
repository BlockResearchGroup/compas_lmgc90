# module qui fournit une classe brique 2D, ou 3D

import numpy

from ..avatar.bulk.rigid2d import *
from ..avatar.bulk.element import *

# macros pour generer des maillages de rectangles (2D)
from .mesh2D import *

# classe brique 2D
class brick2D():
   '''class designed to build rigid or deformable bricks in 2D

   Methods:

   - __init__
   - rigidBrick
   - deformableBrick
   - explodedDeformableBrick'''

   # constructeur
   def __init__(self, name, lx, ly):
      '''__init__(self, name, lx, ly):

      creates a new brick

      parameters:

      - self: the new object, itself
      - name: name of the brick; useful to define special bricks, to 
        define a door or a window for example
      - lx: length of the brick
      - ly: heigth of the brick'''

      # stockage du nom de la brique
      self.name=name
      # stockage des dimensions de la brique
      self.lx=lx # longueur
      self.ly=ly # hauteur

   # creation d'une brique rigide
   def rigidBrick(self, center, model, material, color='BLUEx', number=None):
      '''rigidBrick(self, center, model, material, color='BLUEx', number=None):

      this function build and returns a rigid brick

      parameters:

      - self: the object brick itself
      - center: position of the center of inertia in the global frame
      - model: rigid model for the brick
      - material: the brick is made of this material

      optional parameter :

      - color='BLUEx': color of the contactor polygon
      - number=None: index of the avatar (still present to ensure compatibility)'''

      # creation d'une nouvelle brique rigide
      body = avatar(dimension=2, number=number) 
      # on lui attribue un comportement volumique de type rigide
      body.addBulk( rigid2d() ) 
      # ajout de la position du centre d'inertie de la brique
      body.addNode( node(coor=numpy.array(center), number=1) )
      # on definit les groupes pour la brique
      body.defineGroups()
      # on affecte son modele rigide a la brique
      body.defineModel(model=model)
      # on affecte son materiau a la brique
      body.defineMaterial(material=material)

      # definition du contacteur polygone pour la brique :

      # on cree une matrice de double a la bonne taille
      vertices = numpy.zeros([4, 2], 'd')
      # on caclule les positions des sommets par rapport au centre d'inertie:
      #  3 *-------* 2              
      #    |   +   |
      #  0 *-------* 1
      vertices[0, 0] = -0.5*self.lx 
      vertices[0, 1] = -0.5*self.ly 
      vertices[1, 0] = 0.5*self.lx 
      vertices[1, 1] = -0.5*self.ly 
      vertices[2, 0] = 0.5*self.lx 
      vertices[2, 1] = 0.5*self.ly 
      vertices[3, 0] = -0.5*self.lx 
      vertices[3, 1] = 0.5*self.ly 
      # on peut alors ajouter son contacteur polygone a la brique
      body.addContactors(shape='POLYG',color=color, nb_vertices=4, 
         vertices=vertices)
      # on calcule la surface et l'inertie de la brique
      body.computeRigidProperties()

      # on renvoie le corps genere
      return body

   # creation d'une brique deformable
   def deformableBrick(self, center, material, model, mesh_type='4T3', 
      nb_elem_x=1, nb_elem_y=1, apabh=None, apabv=None, apabhc=0.25, apabvc=0.25,
      colors=['HORIx', 'VERTx', 'HORIx', 'VERTx'], number=None):
      '''deformableBrick(self, center, material, model, number, mesh_type='4T3', 
      nb_elem_x=1, nb_elem_y=1, apabh=None, apabv=None, apabhc=0.25, apabvc=0.25,
      colors=['HORIx', 'VERTx', 'HORIx', 'VERTx']):

      this function builds and returns a deformable brick

      parameters:

      - self: the object brick itself
      - center: position of the center of inertia in the global frame
      - material: the brick is made of this material
      - model: mechanical model for the brick

      optional parameters :

      - mesh_type='4T3': meshing strategy for the brick
        (possible values: 'Q4', 'Q8', '2T3' and '4T3')
      - nb_elem_x: number of elements Q4 following direction Ox
      - nb_elem_y: number of elements Q4 following direction Oy
      - apabh: curvilign abscissas used to put candidate points on horizontal lines
      - apabv: curvilign abscissas used to put candidate points on vertical lines
      - apabhc: curvilign abscissa used to put candidate point near
        a corner, on an horizontal line
      - apabvc: curvilign abscissa used to put candidate point near
        a corner, on an vertical line
      - colors=['HORIx', 'VERTx', 'HORIx', 'VERTx']: colors of the 
        contactors for each side of the brick, given in the 
        following order: 'down', 'right', 'up', 'left' 
      - number=None: index of the avatar (still present to ensure compatibility)'''

      # on calcule la position du coin inferieur gauche de la brique
      x0=center[0] - 0.5*self.lx
      y0=center[1] - 0.5*self.ly
      # on genere le maillage de la brique
      mesh_brick=buildMesh2D(mesh_type, x0, y0, self.lx, self.ly, nb_elem_x, nb_elem_y) 
      # on contruit un corps maille a partir du maillage de la brique
      body=buildMeshedAvatar(mesh=mesh_brick, model=model, material=material)
      # on ajoute les contacteurs :
      #    * une ligne antagoniste sur le dessus du bloc
      body.addContactors(group='up',shape='ALpxx',color=colors[2])
      #    * les points candidats sur le dessous du bloc
      # si on donne des positions pour le points de contacts, on les
      # utilise
      if ( apabh ):
         # fd pas comprendre 
         #body.addContactors(group='down',shape='CLxxx',color=colors[0],
         #   connex='no', weights=apabh)
         body.addContactors(group='down',shape='CLxxx',color=colors[0],
            weights=apabh)
      # sinon, 
      else:
         # on les place aux sommets du maillage
         body.addContactors(group='down',shape='CLxxx',color=colors[0])
         # on deplace les noeuds situes :
         #    * dans le coin inferieur gauche de la brique
         _moveContactorCLxxx(coor=[x0, y0], body=body, color=colors[0], apab=apabhc)
         #    * dans le coin inferieur droit de la brique
         _moveContactorCLxxx(coor=[x0 + self.lx, y0], body=body, color=colors[0], apab=apabhc)

      #    * une ligne antagoniste sur le cote gauche du bloc
      body.addContactors(group='left',shape='ALpxx',color=colors[3])
      #    * les points candidats sur le cote droit du bloc
      # si on donne des positions pour le points de contacts, on les
      # utilise
      if ( apabv ):
         #body.addContactors(group='right',shape='CLxxx',color=colors[1],
         #   connex='no', weights=apabv)
         body.addContactors(group='right',shape='CLxxx',color=colors[1],
            weights=apabv)
      # sinon,
      else:
         # on les place aux sommets 
         body.addContactors(group='right',shape='CLxxx',color=colors[1])
         # on deplace les noeuds situes :
         #    * dans le coin inferieur droit de la brique
         _moveContactorCLxxx(coor=[x0 + self.lx, y0], body=body, color=colors[1], apab=apabvc)
         #    * dans le coin superieur droit de la brique
         _moveContactorCLxxx(coor=[x0 + self.lx, y0 + self.ly], body=body, color=colors[1], apab=apabvc)

      # on renvoie le corps genere
      return body               

    # creation d'une brique deformable, explosee
   def explodedDeformableBrick(self, center, material, model, mesh_type='4T3', 
      nb_elem_x=1, nb_elem_y=1, apabh=None, apabv=None, apabhc=0.25, apabvc=0.25,
      colors=['HORIx', 'VERTx', 'HORIx', 'VERTx'], color='BLUEx', shift=0):
      '''explodedDeformableBrick(self, center, material, model, mesh_type='4T3', 
      nb_elem_x=1, nb_elem_y=1, apabh=None, apabv=None, apabhc=0.25, apabvc=0.25,
      colors=['HORIx', 'VERTx', 'HORIx', 'VERTx'], color='BLUEx', shift=0):

      this function builds and returns a deformable brick

      parameters:

      - self: the object brick itself
      - center: position of the center of inertia in the global frame
      - material: the brick is made of this material
      - model: mechanical model for the brick

      optional parameters :

      - mesh_type='4T3': meshing strategy for the brick
        (possible values: 'Q4', 'Q8', '2T3' and '4T3')
      - nb_elem_x: number of elements Q4 following direction Ox
      - nb_elem_y: number of elements Q4 following direction Oy
      - apabh: curvilign abscissas used to put candidate points on horizontal lines
      - apabv: curvilign abscissas used to put candidate points on vertical lines
      - apabhc: curvilign abscissa used to put candidate point near
        a corner, on an horizontal line
      - apabvc: curvilign abscissa used to put candidate point near
        a corner, on an vertical line
      - color: color of contactors used to set cohesive zones
      - colors=['HORIx', 'VERTx', 'HORIx', 'VERTx']: colors of the 
        contactors for each side of the brick, given in the 
        following order: 'down', 'right', 'up', 'left'
      - shift: shift in the bodies numbering (still present to ensure compatibility)'''

      # on calcule la position du coin inferieur gauche de la brique
      x0=center[0] - 0.5*self.lx
      y0=center[1] - 0.5*self.ly
      # on genere le maillage de la brique
      mesh_brick=buildMesh2D(mesh_type, x0, y0, self.lx, self.ly, nb_elem_x, nb_elem_y) 
      # on contruit un corps maille a partir du maillage de la brique
      body=buildMeshedAvatar(mesh=mesh_brick, model=model, material=material)
      # on explose le corps maille represenant la brique
      bodies=explodeMeshedAvatar2D(body, nbPoints=2, color=color)
      # pour chaque corps resultant
      for new_body in bodies:
         # on ajoute les contacteurs :
         #    * une ligne antagoniste sur le dessus du bloc
         if new_body.hasGroup('up'):
            new_body.addContactors(group='up',shape='ALpxx',color=colors[2])
         #    * les points candidats sur le dessous du bloc
         if new_body.hasGroup('down'):
            # si on donne des positions pour le points de contacts, on les
            # utilise
            if ( apabh ):
               #body.addContactors(group='down',shape='CLxxx',color=colors[0],
               #   connex='no', weights=apabh)
               body.addContactors(group='down',shape='CLxxx',color=colors[0],
                  weights=apabh)
            # sinon,  
            else:
               # on les place aux sommets 
               new_body.addContactors(group='down',shape='CLxxx',color=colors[0])
               # on deplace les noeuds situes :
               #    * dans le coin inferieur gauche de la brique
               _moveContactorCLxxx(coor=[x0, y0], body=new_body, color=colors[0], apab=apabhc)
               #    * dans le coin inferieur droit de la brique
               _moveContactorCLxxx(coor=[x0 + self.lx, y0], body=new_body, color=colors[0], apab=apabhc)

         #    * une ligne antagoniste sur le cote gauche du bloc
         if new_body.hasGroup('left'):
            new_body.addContactors(group='left',shape='ALpxx',color=colors[3])
         #    * les points candidats sur le cote droit du bloc
         if new_body.hasGroup('right'):
            # si on donne des positions pour le points de contacts, on les
            # utilise
            if ( apabv ):
               #body.addContactors(group='right',shape='CLxxx',color=colors[1],
               #   connex='no', weights=apabv)
               body.addContactors(group='right',shape='CLxxx',color=colors[1],
                  weights=apabv)
            # sinon, 
            else:
               # on les place aux sommets 
               new_body.addContactors(group='right',shape='CLxxx',color=colors[1])
               # on deplace les noeuds situes :
               #    * dans le coin inferieur droit de la brique
               _moveContactorCLxxx(coor=[x0 + self.lx, y0], body=new_body, color=colors[1], apab=apabvc)
               #    * dans le coin superieur droit de la brique
               _moveContactorCLxxx(coor=[x0 + self.lx, y0 + self.ly], body=new_body, color=colors[1], apab=apabvc)

      # on renvoie la liste de corps generee
      return bodies

# classe brique 3D
class brick3D():
   '''class designed to build rigid bricks in 3D
   Methods:

   - __init__
   - rigidBrick
   '''

   # constructeur
   def __init__(self, name, lx, ly, lz):
      '''__init__(self, name, lx, ly, lz):

      creates a new brick

      parameters:

      - self: the new object, itself
      - name: name of the brick; useful to define special bricks, to 
        define a door or a window for example
      - lx: length of the brick
      - ly: depth of the brick
      - lz: heigth of the brick'''

      # stockage du nom de la brique
      self.name=name
      # stockage des dimensions de la brique
      self.lx=lx # longueur
      self.ly=ly # largeur
      self.lz=lz # hauteur

   # creation d'une brique rigide
   def rigidBrick(self, center, model, material, color='BLUEx'):
      '''rigidBrick(self, center, model, material, color='BLUEx'):
      this function build and returns a rigid brick

      parameters:

      - self: the object brick itself
      - center: position of the center of inertia in the global frame
      - model: rigid model for the brick
      - material: the brick is made of this material

      optional parameter :

      - color='BLUEx': color of the contactor polhedron
      '''

      # creation d'une nouvelle brique rigide
      body = avatar(dimension=3) 
      # on lui attribue un comportement volumique de type rigide
      body.addBulk( rigid3d() ) 
      # ajout de la position du centre d'inertie de la brique
      body.addNode( node(coor=numpy.array(center), number=1) )
      # on definit les groupes pour la brique
      body.defineGroups()
      # on affecte son modele rigide a la brique
      body.defineModel(model=model)
      # on affecte son materiau a la brique
      body.defineMaterial(material=material)

      # definition du contacteur polyedre pour la brique :

      # on cree une matrice :
      #    - de double, pour les coordonnees des sommets
      vertices = numpy.zeros([8, 3], 'd')
      #    - d'entiers, pour la connectivite des faces
      connectivity = numpy.zeros([12, 3], 'i')

      # on caclule les positions des sommets par rapport au centre d'inertie:
      vertices[0, 0] = -0.5*self.lx 
      vertices[0, 1] = -0.5*self.ly 
      vertices[0, 2] = -0.5*self.lz 

      vertices[1, 0] =  0.5*self.lx 
      vertices[1, 1] = -0.5*self.ly 
      vertices[1, 2] = -0.5*self.lz 

      vertices[2, 0] =  0.5*self.lx 
      vertices[2, 1] =  0.5*self.ly 
      vertices[2, 2] = -0.5*self.lz 

      vertices[3, 0] = -0.5*self.lx 
      vertices[3, 1] =  0.5*self.ly 
      vertices[3, 2] = -0.5*self.lz 

      vertices[4, 0] = -0.5*self.lx 
      vertices[4, 1] = -0.5*self.ly 
      vertices[4, 2] =  0.5*self.lz 

      vertices[5, 0] =  0.5*self.lx 
      vertices[5, 1] = -0.5*self.ly 
      vertices[5, 2] =  0.5*self.lz 

      vertices[6, 0] =  0.5*self.lx 
      vertices[6, 1] =  0.5*self.ly 
      vertices[6, 2] =  0.5*self.lz 

      vertices[7, 0] = -0.5*self.lx 
      vertices[7, 1] =  0.5*self.ly 
      vertices[7, 2] =  0.5*self.lz 
      # on donne la connnectivite des faces triangulaires
      # dans le plan (x, y, zG - lz)
      connectivity[0, 0]=1  
      connectivity[0, 1]=3  
      connectivity[0, 2]=2  

      connectivity[1, 0]=1  
      connectivity[1, 1]=4  
      connectivity[1, 2]=3  
      # dans le plan (x, y, zG + lz)
      connectivity[2, 0]=6  
      connectivity[2, 1]=7  
      connectivity[2, 2]=8  

      connectivity[3, 0]=6  
      connectivity[3, 1]=8  
      connectivity[3, 2]=5  
      # dans le plan (x, yG - ly, z)
      connectivity[4, 0]=1  
      connectivity[4, 1]=2  
      connectivity[4, 2]=5  

      connectivity[5, 0]=2  
      connectivity[5, 1]=6  
      connectivity[5, 2]=5  
      # dans le plan (x, yG + ly, z)
      connectivity[6, 0]=3  
      connectivity[6, 1]=4  
      connectivity[6, 2]=7  

      connectivity[7, 0]=4  
      connectivity[7, 1]=8  
      connectivity[7, 2]=7  
      # dans le plan (xG - lx, y, z)
      connectivity[8, 0]=1  
      connectivity[8, 1]=5  
      connectivity[8, 2]=8  

      connectivity[9, 0]=1  
      connectivity[9, 1]=8  
      connectivity[9, 2]=4  
      # dans le plan (xG + lx, y, z)
      connectivity[10, 0]=2  
      connectivity[10, 1]=3  
      connectivity[10, 2]=6  

      connectivity[11, 0]=3  
      connectivity[11, 1]=7  
      connectivity[11, 2]=6  

      # on peut alors ajouter son contacteur polygone a la brique
      body.addContactors(shape='POLYR', color=color, nb_vertices=8, nb_faces=12, 
         vertices=vertices, connectivity=connectivity)

      # on calcule le volume et l'inertie de la brique
      body.computeRigidProperties()

      # on renvoie le corps genere
      return body

# fonction qui deplace un contacteur CLxxx situe sur un noeud sommet,
# de sorte que la distance entre le noeud et le contacteur soit apab 
def _moveContactorCLxxx(coor, body, color, apab):
   """_moveContactorCLxxx(coor, body, color, apab):
   this function move a contactor CLxxx placed on a vertex
   so as to the distance between the vertex and the contactor is apab

   parameters:

   - coor: coordinates of the vertex
   - body: given body
   - color: color of the considered contactor
   - apab: distance between the vertex and the contactor,
     following the considerd line"""
   
   # on recupere l'indice du noeud ayant les coordonnees donnees 
   index=body.findNode(coor)
   # s'il n'esiste pas
   if index is None:
      # on affiche un warning
      showWarning("no node in this avatar have the searched coordinates")
      # on quitte la fonction
      return

   # pour chaque contacteur du corps
   for contact in body.contactors:
      # si le contacteur n'est pas un CLxxx
      if contact.shape != 'CLxxx':
         # on passe au suivant
         continue
      # si le contacteur n'a pas la bonne couleur
      if contact.color != color:
         # on passe au suivant
         continue
      # pour chaque element supportant le contacteur
      for ie, ele in enumerate(contact.elements):
         # on recupere la connectivite de l'element supportant
         # le contacteur
         connectivity = ele.connectivity
         # si le noeud cherche est dans la connectivite de l'element
         if index in connectivity:
            # si le noeud est le premier dans la connectivite, et
            # que l'abscisse du noeud est nulle
            if index == connectivity[0] and contact.weights[ie] == 0.:
               # la nouvelle abscisse est celle donnee en parametre 
               contact.weights[ie] = apab
            # si le noeud est le deuxieme dans la connectivite, et
            # que l'abscisse du noeud est egale a 1
            elif index == connectivity[1] and contact.weights[ie] == 1.:
               # la nouvelle abscisse est le complementaire de 
               # celle donnee en parametre 
               contact.weights[ie] = 1. - apab
