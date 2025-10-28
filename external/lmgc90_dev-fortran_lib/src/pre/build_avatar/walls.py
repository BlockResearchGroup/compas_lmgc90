# module fournissant des macros pour la generation de parois complexes :
# murs rugueux, tambours, etc...

import numpy
import math
from ..avatar.avatar import *
from ..avatar.bulk.rigid2d import *

# import du module qui calcule les coordonnees de points sur un reseau 2D, pour contruire des parois rugueuses 3D
from .lattices2D import *
from .tools.granulometry import *

# import du module permettant de savoir si on pourra importer les pre_tools
from ..utilities.check_compiled_modules import *

# import des fonctions pour tirer les particules suivant une granulo donnee

# si on peut essayer d'importer le module pre_tools sans tout faire planter
if import_lmgc90():
   # on essaye
   try:
      from ...chipy import lmgc90
   except:
      raise

# fonction qui cree un cluster de disques ou de polygones, pour faire un mur rugueux, en 2D
#   - center : position du centre d'inertie du mur
#   - l : longueur minimale du mur; le mur construit sera peut-etre plus grand
#         puisqu'on dispose un nombre entier de disques de rayon donne
#   - r : rayon d'encombrement d'une particule 
#   - model : modele rigide pour la particule
#   - material : materiau dont la particule est faite
#   - number : numero du corps
# variables optionnelles
#   - theta : rotation du mur autour de son centre d'inertie
#   - nb_vertex : si nb_vertex >= 3 les particules sont des polygones reguliers a nb_vertex cotes, 
#   - color : couleur d'un disque
def roughWall(center, l, r, model, material, theta=0., color='WALLx', nb_vertex=0, number=None):
   '''body=roughWall(center, l, r, model, material, theta=0., color='WALLx', nb_vertex=0, number=None):

   this function builds a 2D rough wall: it returns a body made of a cluster of disks or polygons

   parameters:  

   - center: mass center of the wall  in the global frame
   - l: minimal length of the wall, i.e. since the wall is made of
     disk having a given radius, it could be bigger than expected
   - r: radius of a particle
   - model: rigid model for the particle
   - material: the particle is made of this material

   optional parameters:

   - theta=0.: rotation angle in the inertial frame
   - color='WALLx': color of a disk contactor
   - nb_vertex=0: if nb_vertex is greater or equal to three, generated particles are
     regular polygons having nb_vertex vertices
   - number=None: index of the avatar (still present to ensure compatibility)'''

   # creation d'un nouveau corps rigide 2D
   body = avatar(dimension=2, number=number)
   # on cree un comportement volumique de type rigide
   body.addBulk( rigid2d() )
   # ajout de la position du centre d'inertie au disque
   body.addNode( node(coor=numpy.array(center), number=1) )
   # on definit les groupes pour le disque
   body.defineGroups()
   # on affecte son modele au disque
   body.defineModel(model=model)
   # on affecte son materiau au disque
   body.defineMaterial(material=material)

   # si le nombre de sommets donne est inferieur strictement a 3
   if nb_vertex < 3:
      # on place les contacteurs disques

      # N.B. : le centre d'inertie a ete place dans le repere global, les 
      #        contacteurs disque seront donc place par rapport au centre 
      #        d'inertie (repere local)
      # on calcul le nombre de disques necessaire
      nb_disk=int(math.floor(l/(2.*r))) + 1
      # on ajoute les contacteurs
      # si il y a un nombre impair de disques
      if nb_disk % 2 == 1:
         # on en pose un au milieu
         body.addContactors(shape='DISKx', color=color, byrd=r, shift=[0., 0.])
         # on pose les autres contacteurs autour
         for i in range(1, nb_disk//2 + 1, 1):
            # disque de gauche
            body.addContactors(shape='DISKx', color=color, byrd=r, shift=[-2*i*r, 0.])
            # disque de droite
            body.addContactors(shape='DISKx', color=color, byrd=r, shift=[2*i*r, 0.])
      # sinon
      else:
         # on pose les contacteurs de part et d'autre du centre d'inertie
         for i in range(0, nb_disk//2, 1):
            # disque de gauche
            body.addContactors(shape='DISKx', color=color, byrd=r, shift=[-(2*i + 1)*r, 0.])
            # disque de droite
            body.addContactors(shape='DISKx', color=color, byrd=r, shift=[(2*i + 1)*r, 0.])

   # sinon,               
   else:
      # on place des contacteurs polygones

      # on calcule le nombre de polygones necessaire
      nb_polyg=int(math.floor(l/(2.*r))) + 1

      # definition du contacteur pour le polygone :
      vertices = numpy.zeros([nb_vertex, 2], 'd')
      for i in range(0, nb_vertex, 1):
         vertices[i, 0] = r*math.cos(2.*math.pi*i/float(nb_vertex)) 
         vertices[i, 1] = r*math.sin(2.*math.pi*i/float(nb_vertex)) 

      # on ajoute les contacteurs
      # si il y a un nombre impair de polygone
      if nb_polyg % 2 == 1:
         # on en pose un au milieu
         body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertex, vertices=vertices)
         # on pose les autres contacteurs autour
         for i in range(1, nb_polyg//2 + 1, 1):
            # polygone de gauche

            # on declare le tableau qui va servir a stocker les coordonnees des sommets du contacteur polygone
            # courant
            shiftedvertices = numpy.zeros([nb_vertex, 2], 'd')
            # on calcule les coordonnees des sommets du polygone             
            shiftedvertices[:, 0] = vertices[:, 0] - 2*i*r
            shiftedvertices[:, 1] = vertices[:, 1]
            # on ajoute le contacteur polygone a la paroi
            body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertex, vertices=shiftedvertices)

            # polygone de droite

            # on declare le tableau qui va servir a stocker les coordonnees des sommets du contacteur polygone
            # courant
            shiftedvertices = numpy.zeros([nb_vertex, 2], 'd')
            # on calcule les coordonnees des sommets du polygone             
            shiftedvertices[:, 0] = vertices[:, 0] + 2*i*r
            shiftedvertices[:, 1] = vertices[:, 1]
            # on ajoute le contacteur polygone a la paroi
            body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertex, vertices=shiftedvertices)
      # sinon
      else:
         # on pose les contacteurs de part et d'autre du centre d'inertie
         for i in range(0, nb_polyg//2, 1):
            # polygone de gauche

            # on declare le tableau qui va servir a stocker les coordonnees des sommets du contacteur polygone
            # courant
            shiftedvertices = numpy.zeros([nb_vertex, 2], 'd')
            # on calcule les coordonnees des sommets du polygone             
            shiftedvertices[:, 0] = vertices[:, 0] - (2*i + 1)*r
            shiftedvertices[:, 1] = vertices[:, 1]
            # on ajoute le contacteur polygone a la paroi
            body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertex, vertices=shiftedvertices)

            # polygone de droite

            # on declare le tableau qui va servir a stocker les coordonnees des sommets du contacteur polygone
            # courant
            shiftedvertices = numpy.zeros([nb_vertex, 2], 'd')
            # on calcule les coordonnees des sommets du polygone             
            shiftedvertices[:, 0] = vertices[:, 0] + (2*i + 1)*r
            shiftedvertices[:, 1] = vertices[:, 1]
            # on ajoute le contacteur polygone a la paroi
            body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertex, vertices=shiftedvertices)
               
   # on calcule de la surface et de l'inertie du cluster
   body.computeRigidProperties()
   
   # si on donne un angle non nul
   if theta != 0.:
      # on tourne le corps autour de son centre d'inertie
      body.rotate(psi=theta, center=body.nodes[1].coor)

   return body

# fonction qui cree un cluster de disques ou de polygones, pour faire un mur legerement rugueux, en 2D
#   - center : position du centre d'inertie du mur
#   - l : longueur minimale du mur; le mur construit sera peut-etre plus grand
#         puisqu'on dispose un nombre entier de disques de rayon donne
#   - r : rayon d'encombrement d'une particule 
#   - model : modele rigide pour la particule
#   - material : materiau dont la particule est faite
#   - number : numero du corps
# variables optionnelles
#   - theta : rotation du mur autour de son centre d'inertie
#   - nb_vertex : si nb_vertex >= 3 les particules sont des polygones reguliers a nb_vertex cotes, 
#   - color : couleur d'un disque
def fineWall(center, l, r, model, material, theta=0., color='WALLx', nb_vertex=0, number=None):
   '''body=fineWall(center, l, r, model, material, theta=0., color='WALLx', nb_vertex=0, number=None):

   this function builds a 2D not too rough wall: it returns a body made of a cluster
   of disks or polygons but with contactors overlapping each others to reduce the roughness

   parameters:  

   - center: mass center of the wall  in the global frame
   - l: minimal length of the wall, i.e. since the wall is made of
     disk having a given radius, it could be bigger than expected
   - r: radius of a particle
   - model: rigid model for the particle
   - material: the particle is made of this material

   optional parameters:

   - theta=0.: rotation angle in the inertial frame
   - color='WALLx': color of a disk contactor
   - nb_vertex=0: if nb_vertex is greater or equal to three, generated particles are
     regular polygons having nb_vertex vertices
   - number=None: index of the avatar (still present to ensure compatibility)'''

   # creation d'un nouveau corps rigide 2D
   body = avatar(dimension=2, number=number)
   # on cree un comportement volumique de type rigide
   body.addBulk( rigid2d() )
   # ajout de la position du centre d'inertie au disque
   body.addNode( node(coor=numpy.array(center), number=1) )
   # on definit les groupes pour le disque
   body.defineGroups()
   # on affecte son modele au disque
   body.defineModel(model=model)
   # on affecte son materiau au disque
   body.defineMaterial(material=material)

   # si le nombre de sommets donne est inferieur strictement a 3
   if nb_vertex < 3:
      # on place les contacteurs disques

      # N.B. : le centre d'inertie a ete place dans le repere global, les 
      #        contacteurs disque seront donc place par rapport au centre 
      #        d'inertie (repere local)
      # on calcul le nombre de disques necessaire
      nb_disk=int(math.floor(l/(2.*r))) + 1
      # on ajoute les contacteurs
      # on en pose un au milieu
      body.addContactors(shape='DISKx', color=color, byrd=r, shift=[0., 0.])
      # on pose les autres contacteurs autour
      for i in range(1, nb_disk//2 + 1, 1):
         # disque de gauche
         body.addContactors(shape='DISKx', color=color, byrd=r, shift=[-2*i*r, 0.])
         body.addContactors(shape='DISKx', color=color, byrd=r, shift=[-(2*i - 1)*r, 0.])
        
         # disque de droite
         body.addContactors(shape='DISKx', color=color, byrd=r, shift=[2*i*r, 0.])
         body.addContactors(shape='DISKx', color=color, byrd=r, shift=[(2*i - 1)*r, 0.])
      
   # sinon,               
   else:
      # on place des contacteurs polygones

      # on calcule le nombre de polygones necessaire
      nb_polyg=int(math.floor(l/(2.*r))) + 1

      # definition du contacteur pour le polygone :
      vertices = numpy.zeros([nb_vertex, 2], 'd')
      for i in range(0, nb_vertex, 1):
         vertices[i, 0] = r*math.cos(2.*math.pi*i/float(nb_vertex)) 
         vertices[i, 1] = r*math.sin(2.*math.pi*i/float(nb_vertex)) 

      # on ajoute les contacteurs
      # on en pose un au milieu
      body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertex, vertices=vertices)
      # on pose les autres contacteurs autour
      for i in range(1, nb_polyg//2 + 1, 1):
         # polygone de gauche

         # on declare le tableau qui va servir a stocker les coordonnees des sommets du contacteur polygone
         # courant
         shiftedvertices = numpy.zeros([nb_vertex, 2], 'd')
         # on calcule les coordonnees des sommets du polygone             
         shiftedvertices[:, 0] = vertices[:, 0] - 2*i*r
         shiftedvertices[:, 1] = vertices[:, 1]
         # on ajoute le contacteur polygone a la paroi
         body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertex, vertices=shiftedvertices)

         # polygone de droite

         # on declare le tableau qui va servir a stocker les coordonnees des sommets du contacteur polygone
         # courant
         shiftedvertices = numpy.zeros([nb_vertex, 2], 'd')
         # on calcule les coordonnees des sommets du polygone             
         shiftedvertices[:, 0] = vertices[:, 0] + 2*i*r
         shiftedvertices[:, 1] = vertices[:, 1]
         # on ajoute le contacteur polygone a la paroi
         body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertex, vertices=shiftedvertices)

         # on pose les contacteurs de part et d'autre du centre d'inertie
         # polygone de gauche

         # on declare le tableau qui va servir a stocker les coordonnees des sommets du contacteur polygone
         # courant
         shiftedvertices = numpy.zeros([nb_vertex, 2], 'd')
         # on calcule les coordonnees des sommets du polygone             
         shiftedvertices[:, 0] = vertices[:, 0] - (2*(i-1) + 1)*r
         shiftedvertices[:, 1] = vertices[:, 1]
         # on ajoute le contacteur polygone a la paroi
         body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertex, vertices=shiftedvertices)

         # polygone de droite

         # on declare le tableau qui va servir a stocker les coordonnees des sommets du contacteur polygone
         # courant
         shiftedvertices = numpy.zeros([nb_vertex, 2], 'd')
         # on calcule les coordonnees des sommets du polygone             
         shiftedvertices[:, 0] = vertices[:, 0] + (2*(i-1) + 1)*r
         shiftedvertices[:, 1] = vertices[:, 1]
         # on ajoute le contacteur polygone a la paroi
         body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertex, vertices=shiftedvertices)
         
   # on calcule de la surface et de l'inertie du cluster
   body.computeRigidProperties()
   
   # si on donne un angle non nul
   if theta != 0.:
      # on tourne le corps autour de son centre d'inertie
      body.rotate(psi=theta, center=body.nodes[1].coor)

   return body


# fonction qui cree un cluster de briques (polygones), pour faire un mur lisse, en 2D
#   - center : position du centre d'inertie du mur
#   - l : longueur minimale du mur
#   - n : nombre de briques, dans la longeur du mur 
#   - model : modele rigide pour la particule
#   - material : materiau dont la particule est faite
#   - number : numero du corps
# variables optionnelles
#   - theta : rotation du mur autour de son centre d'inertie
#   - color : couleur d'un disque
def smoothWall(center, l, h, nb_polyg, model, material, theta=0., color='WALLx', number=None):
   '''body=smoothWall(center, l, h, nb_polyg, model, material, theta=0., color='WALLx', number=None):

   this function builds a 2D smooth wall: it returns a body made of a cluster of rectangular polygons

   parameters :  

   - center: mass center of the wall in the global frame
   - l: length of the wall
   - h: height of the wall
   - nb_polyg: number of bricks in the wall
   - model: rigid model for the wall
   - material: the wall is made of this material

   optional parameters:

   - theta=0.: rotation angle in the inertial frame
   - color='WALLx': color of a polygon contactor
   - number=None: index of the avatar (still present to ensure compatibility)'''

   # creation d'un nouveau corps rigide 2D
   body = avatar(dimension=2, number=number)
   # on cree un comportement volumique de type rigide
   body.addBulk( rigid2d() )
   # ajout de la position du centre d'inertie au mur
   body.addNode( node(coor=numpy.zeros(2, 'd'), number=1) )
   # on definit les groupes pour le mur
   body.defineGroups()
   # on affecte son modele au mur
   body.defineModel(model=model)
   # on affecte son materiau au mur
   body.defineMaterial(material=material)

   # on place les contacteurs polygones

   # on calcule la longueur d'une brique
   l_brick = 1.0*l/nb_polyg

   # N.B. : le centre d'inertie a ete place dans le repere global, les 
   #        contacteurs disque seront donc place par rapport au centre 
   #        d'inertie (repere local)
   # on ajoute les contacteurs
   # si il y a un nombre impair de briques
   if nb_polyg % 2 == 1:
      # on en pose une au milieu

      # on cree une matrice a la bonne taille
      vertices = numpy.zeros([4, 2], 'd')

      # on donne les coordonnees des sommets
      vertices[0, 0] = center[0] - 0.5*l_brick
      vertices[0, 1] = center[1] - 0.5*h
      vertices[1, 0] = center[0] + 0.5*l_brick
      vertices[1, 1] = center[1] - 0.5*h
      vertices[2, 0] = center[0] + 0.5*l_brick
      vertices[2, 1] = center[1] + 0.5*h
      vertices[3, 0] = center[0] - 0.5*l_brick
      vertices[3, 1] = center[1] + 0.5*h

      # on ajoute le contacteur
      body.addContactors(shape='POLYG', color=color, nb_vertices=4,
         vertices=vertices)

      # on pose les autres briques autour
      for i in range(1, nb_polyg//2 + 1, 1):
         # brique de gauche

         # on cree une matrice a la bonne taille
         vertices = numpy.zeros([4, 2], 'd')

         # on donne les coordonnees des sommets
         vertices[0, 0] = center[0] - i*l_brick - 0.5*l_brick
         vertices[0, 1] = center[1] - 0.5*h
         vertices[1, 0] = center[0] - i*l_brick + 0.5*l_brick
         vertices[1, 1] = center[1] - 0.5*h
         vertices[2, 0] = center[0] - i*l_brick + 0.5*l_brick
         vertices[2, 1] = center[1] + 0.5*h
         vertices[3, 0] = center[0] - i*l_brick - 0.5*l_brick
         vertices[3, 1] = center[1] + 0.5*h

         # on ajoute le contacteur
         body.addContactors(shape='POLYG', color=color, nb_vertices=4,
            vertices=vertices)

         # brique de droite

         # on cree une matrice a la bonne taille
         vertices = numpy.zeros([4, 2], 'd')

         # on donne les coordonnees des sommets
         vertices[0, 0] = center[0] + i*l_brick - 0.5*l_brick
         vertices[0, 1] = center[1] - 0.5*h
         vertices[1, 0] = center[0] + i*l_brick + 0.5*l_brick
         vertices[1, 1] = center[1] - 0.5*h
         vertices[2, 0] = center[0] + i*l_brick + 0.5*l_brick
         vertices[2, 1] = center[1] + 0.5*h
         vertices[3, 0] = center[0] + i*l_brick - 0.5*l_brick
         vertices[3, 1] = center[1] + 0.5*h

         # on ajoute le contacteur
         body.addContactors(shape='POLYG', color=color, nb_vertices=4,
            vertices=vertices)
   # sinon
   else:
      # on pose les contacteurs de part et d'autre du centre d'inertie
      for i in range(0, nb_polyg//2, 1):
         # brique de gauche

         # on cree une matrice a la bonne taille
         vertices = numpy.zeros([4, 2], 'd')

         # on donne les coordonnees des sommets
         vertices[0, 0] = center[0] - (i + 0.5)*l_brick - 0.5*l_brick
         vertices[0, 1] = center[1] - 0.5*h
         vertices[1, 0] = center[0] - (i + 0.5)*l_brick + 0.5*l_brick
         vertices[1, 1] = center[1] - 0.5*h
         vertices[2, 0] = center[0] - (i + 0.5)*l_brick + 0.5*l_brick
         vertices[2, 1] = center[1] + 0.5*h
         vertices[3, 0] = center[0] - (i + 0.5)*l_brick - 0.5*l_brick
         vertices[3, 1] = center[1] + 0.5*h

         # on ajoute le contacteur
         body.addContactors(shape='POLYG', color=color, nb_vertices=4,
            vertices=vertices)

         # brique de droite

         # on cree une matrice a la bonne taille
         vertices = numpy.zeros([4, 2], 'd')

         # on donne les coordonnees des sommets
         vertices[0, 0] = center[0] + (i + 0.5)*l_brick - 0.5*l_brick
         vertices[0, 1] = center[1] - 0.5*h
         vertices[1, 0] = center[0] + (i + 0.5)*l_brick + 0.5*l_brick
         vertices[1, 1] = center[1] - 0.5*h
         vertices[2, 0] = center[0] + (i + 0.5)*l_brick + 0.5*l_brick
         vertices[2, 1] = center[1] + 0.5*h
         vertices[3, 0] = center[0] + (i + 0.5)*l_brick - 0.5*l_brick
         vertices[3, 1] = center[1] + 0.5*h

         # on ajoute le contacteur
         body.addContactors(shape='POLYG', color=color, nb_vertices=4,
            vertices=vertices)
   
   # on calcule de la surface et de l'inertie du cluster
   body.computeRigidProperties()
   
   # si on donne un angle non nul
   if theta != 0.:
      # on tourne le corps autour de son centre d'inertie
      body.rotate(psi=theta, center=body.nodes[1].coor)

   # on renvoie le corps ainsi defini
   return body

# fonction qui cree un cluster de disque, pour faire un mur rugueux, en 2D
# en respectant la granulometrie de l'echantillon
#   - center   : position du centre d'inertie du mur
#   - l        : longueur minimale du mur; le mur construit sera peut-etre plus grand
#                puisqu'on dispose un nombre entier de disques de rayon donne
#   - rmin     : rayon min, pour la granulo
#   - rmax     : rayon max, pour la granulo
#   - model    : modele rigide pour la particule
#   - material : materiau dont la particule est faite
#   - number   : numero du corps
# variables optionnelles
#   - theta    : rotation du mur autour de son centre d'inertie
#   - color    : couleur d'un disque
#   - nb_vertex: nombre de sommet; si inf a 3 alors disque
#
def granuloRoughWall(center, l, rmin, rmax, model, material, theta=0., color='WALLx', nb_vertex=0, seed=None):
   '''body=granuloRoughWall(center, l, rmin, rmax, model, material, theta=0., color='WALLx', nb_vertex=0, seed=None):

   this function builds a 2D rough wall: it returns a body made of a cluster
   of disks or polygons. The radii of the particles defining the cluster are randomly distrubuted in the
   interval [rmin, rmax]

   parameters:  

   - center: mass center of the wall  in the global frame
   - l: minimal length of the wall, i.e. since the wall is made of
     disk having a given radius, it could be bigger than expected
   - rmin, rmax: bounds of the interval of radii defining the roughness of the wall
   - model: rigid model for the particle
   - material: the particle is made of this material

   optional parameters:

   - theta=0.: rotation angle in the inertial frame
   - color='WALLx': color of a disk contactor
   - nb_vertex=0: if nb_vertex is greater or equal to three, generated particles are
     regular polygons having nb_vertex vertices
   - seed=None: seed to use to control the randomness
   '''

   assert rmin <= rmax, "rmin must be inferior to rmax"
   assert rmin > 0., "rmin must be greater than 0."

   # creation d'un nouveau corps rigide 2D
   body = avatar(dimension=2)
   # on cree un comportement volumique de type rigide
   body.addBulk( rigid2d() )
   # ajout de la position du centre d'inertie au disque
   body.addNode( node(coor=numpy.array(center), number=1) )
   # on definit les groupes pour le disque
   body.defineGroups()
   # on affecte son modele au disque
   body.defineModel(model=model)
   # on affecte son materiau au disque
   body.defineMaterial(material=material)

   # on calcule le nombre de contacteurs sur la paroi
   nb_tactor=int(math.floor(l/(2.*rmin))) + 1

   # on tire aleatoirement une liste de rayons pour construire ces contacteurs
   radii=granulo_Random(nb_tactor, rmin, rmax, seed)

   # si le nombre de sommets donne est inferieur strictement a 3
   if nb_vertex < 3:
      # on place les contacteurs disques

      # N.B. : le centre d'inertie a ete place dans le repere global, les 
      #        contacteurs disque seront donc place par rapport au centre 
      #        d'inertie (repere local)

      # on initialise le decalage pour commencer par le premier disque de la paroi
      xshift = -0.5*l
      # on part du premier disque
      i = 0
      # tant qu'on a pas atteint la fin de la paroi
      while xshift < l*0.5:
         # on incremente le decalage du rayon de la particule courante pour que le decalage devienne
         # l'abscisse du centre d'inertie du polyogne
         xshift = xshift + radii[i] 
         # on choisit l'ordonnee du centre d'inertie du disque de sorte que son sommet tombe sur le 
         # bord superieur de la paroi
         yshift = rmax - radii[i]
         # on ajoute le contacteur disque a la paroi
         body.addContactors(shape='DISKx', color=color, byrd=radii[i], shift=[xshift, yshift])
         # on incremente le decalage du rayon de la particule, pour qu'il donne la position du bord
         # gauche de la nouvelle particule
         xshift = xshift + radii[i]
         # on passe au rayon suivant
         i = i + 1
   # sinon,
   else:
      # on place des contacteurs polygones

      # on initialise le decalage pour commencer par le premier polygone de la paroi
      xshift = -0.5*l
      # on part du premier polygone
      i = 0
      # tant qu'on a pas atteint la fin de la paroi
      while xshift < l*0.5:
         # on incremente le decalage du rayon du disque englobant de la particule courante pour que 
         # le decalage devienne l'abscisse du centre d'inertie du polygone
         xshift = xshift + radii[i]
         # on choisit l'ordonnee du centre d'inertie du disque de sorte que le sommet du disque englobant
         # la particule tombe sur le bord superieur de la paroi
         yshift = rmax - radii[i]

         # construction du contacteur polygone courant

         # on declare le tableau qui va servir a stocker les coordonnees des sommets du contacteur polygone
         # courant
         vertices = numpy.zeros([nb_vertex, 2], 'd')
         # pour chaque sommet du polygone
         for j in range(0, nb_vertex, 1):
            # on calcule l'abscisse du sommet courant
            vertices[j, 0] = radii[i]*math.cos(2.*math.pi*j/float(nb_vertex)) + xshift
            # on calcule l'ordonnee du sommet courant
            vertices[j, 1] = radii[i]*math.sin(2.*math.pi*j/float(nb_vertex)) + yshift
       
         # on ajoute le contacteur polygone a la paroi
         body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertex, vertices=vertices)
         # on incremente le decalage du rayon de la particule, pour qu'il donne la position du bord
         # gauche de la nouvelle particule
         xshift = xshift + radii[i]
         # on passe au rayon suivant
         i = i + 1
               
   # on calcule de la surface et de l'inertie du cluster
   body.computeRigidProperties()
   
   # si on donne un angle non nul
   if theta != 0.:
      # on tourne le corps autour de son centre d'inertie
      body.rotate(psi=theta, center=body.nodes[1].coor)
      
   return body

#########################################################################################################

# fonction qui contruit une paroi rugueuse 3D
def roughWall3D(center, lx, ly, r, model, material, color='WALLx'):
   '''body=roughWall3D(center, l, r, model, material, color='WALLx'):

   this function builds a 3D rough wall: it returns a body made of a cluster
   of spheres

   parameters:  

   - center: mass center of the wall in the global frame
   - lx: minimal length of the wall along x-axis, i.e. since the wall is made of
     spheres having a given radius, it could be bigger than expected
   - ly: minimal length of the wall along y-axis, i.e. since the wall is made of
     spheres having a given radius, it could be bigger than expected
   - r: radius of a particle
   - model: rigid model for the particle
   - material: the particle is made of this material

   optional parameters:

   - color='WALLx': color of a sphere contactor
   '''
   # on choisit un reseau triangulaire, avec des triangles orientes vers le bas
   orientation='down'

   # on calcule les caracteristiques du plus petit reseau capable de recouvrir le rectangle lx*ly
   #   * le nombre d'elements
   nb_ele=int(math.floor(lx/(2.*r))) + 1
   #   * le nombre de couches
   nb_layer=int(math.floor(2.*(ly/(2.*r) - 1)/math.sqrt(3.))) + 2

   # on calcule les coordonnees des points du reseau 2D, en positionnant son centre en (0, 0)
   bx = (nb_ele+1)*r*2
   by = (1+(nb_layer-1)*0.5*math.sqrt(3.))*r*2
   coor=triangularLattice2D(nb_ele=nb_ele, nb_layer=nb_layer, l=2.*r, x0=-0.5*bx, y0=-0.5*by, orientation=orientation)
   # just in case, this would be equivalent to compute the center and shift afterward:
   # instead of precomputing this bx,by...
   # coor[:,:] -= 0.5 * (numpy.amax(coor,axis=0)-numpy.amin(coor,axis=0)+2*r)

   # creation d'un nouvel avatar rigide 3D
   body = avatar(dimension=3)
   # on cree comportement volumique de type rigide
   body.addBulk( rigid3d() )
   # ajout de la position du centre d'inertie a la sphere
   body.addNode( node(coor=numpy.array(center), number=1) )
   # on definit les groupes pour la sphere
   body.defineGroups()
   # on affecte son modele a la sphere
   body.defineModel(model=model)
   # on affecte son materiau a la sphere
   body.defineMaterial(material=material)
   # pour chaque point du reseau
   for c in coor:
      # on ajoute un contacteur sphere au corps, centre sur le point courant du reseau
      body.addContactors(shape='SPHER', color=color, byrd=r, shift=[c[0], c[1], 0.])
   # on calcule du volume et de l'inertie du corps
   body.computeRigidProperties()

   # on renvoie le corps genere
   return body

# fonction qui contruit une paroi rugueuse 3D
def granuloRoughWall3D(center, lx, ly, rmin, rmax, model, material, color='WALLx', seed=None):
   '''body=granuloRoughWall3D(center, l, rmin, rmax, model, material, color='WALLx', seed=None):

   this function builds a 3D rough wall using the same granulometry than the sample: 
   it returns a body made of a cluster of spheres of different radius

   parameters:  

   - center: mass center of the wall in the global frame
   - lx: minimal length of the wall along x-axis, i.e. since the wall is made of
     spheres having a given radius, it could be bigger than expected
   - ly: minimal length of the wall along y-axis, i.e. since the wall is made of
     spheres having a given radius, it could be bigger than expected
   - rmin: minimal radius of a particle
   - rmax: maximal radius of a particle
   - model: rigid model for the particle
   - material: the particle is made of this material

   optional parameters:

   - color='WALLx': color of a sphere contactor
   - seed=None: seed to use to control the randomness
   '''

   assert rmin <= rmax, "rmin must be inferior to rmax"
   assert rmin > 0., "rmin must be greater than 0."

   nb_ele  = int(math.floor(lx/(2.*rmin))) + 1
   nb_layer= int(math.floor(ly/(2.*rmin))) + 1

   nb_points = nb_ele*nb_layer

   wradii = granulo_Random(nb_points, rmin, rmax, seed)

   # on calcule les coordonnees des points du reseau 2D, en poisitionnant son centre en (0, 0)
   coor=squareLattice2D(nb_ele=nb_ele, nb_layer=nb_layer, l=2.*rmin, x0=0.0*lx, y0=0.0*ly)

   body = avatar(dimension=3) # NEW 3D AVATAR
   body.addBulk( rigid3d() )  # RIGID BULK BEHAVIOUR
   # ajout de la position du centre d'inertie a la sphere
   body.addNode( node(coor=numpy.array(center), number=1) )
   # on definit les groupes pour la sphere
   body.defineGroups()
   # on affecte son modele a la sphere
   body.defineModel(model=model)
   # on affecte son materiau a la sphere
   body.defineMaterial(material=material)
   # pour chaque point du reseau

   for r, c in zip(wradii, coor):
      # on ajoute un contacteur sphere au corps, centre sur le point courant du reseau
      body.addContactors(shape='SPHER', color=color, byrd=r, shift=[c[0], c[1], r])
   # on calcule du volume et de l'inertie du corps
   body.computeRigidProperties()

   # on renvoie le corps genere
   return body

#########################################################################################################
# The Dallas team: Rudy/jr : 27.02.2013 
# Double paroi polygones/carres pour eviter que les particules ne s'echapent a travers les polygones

def granuloDoubleRoughWall(center, l, rmin, rmax, model, material, theta=0., color='WALLx', nb_vertices=0, seed=None):
   '''body=granuloDoubleRoughWall(center, l, rmin, rmax, model, material, theta=0., color='WALLx', nb_verices=0, seed=None):

   this function builds a 2D rough wall: it returns a body made of a cluster
   of disks or polygons. The radii of the particles defining the cluster are randomly distrubuted in the
   interval [rmin, rmax]

   parameters:  

   - center: mass center of the wall  in the global frame
   - l: minimal length of the wall, i.e. since the wall is made of
     disk having a given radius, it could be bigger than expected
   - rmin, rmax: bounds of the interval of radii defining the roughness of the wall
   - model: rigid model for the particle
   - material: the particle is made of this material

   optional parameters:

   - theta=0.: rotation angle in the inertial frame
   - color='WALLx': color of a disk contactor
   - nb_vertices=0: if nb_vertices is greater or equal to three, generated particles are
     regular polygons having nb_vertices vertices
   - seed=None: seed to use to control the randomness
   '''

   assert rmin <= rmax, "rmin must be inferior to rmax"
   assert rmin > 0., "rmin must be greater than 0."

   # creation d'un nouveau corps rigide 2D
   body = avatar(dimension=2)
   # on cree un comportement volumique de type rigide
   body.addBulk( rigid2d() )
   # ajout de la position du centre d'inertie au disque
   body.addNode( node(coor=numpy.array(center), number=1) )
   # on definit les groupes pour le disque
   body.defineGroups()
   # on affecte son modele au disque
   body.defineModel(model=model)
   # on affecte son materiau au disque
   body.defineMaterial(material=material)

   # on calcule le nombre de contacteurs sur la paroi
   nb_tactor=int(math.floor(l/(2.*rmin))) + 1

   # on tire aleatoirement une liste de rayons pour construire ces contacteurs
   radii=granulo_Random(nb_tactor, rmin, rmax, seed)

   # si le nombre de sommets donne est inferieur strictement a 3
   if nb_vertices < 3:
      # on place les contacteurs disques

      # N.B. : le centre d'inertie a ete place dans le repere global, les 
      #        contacteurs disque seront donc place par rapport au centre 
      #        d'inertie (repere local)

      # on initialise le decalage pour commencer par le premier disque de la paroi
      xshift = -0.5*l
      xshift_2 = -0.5*l
      nb_vertices_2 = 4

      # on part du premier disque
      i = 0
      # tant qu'on a pas atteint la fin de la paroi
      while xshift < l*0.5:
         # on incremente le decalage du rayon de la particule courante pour que le decalage devienne
         # l'abscisse du centre d'inertie du polyogne
         xlimit =  xshift + 2.0*radii[i]
         if(xlimit > l*0.5):
           radii[i] = radii[i] - (xlimit - l*0.5)*0.5
           
         xshift   = xshift   + radii[i]
         xshift_2 = xshift_2 + radii[i]

         # on choisit l'ordonnee du centre d'inertie du disque de sorte que son sommet tombe sur le 
         # bord superieur de la paroi

         yshift = rmax - radii[i]
         yshift_2 = rmax + rmax

         vertices_2 = numpy.zeros([nb_vertices_2, 2], 'd')

         for j in range(0, nb_vertices_2, 1):
           # on calcule l'abscisse du sommet courant
           vertices_2[j, 0] = rmax*math.cos(2.*math.pi*j/float(nb_vertices_2)+(math.pi/4)) + xshift_2
           # on calcule l'ordonnee du sommet courant
           vertices_2[j, 1] = rmax*math.sin(2.*math.pi*j/float(nb_vertices_2)+(math.pi/4)) + yshift_2
         # on ajoute le contacteur disque a la paroi
         body.addContactors(shape='DISKx', color=color, byrd=radii[i], shift=[xshift, yshift])
         if(xshift_2 <= 0.5*l):
            body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertices_2, vertices=vertices_2)
         # on incremente le decalage du rayon de la particule, pour qu'il donne la position du bord
         # gauche de la nouvelle particule
         xshift   = xshift   + radii[i]
         xshift_2 = xshift_2 + radii[i]

         # on passe au rayon suivant
         i = i + 1
   # sinon,
   else:
      # on place des contacteurs polygones

      # on initialise le decalage pour commencer par le premier polygone de la paroi
      xshift   = -0.5*l
      xshift_2 = -0.5*l

      nb_vertices_2 = 4

      # on part du premier polygone
      i = 0
      # tant qu'on a pas atteint la fin de la paroi
      while xshift < l*0.5:
         # on incremente le decalage du rayon du disque englobant de la particule courante pour que 
         # le decalage devienne l'abscisse du centre d'inertie du polygone
         xlimit =  xshift + 2.0*radii[i]
         if(xlimit > l*0.5):
           radii[i] = radii[i] - (xlimit - l*0.5)*0.5
        
         xshift = xshift + radii[i]
         xshift_2 = xshift_2 + radii[i]

         yshift   = 0.
         yshift_2 = rmax
         
         # construction du contacteur polygone courant
            
         # on declare le tableau qui va servir a stocker les coordonnees des sommets du contacteur polygone
         # courant
         vertices = numpy.zeros([nb_vertices, 2], 'd')

         vertices_2 = numpy.zeros([nb_vertices_2, 2], 'd')

         # pour chaque sommet du polygone
         for j in range(0, nb_vertices, 1):
           # on calcule l'abscisse du sommet courant
           vertices[j, 0] = radii[i]*math.cos(2.*math.pi*j/float(nb_vertices)) + xshift
           # on calcule l'ordonnee du sommet courant
           vertices[j, 1] = radii[i]*math.sin(2.*math.pi*j/float(nb_vertices)) + yshift
                  
         vertices_2[0, 0] = -radii[i] + xshift_2 ; vertices_2[0, 1] = -rmax  + yshift_2
         vertices_2[1, 0] =  radii[i] + xshift_2 ; vertices_2[1, 1] = -rmax  + yshift_2
         vertices_2[2, 0] =  radii[i] + xshift_2 ; vertices_2[2, 1] =  rmax  + yshift_2
         vertices_2[3, 0] = -radii[i] + xshift_2 ; vertices_2[3, 1] =  rmax  + yshift_2

         
         # on ajoute le contacteur polygone a la paroi
         
         body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertices, vertices=vertices)
         body.addContactors(shape='POLYG', color=color, nb_vertices=nb_vertices_2, vertices=vertices_2)
         # on incremente le decalage du rayon de la particule, pour qu'il donne la position du bord
         # gauche de la nouvelle particule
         xshift   = xshift   + radii[i]
         xshift_2 = xshift_2 + radii[i]
        
         # on passe au rayon suivant
         i = i + 1
               
   # on calcule de la surface et de l'inertie du cluster
   body.computeRigidProperties()
   
   # si on donne un angle non nul
   if theta != 0.:
      # on tourne le corps autour de son centre d'inertie
      body.rotate(psi=theta, center=body.nodes[1].coor)

   return body
