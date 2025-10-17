import math
from pathlib import Path
from copy import deepcopy

import numpy

try:
  from matplotlib import pyplot as plt
except ImportError:
  plt = None

from .nodes import *
from .bulks import *
from .bulk.rigid2d import *
from .bulk.rigid3d import *
from .bulk.element import *
from .contactors import *
from .groups import *
from .group import group
from ..config.lmgc90dicts import *
from ..utilities.error    import *

# pour la verification des types (modele et materiau)
from ..shared.bulk_behav import material as class_material
from ..shared.model import model as class_model

from .contactor.contactorFactory import contactorFactory

from ..build_avatar.mesh import mesh as class_mesh

## @class avatar
# attributs:  \n
#   atype is a string describing the numerical spatial discretization\n
#   nodes is a node iterator with number as key\n
#   bulks is a bulk iterator with number as key\n
#   contactors is a contactor iterator with number as key\n
#   groups is a group iterator with number as key\n
class avatar():

    ## constructor
    def __init__(self, dimension, number=None):
        """__init__(self, dimension, number=None):

        allow to define an avatar

        parameters:

        - self: the avatar itself
        - dimension: spatial dimension (2, in 2D, 3 in 3D)

        optional parameters:

        - number=None: index of the avatar (still present to ensure compatibility)
        """
        # si l'utilisateur attribue un numero a l'avatar
        if number != None:
           # on lui indique qu'il ne sera pas utilise
           showWarning('assign an index to an avatar is useless, and will be forbidden!')
        # le numero de l'avatar est inconnu pour l'instant et sera defini lors de son
        # ajout dans un container d'avatar (et eventuellement modifie, suite au passage dans
        # un nouveau container)
        # number is a numbering by avatar type (rigid or mesh)
        self.number = None
        # m_num is a numbering by physic type (mecaMAILx, therMAILx, etc)
        self.m_num  = None

        self.atype  = None

        # am : ajout de la notion de dimension, pour la gestion des mailles
        # si la dimension est impossible
        if dimension != 2 and dimension != 3 and dimension != 1:
           # on affiche un message d'erreur
           showError("spatial dimension must be 2 (in 2D) or 3 (in 3D)!")

        # si tous les tests sur la dimension ont reussi, on la stocke
        self.dimension = dimension

        self.nodes = nodes()
        self.bulks = bulks()
        self.contactors = contactors()
        self.groups = groups()
        #
        self.modelType = None
        self.drvDof = False
        self.iniDof = False
        self.iniGpv = False

    ## add one node
    # @arg Noeud a node 
    def addNode(self,Noeud):
        """ Usage : avatar.addNode(noeud)

        where noeud is a node object
        """
        # test paranoiaque de la dimension 
        if numpy.size(Noeud.coor) == self.dimension:
           self.nodes.addNode(Noeud)
        else:
           showError('incompatible size between coor array and dimension')

    ## add nodes of an iterator
    # @arg Noeuds node iterator
    def addNodes(self,Noeuds):
        """ Usage : avatar.addNodes(noeuds)

        where noeuds is a node iterator
        """
        for Noeud in Noeuds:
           self.addNode(Noeud)
       
    ## add one bulk
    # @arg Ele a bulk
    # @todo : check that bulk type matches avatar type
    def addBulk(self,Ele):
        """ Usage : avatar.addBulk(Ele)

        where Ele is a bulk
        """
        # on attribue au bulk le numero du prochain bulk
        # de l'avatar
        # N.B.: - la numerotation commence a 0
        #       - le numero d'un bulk est confondu avec son 
        #       indice dans le container de bulks attache a 
        #       l'avatar auquel il appartient
        Ele.number=len(self.bulks)
        # on l'ajoute dans le container de bulks de l'avatar
        self.bulks.addBulk(Ele)
       
        if isinstance(Ele,rigid2d):

          if self.dimension != 2:
            showError("Trying to assign a rigid 2D element to a 3D avatar!")

          if self.atype is None:
            self.atype = 'RBDY2'
          else:
            if self.atype=='MAILx':
              showError("Trying to assign a rigid 2D element to a meshed avatar!")

        elif isinstance(Ele,rigid3d):

          if self.dimension != 3:
            showError("Trying to assign a rigid 3D element to a 2D avatar!")

          if self.atype is None:
            self.atype = 'RBDY3'
          else:
            if self.atype=='MAILx':
              showError("Trying to assign a rigid 3D element to a meshed avatar!")

        elif isinstance(Ele,element):

          if self.atype is None:
            self.atype = 'MAILx'
          else:
            if self.atype!='MAILx':
              showError("Trying to assign a finite element to a rigid avatar!")

    ## add bulks of an iterator
    # @arg Eles bulks iterator
    def addBulks(self,Eles):
        """ Usage : avatar.addBulks(Eles)

        where Eles is a bulk iterator
        """
        for Ele in Eles:
            self.addBulk(Ele)

    # methode privee ajoute un contacteur a l'avatar, en lui affectant un numero
    def _addContactor(self, tact):
       """_addContactor(self, tact):

       this private method adds a contactor to the contactors container 
       attached to the avatar

       parameters:

       - self: the avatar itself
       - tact: the new contactor to be added
       """
       # on attribue au contacteur le numero du prochain contacteur
       # de l'avatar
       # N.B.: - la numerotation commence a 0
       #       - le numero d'un contacteur est confondu avec son 
       #       indice dans le container de contacteurs attache a 
       #       l'avatar auquel il appartient
       tact.number=len(self.contactors)
       # on l'ajoute dans le container de contacteurs de l'avatar
       self.contactors.addContactor(tact)
      
    # methode qui calcule les proprietes specifiques d'un corps rigide : masse, inertie et position du centre de gravite
    def computeRigidProperties(self):
       """computeRigidProperties(self):

       this function computes rigid properties of a rigid avatar : mass, inertia and mass center, from the contactors

       parameters:

       - self: the avatar itself
       """
        
       # si l'avatar n'est pas rigide
       if self.atype != 'RBDY2' and self.atype != 'RBDY3':
          # il n'est pas concerne par cette fonction
          showWarning("skipping deformable body")
          # on quitte la fonction
          return

       # si l'avatar n'a pas son bulk
       if len(self.bulks) == 0:
          # on ne peut pas encore calculer les proprietes du rigide
          showWarning("this avatar missing its bulk, skipping")
          # on quitte la fonction
          return

       # si l'avatar n'a pas encore son noeud
       if len(self.nodes) == 0:
          # on ne peut pas encore calculer les proprietes du rigide
          showWarning("this avatar missing its node, skipping")
          # on quitte la fonction
          return
          
       # si l'avatar n'a pas de contacteurs
       if len(self.contactors) == 0:
          # on ne peut pas encore calculer les proprietes du rigide
          showWarning("this avatar missing contactors, skipping")
          # on quitte la fonction
          return

       # ici, on a affaire a un rigide bien defini

       # si l'avatar est un rigide 2D
       if self.atype == 'RBDY2':
          # on cherche si on doit recalculer la position du centre d'inertie

          # on suppose initialement qu'il n'y a pas a la recalculer
          comp_node = False
          # si la position du centre d'inertie est nulle
          if numpy.allclose(self.nodes[1].coor, numpy.zeros(2, 'd'), atol=1e-6):
             # on indique qu'on va devoir la recalculer
             comp_node = True 

          # si l'avatar ne porte qu'un seul contacteur
          if len(self.contactors) == 1:
             # si on ne doit par recacluler la position du centre d'inertie,
             # mais que le shift du contacteur est non nul
             if not comp_node and not numpy.allclose(self.contactors[0].shift, numpy.zeros(2, 'd'), atol=1e-6):
                #fd
                #fd on est dans un cas non gere car si cooref /= (0.,0.) alors OG = (0. 0.) (i.e. le shift)
                #fd
                msg  = "Beware, concerning RBDY2 numbered " + str(self.number) + "\n"
                msg += "The vertices were defined in the inertia frame but ||OG|| /= 0 "
                msg += str(numpy.dot(self.contactors[0].shift, self.contactors[0].shift)) + '\n'
                msg += "The vertices should be defined either:\n"
                msg += "  in the inertia frame\n"
                msg += "  or in the absolute frame"
                showError(msg)
             # sinon,
             #fd
             #fd on est dans le cas ou on a volontairement decrit les vertex dans le repere 
             #fd absolu et pas dans le repere barycentrique
             #fd

             # si le seul contacteur porte par l'objet est un point
             if self.contactors[0].shape == 'PT2Dx':
                # on indique que ca n'a pas de sens
                msg  = "Beware, concerning RBDY2 numbered " + str(self.number) + "\n"
                msg += "There is only a point type contactor\n"
                msg += "This contactore does not allow to compute the surface and the inertial of the object\n"
                msg += "At least another contactor should be added to ask for the automatic computation\n"
                msg += "of the mass and the inertia of the object!" 
                showError(msg)

          # on calcule la surface totale de l'avatar
          # on initialise la surface totale a 0
          area = 0.
          # pour chaque contacteur
          for tact in self.contactors:
             # on ajoute la contribution du contacteur courant
             area += tact.area

          # si la surface calculee est nulle
          if area <= 1e-14:
             # on est dans le cas d'un contacteur special qui n'a pas de surface
             # e.g. disque creux ou point 2D, et le calcul d'un centre d'inertie
             # ou de inertie n'ont pas de sens

             #  on fixe l'avrd et le gyrd a 0.
             self.bulks[0].avrd = 0.
             self.bulks[0].gyrd = 0.
             # on quitte la fonction
             return

          # ici, on est sur que le corps a une surface non nulle

          # on en deduit l'avrd
          self.bulks[0].avrd = math.sqrt(area/math.pi)

          #fd
          #fd si necessaire on recalcule le centre d'inertie et on corrige le shift
          #fd
          if comp_node:
             # on calcule la postion du centre d'inertie a partir des shifts
             # on initialise la position du centre d'inertie a O
             OG=numpy.zeros(2, 'd')
             # pour chaque contacteur
             for tact in self.contactors:
                # on ajoute la contribution du contacteur courant
                OG += tact.area*tact.shift
             # on divise par la surface totale pour obtenir la position du centre d'inertie
             OG = OG/area

             # on corrige les shifts
             # pour chaque contacteur
             for tact in self.contactors:
                # on corrige le shift du contacteur courant
                tact.shift -= OG

             # on stocke la position du centre d'inertie
             self.nodes[1].coor = OG 

          # on recalcule l'inertie du corps

          # on initialise l'inertie totale a 0
          I = 0.
          # pour chaque contacteur
          for tact in self.contactors:
             # on calcule le carre de la norme du shift du contacteur courant
             d2 = numpy.dot(tact.shift, tact.shift)
             # on ajoute la contribution du contacteur courant (formule de Huygens)
             I += tact.I + tact.area*d2
   
          # on en deduit le gyrd
          self.bulks[0].gyrd = math.sqrt(I/area)

       # si l'avatar est un rigide 3D
       if self.atype == 'RBDY3':
          # on cherche si on doit recalculer la position du centre d'inertie

          # on suppose initialement qu'il n'y a pas a la recalculer
          comp_node = False
          # si la position du centre d'inertie est nulle
          if numpy.allclose(self.nodes[1].coor, numpy.zeros(3, 'd'), atol=1e-6):
             # on indique qu'on va devoir la recalculer
             comp_node = True 

          # si l'avatar ne porte qu'un seul contacteur
          if len(self.contactors) == 1:
             # si on ne doit par recalculer la position du centre d'inertie,
             # mais que le shift du contacteur est non nul
             if not comp_node and not numpy.allclose(self.contactors[0].shift, numpy.zeros(3, 'd'), atol=1e-6):
                # on est dans un cas non gere car si cooref /= (0., 0., 0.) alors OG = (0., 0., 0.) (i.e. le shift)
                msg  = "Beware, concerning RBDY3 numbered " + str(self.number) + "\n"
                msg += "The vertices were defined in the inertia frame but ||OG|| /= 0 "
                msg += str(numpy.dot(self.contactors[0].shift, self.contactors[0].shift)) + '\n'
                msg += "The vertices should be defined either:\n"
                msg += "  in the inertia frame\n"
                msg += "  or in the absolute frame"
                showError(msg)
             # sinon, on est dans le cas ou on a volontairement decrit les vertex dans le repere 
             # absolu et pas dans le repere barycentrique

             # si le seul contacteur porte par l'objet est un point
             if self.contactors[0].shape == 'PT3Dx':
                msg  = "Beware, concerning RBDY3 numbered " + str(self.number) + "\n"
                msg += "There is only a point type contactor\n"
                msg += "This contactore does not allow to compute the surface and the inertial of the object\n"
                msg += "At least another contactor should be added to ask for the automatic computation\n"
                msg += "of the mass and the inertia of the object!" 
                showError(msg)

          # on calcule le volume totale de l'avatar
          # on initialise le volume total a 0
          volume = 0.
          # pour chaque contacteur
          for tact in self.contactors:
             # on ajoute la contribution du contacteur courant
             volume += tact.volume

          # on en deduit l'avrd
          self.bulks[0].avrd = math.pow(0.75*volume/math.pi, 1./3.)

          # si necessaire on recalcule le centre d'inertie et on corrige le shift
          if comp_node:
             # on calcule la postion du centre d'inertie a partir des shifts
             # on initialise la position du centre d'inertie a O
             OG=numpy.zeros(3, 'd')
             # pour chaque contacteur
             for tact in self.contactors:
                # on ajoute la contribution du contacteur courant
                OG += tact.volume*tact.shift
             # on divise par le volume total pour obtenir la position du centre d'inertie
             OG = OG/volume

             # on corrige les shifts
             # pour chaque contacteur
             for tact in self.contactors:
                # on corrige le shift du contacteur courant
                tact.shift -= OG

             # on stocke la position du centre d'inertie
             self.nodes[1].coor = OG 

          # on recalcule l'inertie du corps

          # on initialise l'inertie totale a la matrice nulle
          I = numpy.zeros([3, 3], 'd')
          # pour chaque contacteur
          for tact in self.contactors:
             # assemblage des contributions des tactors inertie + vol*distance_axe  
   
             # on ajoute l'inertie du contacteur courant a l'inertie totale
             I += tact.I
   
             # contribution de la distance a l'axe aux termes diagonaux
             for i in range(0, 3):
                d = numpy.array(tact.shift)
                d[i] = 0.
                I[i, i] += tact.volume*numpy.dot(d, d)
   
             # contribution de la distance a l'axe aux termes extra-diagonaux
             d = tact.shift
             I[0, 1] -= tact.volume*d[0]*d[1]
             I[1, 0] -= tact.volume*d[0]*d[1]
             I[0, 2] -= tact.volume*d[0]*d[2]
             I[2, 0] -= tact.volume*d[0]*d[2]
             I[1, 2] -= tact.volume*d[1]*d[2]
             I[2, 1] -= tact.volume*d[1]*d[2]
   
          # diagonalisation de la matrice d'inertie
   
          # nettoyage de la matrice :
   
          # on initialise le nombre de termes au-dessus de la diagonale annulles
          nb=0
          # pour chaque ligne
          for i in range(0, 3):
             # pour chaque terme au-dessus de la diagonale
             for j in range(i + 1, 3):
                # si le terme extra-diagonal courant est negligeable devant le terme
                # diagonal courant
                if math.fabs(I[i, j]) < 1.e-14*math.fabs(I[i, i]): 
                   # on incremente le nombre de termes au-dessus de la diagonale
                   # annulles
                   nb += 1
                   # on annule le terme extra-diagonal au-dessus de la diagonale
                   I[i, j]=0.e0
                   # et le terme extra-diagonal au-dessous de la diagonale (matrice
                   # symetrique)
                   I[j, i]=0.e0
          
          # diagonalisation de la matrice
          
          # si la matrice "nettoyee" est deja diagonale
          if nb == 3:
             # on obtient immediatement les valeurs propres (valeurs diagonales)
             I_diag=numpy.array([ I[0,0], I[1,1], I[2,2] ])
             # et la matrice de passage (matrice identite)
             P=numpy.eye(3, 3)
          # sinon,
          else:
             # on appelle la routine de calcul des valeurs propres et vecteurs propres
             # disponible dans scipy 
             I_diag, P=numpy.linalg.eigh(I) 
             # on s'assure que le repere est direct en recalculant la troisieme direction
             # comme le produit vectoriel des deux premieres
             P[:, 2] = numpy.cross(P[:, 0], P[:, 1])
   
          # on stocke les inerties principales
          self.bulks[0].setInertia(I_diag)
    
          # on stocke le repere principal d'inertie
          self.bulks[0].setFrame(P)
  
          # on impose une condition initiale bidon (vitesse initiale nulle), pour
          # forcer l'ecriture du repere de l'objet dans le fichier BODIES.DAT
          self.imposeInitValue()
 
          # on exprime les shifts par rapport au repere principal d'inertie
          
          # pour chaque contacteur
          for tact in self.contactors:
             # on passe le shift du repere global au repere d'inertie
             tact.shift=numpy.dot(P.T, tact.shift)  
  
             # mise a jour les donnees du contacteur, connaissant l'orientation du repere principal d'inertie
             tact.updateFrame(P)
  
    # fonction qui verifie que tous les elements d'un avatar sont associes
    # a un modele et un element
    def checkModelAndMaterialDefinitions(self):
       """checkModelAndMaterialDefinitions(self):

       this functions checks if all volumic (or surfacic, in 2D) elements of the given avatar
       are associated to a model and a material. If any of the elements of tha given avatar is
       not associated to a model or a material it stops the exectution of the script.
       """
       # on suppose initialement que tous les elements volumiques (ou surfaciques, en 2D) :
       #    * portent un modele
       all_bulks_have_a_model=True
       #    * portent un materiau
       all_bulks_have_a_material=False

       # pour chaque element de l'avatar
       for ele in self.bulks:
          # si l'element est suppose porter un modele et un materiau
          if ele.etype in dimension2geoElement[self.dimension]:
             # si l'element ne porte pas de modele
             if ele.model is None:
                # on indique que tous les elements ne portent pas un modele
                all_bulks_have_a_model=False
                # on sort de la boucle
                break
             # si l'element ne porte pas de materiau
             if ele.material is None:
                # on indique que tous les elements ne portent pas un materiau
                all_bulks_have_a_material=False
                # on sort de la boucle
                break

       # si tous les elements ne portent pas de modele
       if not all_bulks_have_a_model:
          # on parcours les groupes de l'avatar
          for group in list(self.groups.keys()):
             # si l'avatar est maille, on laisse tomber le groupe "all"
             if self.atype == "MAILx" and group == "all":
                continue
             # on parcours les elements du groupe courant
             for ele in self.groups[group].bulks:
                # si l'element est suppose porter un modele et n'en porte pas
                if ele.etype in dimension2geoElement[self.dimension] and ele.model is None:
                   # on a trouve un groupe qui ne porte de modele
                   # et on construit un message d'erreur pour prvenir l'utilisateur
                   showError("bulks belonging to group \"" + group + "\" are not associated to any model!")

       # si tous les elements ne portent pas de materiau
       if not all_bulks_have_a_material:
          # on parcours les groupes de l'avatar
          for group in list(self.groups.keys()):
             # si l'avatar est maille, on laisse tomber le groupe "all"
             if self.atype == "MAILx" and group == "all":
                continue
             # on parcours les elements du groupe courant
             for ele in self.groups[group].bulks:
                # si l'element est suppose porter un materiau et n'en porte pas
                if ele.etype in dimension2geoElement[self.dimension] and ele.material is None:
                   # on a trouve un groupe qui ne porte de materiau
                   # et on construit un message d'erreur pour prvenir l'utilisateur
                   showError("bulks belonging to group \"" + group + "\" are not associated to any material!")

# ca doit appeler une routine dans nodes
    ## translate an avatar
    #  @param dx,dy, dz tranlation vector
    def translate(self,dx=0.,dy=0.,dz=0.):
        """ usage self.tranlsate(dx=0.,dy=0.,dz=0.)

        where dx,dy, dz are components of translation vector
        """
        for no in self.nodes:
             no.translate(dx=dx,dy=dy,dz=dz)

    ## scale an avatar
    #  @param scale
    def scale(self,scale=1.0):
        """ usage self.scale(scale = 1.0)

        where scale is the scale
        """
        for no in self.nodes:
             no.scale(scale=scale)

    def rotate(self, description='Euler', phi=0., theta=0., psi=0., alpha=0., axis=[0., 0., 1.], center=None):
        """
        This function rotates the considered avatar, according to the given rotation parameters and a
        rotation center. Supported rotation paramters are: Euler's angles or an axis and an angle

        parameters:

        - self: the avatar itself
        - description='Euler': defines the rotation parameters:

          - if description = 'Euler', the rotation uses Euler's angles, consequently only phi, theta, psi and
            center are considered
          - if description = 'axis', the rotation uses an axis and an angle, consequently only axis, alpha and
            center are considered
        - phi: first Euler's angle (rotation with respect to z-axis)
        - theta: second Euler's angle (rotation with respect to x-axis)
        - psi: third Euler's angle (rotation with respect to z-axis, the only one admissible in 2D)
        - axis: a 3D vector defining rotation axis (colinear to z-axis in 2D)
        - angle: rotation angle
        - center: rotation center

        N.B. all angles are in radians
        """
        # selon la parametrisation utilisee
        if description == 'Euler': # cas des angles d'Euler
           # test de coherence
           if (self.dimension == 2 and (phi != 0. or theta != 0.)):
              showError('in 2D only psi should be defined (rotation around z axis)')
   
           # construction de la matrice de rotation
           q1 = numpy.array([[math.cos(phi), -math.sin(phi), 0.],
                             [math.sin(phi),  math.cos(phi), 0.],
                             [0.,             0.,            1.]],'d')
           q2 = numpy.array([[1., 0.,               0.],
                             [0., math.cos(theta), -math.sin(theta)],
                             [0., math.sin(theta),  math.cos(theta)]],'d')
           q3 = numpy.array([[math.cos(psi), -math.sin(psi), 0.],
                             [math.sin(psi),  math.cos(psi), 0.],
                             [0.,             0.,            1.]],'d')
           q  = numpy.dot(numpy.dot(q1,q2),q3)

        elif description == 'axis': # cas ou on donne un axe de rotation
           # tests de coherence :
           #    * taille du vecteur
           if numpy.size(axis) != 3:
              showError('the given axis should be a 3D vector')
           #    * coherence dans le cas 2D
           if (self.dimension == 2 and (axis[0] != 0. or axis[1] != 0.)):
              showError('in 2D axis must be colinear with e_z (rotation around z axis)')

           # calcul du vecteur directeur de l'axe (i.e. l'axe normalise)
           n = axis/numpy.linalg.norm(axis)

           # construction de la matrice de rotation

           # construction de la matrice associe a l'operateur u |--> n ^ u, ou ^ est
           # le produit vectoriel
           cross = numpy.array([[ 0.,   -n[2],  n[1]],
                                [ n[2],  0.,   -n[0]],
                                [-n[1],  n[0],  0.]], 'd')
           # construction effective de la matrice de rotation (formule d'Olinde Rodrigues) :
           # Q = cos(alpha)*I + (1 - cos(alpha))*(n x n) + sin(alpha)*cross,
           # ou I est le tenseur identite et x le produit tensoriel 
           q  = math.cos(alpha)*numpy.eye(3) + \
                (1. - math.cos(alpha))*numpy.outer(n, n) + \
                math.sin(alpha)*cross

        # cas general
        else:
           # affiche un message d'erreur
           showError('unknown kind of parameters!')

        if center is None:
          center = numpy.zeros( [self.dimension] )

        if not isinstance(center, numpy.ndarray ):
          center = numpy.array(center)

        self.nodes.applyAffineMap(q[:self.dimension,:self.dimension], center)

        for bulk in self.bulks:
           # cas particulier des rigides
           if isinstance(bulk, rigid2d) or isinstance(bulk, rigid3d):
              # on tourne le repere local
              bulk.applyLinearMapToFrame(q)
              # on doit ecrire la condition initiale correspondante a la rotation
              self.imposeInitValue()

    # fonction qui transforme l'objet en son symetrique par rapport a un plan
    def mirror(self, n, center, atol=1.e-8):
        """
        This function changes an avatar to its symmetrical through the
        given plane

        parameters:

        - self the avatar itself
        - n: normal to the considered plane
        - center: a point beonging to the considered plane

        optional parameters:

        - atol: absolute tolerance
        """
        # on normalise le vecteur othogonal au plan
        norm = numpy.linalg.norm(n)
        if norm < atol:
           showError("mirror :: norm of n is too small! ")
        n = n/norm
 
        # calcul d'une base du plan
        
        #    * calcul d'un premier vecteur norme orthogonal a n

        # si le vecteur n est dans le plan (xOy)
        if abs(n[2]) < atol:
           # on construit la base comme dans le cas 2D
           t = numpy.array([-n[1], n[0], 0.], 'd')

           norm = numpy.linalg.norm(t)
           if norm < atol:
              showError("mirror :: norm of t is too small! ")
           t = t/norm
        # sinon
        else:
           # on construit la base en utilisant une methode "generale"
           t = numpy.array([0., 1., -n[1]/n[2]], 'd')

           norm = numpy.linalg.norm(t)
           if norm < atol:
              showError("mirrorByPlane :: norm of t is too small! ")
           t = t/norm
       
        ## methode utilisee dans LMGC90 (cf. mod_SPSPx)
        #t = numpy.array([-n[1]*n[2], -n[2]*n[0], 2.*n[0]*n[1]])
        #norm = numpy.linalg.norm(t)
        #if norm < atol:
        #   showError("mirror :: norm of t is too small! ")

        #    * calcul du troisieme vecteur de la base
        s = numpy.cross(n, t)

        # matrice de passage : (n, t, s) -> (x, y, z)
        #am : piege a con : numpy.array([n, t, s]) donne P^T et pas P...
        P = numpy.array([[n[0], t[0], s[0]],
                         [n[1], t[1], s[1]],
                         [n[2], t[2], s[2]]], 'd')

        # matrice representative de la symetrie dans la base (n, t, s)
        M_prime = numpy.array([[-1.,  0.,  0.],
                               [ 0.,  1.,  0.],
                               [ 0.,  0.,  1.]], 'd')

        # matrice representative de la symetrie dans la base (x, y, z)
        M = numpy.dot(P, numpy.dot(M_prime, P.T))

        # calcul du "centre" du plan comme un vecteur 3D
        if self.dimension == 2:
           Centre = numpy.array( [ center[0], center[1], 0. ], 'd' )
        else:
           Centre = numpy.array( [ center[0], center[1], center[2] ], 'd')

        # calcul du symetrique de chaque noeud par rapport au plan
        for nod in self.nodes:
           nod.applyAffineMapToCoor(M, center)

        for bulk in self.bulks:
           # cas des rigides
           if isinstance(bulk, rigid2d) or isinstance(bulk, rigid3d):
              # on tourne le repere local
              bulk.applyLinearMapToFrame(M)
              # # on doit ecrire la condition initiale correspondant a la symetrie
              self.imposeInitValue()
           # cas des dformables

           
           elif isinstance(bulk, element):
              # on retourne les elements
              bulk.connectivity.reverse()
           # cas par defaut
           else:
              showError('mirror :: unknown bulk type!')


    # fonction qui cherche un noeud dans l'avatar, a partir de ces coordonnees
    def findNode(self, coor, atol=1.e-8):
        """findNode(self, n, atol=1.e-8):

        this function return the index of the node which coordinates are 
        coor, subject to the absolute tolerance atol, if it exists; 
        None otherwise

        parameters:

        - self: the avatar
        - coor: the given coordinates
        - atol: absolute tolerance

        returned value:

        - index of the node, if it exists; None otherwise"""

        # on cree un objet numpy a partir des coordonnees
        coor_numpy = numpy.array(coor)
    
        # si le noeud n'a pas le bon nombre de coordonnees
        if coor_numpy.size != self.dimension:
           # on ne le trouvera pas
           return None

        # pour chaque noeud de l'avatar
        for nod in self.nodes:
           # si le neoud courant a les coorodnnees cherchees
           if numpy.allclose(coor_numpy, nod.coor, atol=atol):
              # on renvoie son indice
              return nod.number

        # si on arrive ici, on n'a pas trouve le noeud
        return None

#    ## add a group
#    # @arg Group a group
#    def addGroup(self,Group):
#        """ Usage : avatar.addGroup(Group)
#            where Group is a group
#        """
#        self.groups.addGroup(Group)
       
#    ## add Groups of an iterator
#    # @arg Groups group iterator
#    def addGroups(self,Groups):
#        """ Usage : avatar.addGroups(groups)
#            where groups is a group iterator
#        """
#        for Group in Groups:
#            self.groups.addGroup(Group)
# 
#
#
    def defineGroups(self):
        # A REVOIR L ENDROIT DE LA DEFINITION DES GROUPES
        """defineGroups(self)

        define groups linked to each elements
        ! beware the method is not robuste for too many mesh elements"""
        

        self.groups.addGroup(group.group('all'))

        # affectation du groupe all: valable pour les rigides, defo, etc
        for bulk in self.bulks:
            self.groups['all'].addBulk(bulk)
        for node in self.nodes:
            self.groups['all'].addNode(node)

        # test pour savoir si on est bien un MAILx
        if (self.atype != 'MAILx'):
          # fd affichage casse burne <- une variable de bavardage ? 
          #  print 'Only the group all will be defined'
          return

        listeGroupes     =  []
        listeTypeElement =  []
        
        for bulk in self.bulks:
            if bulk.etype  not in listeTypeElement:
              listeTypeElement.append(bulk.etype)
              self.groups.addGroup(group.group(bulk.etype))

            self.groups[bulk.etype].addBulk(bulk)
            for j in bulk.connectivity:
               self.groups[bulk.etype].addNode(self.nodes[j])
                    
            if bulk.physicalEntity not in listeGroupes:
                self.groups.addGroup(group.group(bulk.physicalEntity))
                listeGroupes.append(bulk.physicalEntity)

            self.groups[bulk.physicalEntity].addBulk(bulk)
            for j in bulk.connectivity:
              self.groups[bulk.physicalEntity].addNode(self.nodes[j])

    # fonction qui inidque si un groupe appartient a un avatar ou non
    def hasGroup(self, group):
        """hasGroup(self,group):

        this functions return "True" iff the given group belongs to the given avatar
        """
        return (group in list(self.groups.keys()))

    ## setting model
    def defineModel(self,group='all',model=None):
        """defineModel(self,group='all',model=None)

        match a model (class model) or a set of models (class model) to a group
        """
        # si le groupe demande n'existe pas
        if not self.hasGroup(group):
           # on affiche un message d'erreur
           showError('group: ' + group + ' do not belong to this avatar!')

        # verification de la validite du modele
        
        # si le modele n'est pas une instance de la classe modele
        if not isinstance(model, class_model):
           # on affiche un message d'erreur
           showError("given model must be a model instance!")

        # si la dimension du modele differe de celle de l'avatar
        if self.dimension != model.dimension:
           # on affiche un message d'erreur
           msg  = "model dimension differs from avatar dimension :\n"
           msg += "   avatar : " + str(self.dimension) + "D\n"
           msg += "   model  : " + str(model.dimension) + "D"
           showError(msg)

        # si l'element fini est incomptible avec l'avatar
        #    * cas de l'element fini classique avec un rigide
        # si on refile un modele base sur element fini classique a un avatar rigide
        if (self.atype == 'RBDY2' or self.atype == 'RBDY3') and not model.element in rigidElements:
           # on affiche un message d'erreur
           showError("a standard finite element cannot be handled by a rigid avatar!")
        # si on refile un element fini rigide a un avatar deformable
        if self.atype == 'MAILx' and model.element in rigidElements:
           # on affiche un message d'erreur
           showError("a rigid finite element cannot be handled by a meshed avatar!")

        # si un modele a deja ete affecte a une partie des elements de l'avatar
        # et que le modele qu'on s'apprete a affecter est d'un type different
        if self.modelType != None and model.physics != self.modelType:
           # on affiche un message d'erreur
           showError("Different types of models cannot be associated to the same avatar!")

        # on initialise le nombre de bulks auxquels on a associe le modele
        # donne
        nb_model_defined_bulks=0
        # pour chaque bulk, associe au groupe group 
        for bulk in self.groups[group].bulks:
           try:
              # on tente d'associer le modele donne a l'element courant
              bulk.defineModel(model)
              # si on a pu associer le modele donne a l'element courant
              #    * on incremente le nombre de bulks auxquels on a associe le
              #      modele donne
              nb_model_defined_bulks += 1
              #    * on definit les ddl pour tous les noeuds de l'element
              # pour chaque noeud 
              for node in bulk.connectivity:
                 # on definit les ddl en fonction du modele 
                 self.nodes[node].defineDof(model)
           # si l'association du modele a l'element courant a leve une exception
           except ValueError as e:
              # si ce n'est pas l'exception attendue, i.e. celle prevue dans
              # la methode defineModel de la classe bulk, on la renvoie
              if not str(e).startswith("Cannot add model"):
                 raise

        # si le modele donne n'a pu etre associe a aucun element de l'avatar
        if nb_model_defined_bulks == 0:
           # on affiche un message d'erreur
           msg  = "model with element " + model.element
           msg += " can not be associated to the group " + group + " of the considered avatar"
           showError(msg)

        # on garde le type de model associe a l'avatar
        self.modelType = model.physics
 
    ## set the material of an element
    #
    def defineMaterial(self,group='all',material=None):
        """ defineMaterial(self,group='all',material=None)

        associate a material or a material container to the element
        """
        # si le groupe demande n'existe pas
        if not self.hasGroup(group):
           # on affiche un message d'erreur
           showError('group: ' + group + ' do not belong to this avatar!')

        # verification de la validite du materiau
        
        # si le materiau n'est pas une instance de la classe materiau
        if not isinstance(material, class_material):
           # si le materiau est une chaine
           if isinstance(material, str):
              # on affiche un message d'erreur pour les vieux
              showError("Sorry, using strings as materials is no longer supported!\nYou must use an object \"material\"") 
           # sinon,
           else:
              # on affiche un message plus brutal
              showError("material must be a material instance!")

        # ici, on est sur que le materiau est un objet de type material

        # si le materiau est defini en externe (i.e. un USER_MAT)
        if material.materialType == 'USER_MAT':
           # on affiche un message inidquant que les tests de coherence materiau/modele sont desactives
           showWarning("Consistancy tests will not be performed since the given material is user defined!")

        # on initialise le nombre de bulks auxquels on a associe le modele
        # donne
        nb_material_defined_bulks=0
        # pour chaque bulk, associe au groupe group 
        for bulk in self.groups[group].bulks:
           try:
              # on tente d'associer le materiau donne a l'element courant
              bulk.defineMaterial(material)
              # si on a pu associer le modele donne a l'element courant,
              # on incremente le nombre de bulks auxquels on a associe le
              # modele donne
              nb_material_defined_bulks += 1
           # si l'association du materiau a l'element courant a leve une 
           # exception
           except ValueError as e:
              # si ce n'est pas l'exception attendue, i.e. celle prevue dans
              # la methode defineMaterial de la classe bulk, on la renvoie
              if not str(e).startswith("Cannot add material"):
                 raise

        # si le materiau donne n'a pu etre associe a aucun element de l'avatar
        if nb_material_defined_bulks == 0:
           # on affiche un message d'erreur
           msg  = "material " + material.nom + " cannot be associated "
           msg += "to the group " + group + " since no bulk of this group is "
           msg += "associated to a model"
           showError(msg)

    ## build a string to represent the avatar            
    def __str__(self):
      impr ='avatar number : %10s of type %5s' %(self.number, self.atype)
      return impr

    ## add contactors
    # @arg Tact a contactor
    def addContactors(self,shape,color,group='all',**options):
        """addContactors(self,shape,color,group='all',**options)

        define contactors for the elements of a group

        parameters:

        - self: the contactor itself
        - shape: type of the contactors
        - color: color of the contactor

        optional parameters:

        - group='all': name of the considered group
        - ** options': a set of options associated to the contactors
        """
        # si le groupe demande n'existe pas
        if not self.hasGroup(group):
           # on affiche un message d'erreur
           showError('group: ' + group + ' do not belong to this avatar!')

        # si la dimension du contacteur differe de celle de l'avatar
        if not shape in dimension2contactor[self.dimension]:
           # on construit un message d'erreur rappelant les types disponibles
           msg = "Contactor type not available in " + str(self.dimension) + "D\n the available contactors in " + str(self.dimension) + "D are:\n"
           for i in dimension2contactor[self.dimension]:
               msg+=i+'\n'
           # on l'affiche
           showError(msg)

        # si le type de contacteur est incomptible avec l'avatar
        #    * cas du contacteur element fini avec un rigide
        # si on refile un contacteur base sur element fini a un avatar rigide
        if (self.atype == 'RBDY2' or self.atype == 'RBDY3') and not shape in rigidContactor:
           # on construit un message d'erreur rappelant les types disponibles
           msg = "Contactor type not available for a rigid avatar\n the available contactors for a rigid avatar are:\n"
           for i in rigidContactor:
               msg+=i+'\n'
           # on l'affiche
           showError(msg)
        #    * cas du contaceur rigide (autre que le POLYD) avec un maillage element fini
        # si on refile un contacteur rigide a un avatar deformable
        if self.atype == 'MAILx' and shape in rigidContactor:
           # on construit un message d'erreur rappelant les types disponibles
           msg = "Contactor type not available for a meshed avatar\n the available contactors for a meshed avatar are:\n"
           for i in listeContactor:
              if i not in rigidContactor:
                 msg+=i+'\n'
           # on l'affiche
           showError(msg)

        # on recupere les clefs du dictionnaires donnant les options du contacteur
        cles = list(options.keys())

        # si le contacteur est une ligne ou une surface candidate
        if shape == 'CLxxx' or shape == 'CSxx3' or shape == 'CSxx4':
           # si on ne donne pas de poids pour placer le contacteur
           if 'weights' not in cles:
              # on affecte la valeur None a l'option weights, pour activer le positionnement automatique des candidats aux noeuds
              options['weights']=None
 
        # on initialise une liste vide
        list_ele = []
        # pour chaque element du groupe
        for ele in self.groups[group].bulks: 
           # on ajoute l'element a la liste
           list_ele.append(ele)

        # on construit un seul nouveau contacteur avec tous les elements du groupe
        # N.B.: la copie profonde de la liste d'elements est effectuee par chaque constructeur de contacteur maille,
        #       il est donc inutile de le faire ici
        contact=contactorFactory(shape=shape, elements=list_ele, color=color, **options)
        if isinstance(contact, list):
          for c_ in contact :  
            # une fois le contacteur entierement defini, on l'ajoute au container de contacteurs de l'avatar
            self._addContactor(c_)
            # et au container de contacteurs portes par le groupe
            self.groups[group].addContactor(c_)
        else :    
          # une fois le contacteur entierement defini, on l'ajoute au container de contacteurs de l'avatar
          self._addContactor(contact)
          # et au container de contacteurs portes par le groupe
          self.groups[group].addContactor(contact)

    def imposeDrivenDof(self,group='all',component=1, description='predefined',ct=0.,amp=0.,omega=0.,
                        phi=0.,rampi=1.,ramp=0.,evolutionFile='', dofty='temp', plot_time_range=None):
        """
        This function associate a boundary condition to each node of the considered group of the
        considered avatar

        parameters:

        - dofty : type of boundary condition (vlocy for velocity, force for.. force, temp for temperature and flux for heat flux)
        - description : 'predefined' or 'evolution'
        - plot_time_range: if provided, must have two values to display the boundary condition

          * 'predefined': ct, amp, omega, phi, rampi and ramp must be defined
            so that the boundary value will be : [ ct + amp * cos(omega*t+phi) ] * min(1, rampi+ramp*t),
            where t is the time
          * 'evolution' : evolutionFile must be defined
            the input file must contain two columns separated by a blank character. First columns is time
            and the second one the imposed value. A linear interpolation is made by LMGC90 to compute values
            at computational times different from those given in input 
        """

        # some sanity/paranoid checks

        if not self.hasGroup(group):
           showError('group: ' + group + ' do not belong to this avatar!')

        # on verifie la liste des composantes

        if not isinstance(component, list):
           component = [component]

        if self.modelType is None:
           showError("You must define the model of the considered avatar before impose driven dof!")

        if len(component) == 0:
           showError("The given list of components is empty!")
 
        # on pour chaque composante
        for comp in component:
            if not isinstance(comp, int):
               showError("a component must be an integer!")

        if min(component) < 1:
           showError("a component must be larger or equal to 1!")


        # on verifie le type de ddl impose

        # si on ne reconnait pas le type de ddl impose
        if not dofty in model2dofty[self.modelType]:
           # on construit un message d'erreur rappelant les types disponibles
           msg = 'Unknown degree of freedom type\n must be among:\n'
           for i in model2dofty[self.modelType]:
              msg+=i+'\n'
           # on l'affiche
           showError(msg)

        if plot_time_range is not None and plt is not None:
          assert len(plot_time_range) == 2, 'plot_time_range must have two values for the display range'
          t = numpy.linspace(plot_time_range[0], plot_time_range[1], 10000, endpoint=True)
          if description == 'predefined':
            cl = ( ct + amp * numpy.cos( omega*t + phi ) ) * numpy.minimum(rampi+t*ramp, 1.)
            plt.plot(t, cl)
            plt.show()
          else:
            # ugly attempt to find the evolution file
            ef = Path(evolutionFile)
            ef = ef if ef.is_file() else 'DATBOX'/ef
            if ef.is_file():
              cl = numpy.loadtxt(ef)
              cl = numpy.interp(t, cl[:,0], cl[:,1])
              plt.plot(t, cl)
              plt.show()

        self.drvDof = True

        for no in self.groups[group].nodes:
            if no.dof is None:
               showError("no dof defined. It's defined when you define a model.")
   
            no.imposeDrivenDof(component,description, ct,amp,omega,phi,rampi,
                               ramp,evolutionFile,dofty)

    def relaxDrivenDof(self, group='all', component=1):
        """relaxDrivenDof(self, group='all', component=1):

        this function relax driven dof
        """
        # si le groupe demande n'existe pas
        if not self.hasGroup(group):
           # on affiche un message d'erreur
           showError('group: ' + group + ' does not belong to this avatar!')

        # on verifie la liste des composantes

        # si la liste des composantes n'est pas une liste
        if not isinstance(component, list):
           # on la transforme en liste
           component = [component]

        # si l'avatar ne connait pas son type de modele
        if self.modelType is None:
           # on affiche un message d'erreur
           showError("You must define the model of the considered avatar before impose driven dof!")

        # si la liste des composantes est vide
        if len(component) == 0:
           # on affiche un message d'erreur
           showError("The given list of components is empty!")
 
        # on pour chaque composante
        for comp in component:
            # si la composante courante n'est pas un entier
            if not isinstance(comp, int):
               # on affiche un message d'erreur
               showError("a component must be an integer!")

        # si la plus petite composante consideree est plus petite que 1
        if min(component) < 1:
           # on affiche un message d'erreur
           showError("a component must be larger or equal to 1!")

        # ici, on est sur que la liste des composantes est bonne

        # pour chaque noeud du groupe
        for no in self.groups[group].nodes:
           # si le noeud courant ne porte pas d'objet dof
           if no.dof is None:
              # on affiche un message d'erreur
              showError("no dof defined. It's defined when you define a model.")
   
           # on relaxe la condition limite porte par le ddl du noeud, suivant la composante donnee
           no.relaxDrivenDof(component)

    def imposeInitValue(self,group='all',component=1,value=0.):
        """imposeInitValue(self,group='all',component=1,value=0.)

        impose a value to a degree of freedom
        """
        # si le groupe demande n'existe pas
        if not self.hasGroup(group):
           # on affiche un message d'erreur
           showError('group: ' + group + ' does not belong to this avatar!')

        # si la liste des composantes n'est pas une liste
        if not isinstance(component, list):
           # on la transforme en liste
           component = [component]
        # si la liste des valeurs n'est pas une liste 
        if not isinstance(value, list):
           # on la transforme en liste
           value     = [value]

        # si la liste des composantes n'est pas de la meme taille que
        # la liste des valeurs
        if len(component) != len(value):
           # on affiche un message d'erreur
           showError("You must define a value for each component!")

        # si la liste des composantes est vide
        if len(component) == 0:
           # on affiche un message d'erreur
           showError("The given list of components is empty!")
 
        # on enumere la liste des composantes
        for indic, comp in enumerate(component):
            # si la composante courante n'est pas un entier
            if not isinstance(comp, int):
               # on affiche un message d'erreur
               showError("a component must be an integer!")
            # on tente de convertir la valeur associee a la composante courante en reel
            try:
               value[indic] = float(value[indic])
            # si on echoue
            except:
               # on affiche un message d'erreur
               showError("a value must be a float!")

        # si la plus petite composante consideree est plus petite que 1
        if min(component) < 1:
           # on affiche un message d'erreur
           showError("a component must be larger or equal to 1!")

        ## ici, on est sur que tout est bon
 
        # on indique que l'avatar porte une condition initiale sur certains noeuds
        self.iniDof = True
            
        for no in self.groups[group].nodes:
            if no.dof is None:
               showError("no dof defined. It's defined when you define a model.")
   
            no.imposeInitValue(component,value)
 
    # fonction qui construit un nouveau groupe comme le sous-groupe
    # d'un groupe existant
    def addGroupUsingPredicate(self, name, predicate, super_group='all'):
       """adddGroupUsingPredicate(self, name, predicate, super_group='all'):

       this function builds and add a new group to the avatar, by
       selecting nodes verifying a given predicate. All nodes
       verifying the predicate are added to the new group.
       If all nodes supporting an element are in the new group, this
       element is also added to the new group.

       parameters:

       - self : the avatar itself
       - name : the name of the new group
       - predicate : the given predicate defined as a function
         of the coordinates returning a boolean

       optional parameters:

       - super_group='all' : the new group is defines as a subgroup of this group
       """
       # on verifie que le nom du nouveau groupe soit une chaine de caracteres
       
       # si le nom du nouveau groupe n'est pas une chaine de caracteres
       if not isinstance(name, str):
          # on affiche un message d'erreur
          showError("A group name must be a string!")

       # si le groupe demande n'existe pas
       if not self.hasGroup(super_group):
          # on affiche un warning
          showError('group: ' + super_group + ' does not belong to this avatar!\nThe new group ' + name + ' cannot be built!')

       # ici, on est sur que le nom du groupe est correct
 
       # on cree le nouveau groupe
       sub_group=group.group(name)

       # tri des noeuds
       
       # on definit la liste des indices des noeuds appartenant au nouveau groupe a vide
       new_numbers=[]
       # pour chaque neoud du groupe existant
       for nod in self.groups[super_group].nodes:
          # on essaye de tester les coordonnees du noeud courant
          try:
             # on teste si les coordonnees du noeud courant verifient
             # le predicat
             is_verfied = predicate(nod.coor)
          # si on echoue
          except:
             # on affiche un message d'erreur
             showError("Applying the given predicate on the nodes of this avatar raised an exception!\nPlease check your predicate (pay attention to the dimension)")
             ## on quitte le programme
             #sys.exit(0)

          # ici, on est sur que le predicat a pu etre applique et on peu analyser le resultat
          
          # si les coordonnees du noeud verifient le predicat
          if is_verfied:
             # on ajoute le noeud au groupe
             sub_group.addNode(nod)
             # on ajoute le numero du noeud a la liste des numeros de noeuds du nouveau groupe
             new_numbers.append(nod.number)

       # si l'ensemble des noeuds du nouveau sous-groupe est vide
       if len(sub_group.nodes) == 0:
          # on affiche un warning
          showWarning("The new group " + name + " is empty!")
       # sinon,
       else:
          # tri des elements

          # pour chaque element du groupe existant
          for ele in self.groups[super_group].bulks:
             # on supppose initialement que l'element fait partie du nouveau groupe
             # i.e. s'appuie sur un ensemble de noeud inclus dans l'ensemble de noeuds du nouveau groupe
             in_new=True
             # pour chaque noeud de la connectivite de l'element
             for num in ele.connectivity:
                # si le noeud courant ne fait pas partie du nouveau groupe
                if not num in new_numbers:
                   # on indique que l'element ne saurait faire partie du nouveau groupe
                   in_new=False
                   # on sort de la boucle
                   break

             # si l'element fait partie du nouveau groupe
             if in_new:
                # on l'ajoute au nouveau groupe
                sub_group.addBulk(ele)

          # si l'ensemble des elements du nouveau sous-groupe est vide
          if len(sub_group.bulks) == 0:
             # on affiche un warning
             showWarning("The new group " + name + " contains no element!")

          # on ajoute le groupe a l'avatar <- si il n est pas vide !!
          self.groups.addGroup(sub_group)
          
    # fonction qui renvoie la liste des noeuds d'un groupe physique
    # existant
    def getNodebyGroup(self, group):
       """getNodebyGroup(self,group):
       this functions return the node list by physical group to this avatar
       """
       # on teste si le groupe physique existe pour cet avatar
       if self.hasGroup(group):
          listnode = []
          for node in self.groups[group].nodes:
             listnode.append(node.number)
       else :
          showError('group: ' + group + ' do not belong to this avatar!')
          
       return numpy.array(listnode)

    def getNodeCoor(self,noid=1):
       """
       Get the real coordinates of a node
       """
       return self.nodes[noid].coor + self.nodes[noid].dof.disp

    def getBulkFrame(self,elid=0):
        """
        Get the real orientation of an element
        """
        ele = self.bulks[elid]
        frame = ele.axis
        node  = self.nodes[ele.connectivity[0]]
        if node.dof.rot is not None:
            frame = numpy.matmul( node.dof.rot, frame)
        return frame

    def updateReferenceConfig(self):
        """
        Use current configuration as a reference configuration
        """
        # managing translation
        for node in self.nodes:
            node.coor[:] += node.dof.disp
            node.dof.disp[:] = 0.

        # managing rotation... rigid3d only
        for ele in self.bulks:
          if isinstance(ele, rigid3d):
            node  = self.nodes[ele.connectivity[0]]
            if node.dof.rot is not None:
              frame = numpy.matmul( node.dof.rot, ele.axis)
              ele.axis[:,:] = frame
              node.dof.rot[:,:] = numpy.eye(3, dtype=float)


# construction d'un avatar a partir d'un maillage
def buildMeshedAvatar(mesh, model, material):
   """buildMeshedAvatar(mesh, model, material):

   this function builds a meshed avatar from the mesh.

   parameters:

   - mesh: a given mesh
   - model: a given model
   - material: a given material
   """
   if not isinstance(mesh, class_mesh):
      showError("The given mesh is not a mesh!")

   # on cree un avatar maille
   body = avatar(dimension=mesh.dimension)
   # on renumerote les noeuds du maillage en utilisant leur rang
   mesh.rankRenumbering()
   # on ajoute les noeuds du maillage a l'avatar maille
   body.addBulks(deepcopy(mesh.bulks))
   # on ajoute les elements du maillage a l'avatar maille
   body.addNodes(deepcopy(mesh.nodes))
   # on definit les groupes pour l'avatar maille
   body.defineGroups()
   # on affecte son modele a l'avatar maille
   body.defineModel(model=model)
   # on affecte son materiau a l'avatar maille
   body.defineMaterial(material=material)

   # on renvoie l'avatar ainsi construit
   return body

