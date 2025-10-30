import numpy

from ...shared.bulk_behav  import *
from ...shared.model       import *

from ...config.lmgc90dicts import *

from ...utilities.error    import *

## @class bulk
#
# description 
class bulk():

    def __init__(self, elem_dim, connectivity, physicalEntity='1', geometricalEntity='1', number=None, nbNodes=None):
        """ __init__(self, elem_dim, connectivity, physicalEntity='1', geometricalEntity='1', number=None, nbNodes=None)

        this function initializes a new bulk

        N.B.: a bulk is always a finite element; a rigid body have a single finite element, which geometrical support
        is a point

        parameters:

        - self: the bulk itself
        - elem_dim: dimension of the element (volumic=3, surfacic=2, lineic=1, point=0)
        - connectivity: connectivity of the element

        optional parameters:

        - physicalEntity='1': physical entity at which belongs the element; used to define groups
          of the avatar belonging the element
        - geometricalEntity='1': geometrical entity to which belongs the element (defined in gmsh meshes files only);
          useful to differentiate several bodies stored in one mesh file
        """

        # fd ca a un interet de passer un numero dans certains cas 
        # si l'utilisateur attribue un numero au contacteur
        #if number != None:
        #   # on lui indique qu'il ne sera pas utilise
        #   showWarning('assign an index to a bulk is useless, and will be forbidden!')

        # le numero du bulk est inconnu pour l'instant et sera defini lors de son
        # ajout a un avatar
        self.number = None

        #fd necessaire pour garder le lien avec le maillage de depart
        self.originalnumber=number


        # si l'utilisateur a donne un nombre de noeuds
        if nbNodes != None:
           # on lui indique que c'est inutile
           showWarning("assign a number of nodes to an element is useless since its can be computed from the connectivity!")

        # on calcule le nombre de noeuds a partir de la connecivite
        nbNodes = len(connectivity)

        if elem_dim not in list(geoAndnbNodes2Element.keys()):
           showError("unknown geometrical element dimension: " + str(elem_dim) + "!")

        if nbNodes not in list(geoAndnbNodes2Element[elem_dim].keys()):
           showError("the given number of nodes (" + str(nbNodes) + ") is incompatible with element geometric dimension" + str(elem_dim))

        # ici, on est sur que le type d'element et la connectivite donnes sont valides

        self.etype          = geoAndnbNodes2Element[elem_dim][nbNodes]
        self.nbNodes        = nbNodes
        self.connectivity   = connectivity

        # on stocke les entites physiques et geometriques
        # TODO : en verifier la validite...
        self.physicalEntity    = physicalEntity
        self.geometricalEntity = geometricalEntity

        # on initialise a vide le modele et le materiau portes par l'element
        self.model    = None
        self.material = None

    ## @brief define material of the bulk
    #
    def defineMaterial(self,mat):
        """defineMaterial(mat)

        'mat' is either a string or a of the class 'material'
        """
        # si aucun modele n'est associe a l'element
        if self.model is None:
           # on lance une excpetion
           raise ValueError("Cannot add material to the bulk")

        # si le materiau n'est une instance de la classe materiau
        if not isinstance(mat, material):
           # on affiche un message d'erreur
           showError("material must be a material instance!")

        # ici, on est sur que le materiau est un objet de type material

        # on teste la coherence entre le modele et le materiau en fonction
        # du type de modele
        if self.model.physics == 'MECAx': # cas du modele mecanique
           # * cas du modele rigide
           if self.model.element in rigidElements:
              # on verifie que que le materiau definisse une masse volumique
              if not 'density' in bulkBehavOptions[mat.materialType]:
                 # si ce n'est pas le cas on indique la liste des materiaux compatibles
                 msg = "Material type not available with a mechanical rigi model,\n" + \
                       "the available materials with this model are:\n"
                 for i in listeBulkBehav:
                    if 'density' in bulkBehavOptions[i]:
                       msg+=i+'\n'
                 showError(msg)           
           # * cas du modele discret
           elif self.model.element in discreteElements:
              # on verifie que le materiau soit du bon type
              if mat.materialType != 'DISCRETE':
                 showError("the only material available for a discrete element model for a meshed avatar is DISCRETE!")
              # on verifie que le materiau soit compatible avec la dimension
              if numpy.size(mat.masses) != self.model.dimension:
                 showError("the material \"" + self.mat.nom + "\" is defined in " + str(numpy.size(mat.masses)) + "D while the model is defined in " + str(self.model.dimension) + "D!")
           # * cas du modele discret
           elif self.model.element in jointElements:
              # on verifie que le materiau soit du bon type
              if mat.materialType not in  ['JOINT_ELAS','JOINT_MC','JOINT_FCZM']:
                 showError("material not available for a joint element model")
              # on verifie que le materiau soit compatible avec la dimension
              if numpy.size(mat.stiffnesses) != 3:
                 showError("the material \"" + self.mat.nom + "\" has a stifness size" + str(numpy.size(mat.stiffnesses)) + "which might be 3 ")
           # * cas d'un modele associe a un element fini non gere par LMGC90   
           elif self.model.element.startswith('EXT'):
              # on verifie que le materiau soit du bon type
              if mat.materialType != 'EXTERNAL':
                 showError("the only material available for a an external finite element is EXTERNAL!")
           # * cas du modele utilisateur (associe a un fichier MatLib)
           elif hasattr(self.model, 'user_model_name'):
              # on verifie que le materiau soit du bon type
              if mat.materialType != 'USER_MAT':
                 showError("A user defined material (type \"USER_MAT\") must be associated to user modeul (i.e. option \"user_model_name\" must be defined)!\n")
           # * cas d'un modele gere par LMGC90 (interne a LMGC90 ou un modele de MatLib deja cable)
           elif hasattr(self.model, 'material'):
              # on verifie que le materiau est compatible avec le modele
              if not mat.materialType in mecaModel2bulkBehavs[self.model.material]:
                 # si ce n'est pas le cas, on indique a l'utilisateur la liste des modeles compatibles
                 msg = "Material type unavailable with a model \"material=\"" + self.model.material + "\", " + \
                       "the available materials with this model are:\n"
                 for i in  mecaModel2bulkBehavs[self.model.material]:
                    msg+=i+'\n'
                 showError(msg)           
              # si l'anisotropie du materiau est incompatible avec l'anistropie du modele
              if not mat.anisotropy == anisotopyFromModel2BulkBehav[self.model.anisotropy]:
                 # on indique a l'utilisateur la valeur de l'anisotropie a utiliser
                 msg = "Material anisotropy incompatible with model anisotropy\n" + \
                       "Material anisotropy must be: " + anisotopyFromModel2BulkBehav[self.model.anisotropy]
                 showError(msg)
           # * cas general (hautement improbable...)
           else:
              showError("Model associated to the current bulk is unhandled!")

        if self.model.physics == 'THERx': # cas du modele thermique
           # * cas du modele rigide
           if self.model.element in rigidElements:
              # on leve une exception pour indiquer que ce n'est pas encore
              # implemente
              raise NotImplementedError("thermal model not available for rigids...")
           # * cas general (element fini avec thermique claculee en uitilisant le modele interne de LMGC90)
           else:
              # si le modele n'est pas adapte
              if mat.materialType != 'THERMO_ELAS':
                 # on affiche une message d'erreur
                 showError("the only material available for a thermic model for a meshed avatar is THERMO_ELAS!")

        # si tout est bon, on stocke le nom du materiau
        self.material = mat

    ## @brief define models of the bulk
    #
    def defineModel(self,mod):
        """defineModel(mod)

        'mod' is a model
        """
        # si le mdoele donne est bien un modele
        if isinstance(mod, model):
            # si l'element porte par le modele repose sur l'element
            # geometrique porte par l'element
            if mod.element in geo2element[self.etype]:
                # on associe le modele a l'element
                self.model = mod
            # sinon, si le modele est un modele caracterisant un element dini externe, non gere par LMGC90
            elif mod.element.startswith('EXT'):
                # on associe le modele a l'element sans se poser de questions... 
                self.model = mod
            # sinon,
            else:
                # on lance une excpetion
                raise ValueError("Cannot add model to the bulk")
        # sinon,
        else:
            # on affiche un message d'erreur
            showError('[bulk.defineModel] Could not add the model to the bulk')

    def __str__(self):
        impr = "%5s\tnumber :\t%10s\n" % (self.etype,self.number)
        return impr

