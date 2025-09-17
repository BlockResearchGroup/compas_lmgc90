
from ...config.lmgc90dicts import *

from ...avatar.bulks       import *
from ...avatar.bulk.bulk   import *

## @class contactor
#
class contactor():
    """ class contactor()

    attributs:

    - number  : an identifier
    - shape   : lmgc90 tact type (char[5])
    - elements: a bulk list
    - color   : lmgc90 color for tact_behav (char[5])
    - connectivity: nodes' indices defining the contactor 

    methods:

    - addOptions
    - addOption
    - modifie
    """

    ## @brief default constructor
    #
    def __init__(self, elements, shape, color, number=None):
        """__init__(self, elements, shape, color, number=None)

        allow to define a contactor

        parameters:

        - self: the contactor itself
        - elements: a list of connex elements, which is the base of the contactor
          WARNING: elements must be of the same type!
        - shape: type of the contactor
        - color: color of the contactor

        optional parameters:

        - number=None: index of the contactor (still present to ensure compatibility)
        """
        # on teste la liste d'elements

        # la liste d'elements n'est pas une liste 
        if not isinstance(elements, list):        
           # on affiche un message d')erreur
           msg='the given elements list is not a list!'
           showError(msg)

        # on teste le premier
        first_element=elements[0]

        # si ce n'est pas un element
        if not isinstance(first_element, bulk):
           # on affiche un message d'erreur
           msg='all elements of the given elements list must be bulk objects!'
           showError(msg)
        # on conserve le type de l'element
        element_type=first_element.etype

        # pour chaque autre element de la liste
        for i in range(1, len(elements)):
           ele = elements[i]
           # si ce n'est pas un element
           if not isinstance(ele, bulk):
              # on affiche un message d'erreur
              msg='all elements of the given elements list must be bulk objects!'
              showError(msg)
           # si l'element n'est pas du bon type
           if ele.etype != element_type:
              # on affiche un message d'erreur
              msg='all elements of the given elements list must have the same type!'
              showError(msg)

        # ici, on est sur que la liste elements ne contient que des elements du meme type

        # si le type de contacteur n'est compatible avec le type d'element
        if not shape in geo2contactor[element_type]:
           # on construit un message d'erreur
           msg='Incompatible element (%s) and contactor type (%s)\n' %(element_type, shape)
           if number != None:
              msg+='for contactor :%s' % str(number)
           # on l'affiche
           showError(msg)

        # on verifie la couleur du contacteur
        
        # si la couleur du contacteur n'est pas une chaine
        if not isinstance(color, str) and len(color) != 5:
           # on affiche un message d'erreur
           showError("color of a contactor must be a 5 characters string!")
          
        # si tout est bon, on cree le contacteur

        # si l'utilisateur attribue un numero au contacteur
        if number != None:
           # on lui indique qu'il ne sera pas utilise
           showWarning('assigning an index to a contactor is useless')

        # le numero du contacteur est inconnu pour l'instant et sera defini lors de son
        # ajout a un avatar
        self.number = None

        # on stocke la liste d'elements sur laquelle repose le contacteur
        self.elements = elements
        # on stocke la couleur du contacteur
        self.color    = color
 
    ## @brief get the value of an option
    # @param attribut : name of the attribut to get the value 
    # @return : value of attribut
    # @todo : to put in contactor class ?
    def getOption(self,attribut):
        """ getOption(self,attribut)
        """
        value=0.
        if attribut in contactorOptions[self.shape]:
            value=getattr(self, attribut)
        return value

    ## @brief write the options along with their values in a string
    # @return : string with all options' name and value
    # @todo : to put in contactor class ?
    def writeOpt(self):
        ligne=''
        for opt in contactorOptions[self.shape]:
            ligne+=' %5s=%14.7E' % (opt, getattr(self, opt))
        return ligne

    def __str__(self):
        """method allowing to print informations concerning the contactor
        """
        impr='Contactor number :\t%5s \t of type :\t%5s' % (self.number,self.shape)
        return impr
    
    def strInBodiesFile(self, number):
       """strInBodiesFile(self, number):

       this function returns a string used to represent the contactor in the BODIES.DAT file.

       parameters:

       - self: the contactor itself
       - number: index oh the contactor

       returned value: a string used to represent the contactor in the BODIES.DAT file.
       """
       # N.B.: il s'agit d'une fonction virtuelle pure, qui n'a donc pas d'implementation generique.
       raise NotImplementedError

