from .shared import model

##############################################################################
#
#   Definition de la classe de base pour le conteneur de model
#
    
## @class models : model list    
class models(dict):
    """class models(mapping_container):

    this class defines a container of models

    methods:

    - addModel: function that add a model to the container
    - listeType
    - listeElements

    overridden operators:

    - __add__: operetor '+' adds a model in the model container
    - __str__: this function returns a string describing the model container
    """

    # fonction qui ajoute un modele dans le container
    def addModel(self,*mo, override=False):
       """addModel(self, * mo):

       this function adds a model in the models container

       parameters:

       - self: the model container itself
       - *mo: the list of models to add
       """    
       # pour chaque modele dans la liste
       for m in mo:
          assert isinstance(m, model.model), f"{str(m)} is not a model"
          # on ajoute le modele dans le container, en utilisant
          # le nom comme clef
          if not override and m.nom in self.keys():
            msg = f"{m.nom} already in container"
            raise KeyError(msg)

          self[m.nom] = m

    # surcharge de l'operateur +
    def __add__(self,mo):
       """__add__(self, mo):

       this function overrides the operator '+' to add a 
       model in the models container

       parameters:

       - self: the models container itself
       - mo: the model to add
       """
       self.addModel(mo)
       return self

    # affichage d'un conteneur de models    
    def __str__(self):
       """__str__(self):

       this function returns a string describing the model
       container

       parameters:

       - self: the model container itself
       """
       # on initalise la chaine decrivant le container de modeles
       impr='Modeles definis\t:\n'
       # pour chaque modele du container
       for mod in self:
          # on cree une nouvelle chaine a partir du container courant
          ligne='%s\n' % mod
          # on l'ajoute a la chaine a renvoyer
          impr=impr+ligne
       # on renvoie la chaine decrivant le container de modeles
       return impr
   
    # fonction qui renvoie une liste des types de modeles (meca ou thermique)
    # definis dans le container
    def listeType(self):
       """listeType(self):

       this function returns the list of models defined in the model container

       parameters:

       - self: the model container itself
       """ 
       # on definit la liste des types definis dans le conteneur de modeles
       listype=[]
       # pour chaque modele du conteneur
       for mod in self:
          # si le type de modele n'est pas dans la liste
          if mod.physics not in listype:
             # on l'ajoute a la liste
             listype.append(mod.physics)
          # sinon
          else:
             # on affiche un message pour prevenir l'utilisateur
             msg='le type :\t%s est defini deux fois dans l iterateur \n\t attention au type d element'
             showWarning(msg)
       # on renvoie la liste des types
       return listype
    
    # fonction qui renvoie une liste des types d'elements definis 
    # dans le container
    def listeElements(self):
       """listeType(self):

       this function returns the list of elements defined in the model container

       parameters:

       - self: the model container itself
       """ 
       # on definit la liste des elements definis dans le conteneur de modeles
       listel=[]
       # pour chaque modele du conteneur
       for mod in self:
          # si le type d'element n'est pas dans la liste
          if mod.element not in listel:
             # on l'ajoute a la liste
             listel.append(mod.element)
       # on renvoie la liste des elements
       return listel

    # fonction qui teste tous les modeles stockes dans le container
    def check(self):
       """check(self):

       this function checks all the models in the model container

       parameters:

       - self: the model container itself
       """ 
       # pour chaque modele du container
       for mod in self:
          # on teste le modele courant
          mod.check()

            
