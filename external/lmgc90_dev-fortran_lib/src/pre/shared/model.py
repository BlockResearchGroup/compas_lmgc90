
from ..utilities.error    import *
from ..config.lmgc90dicts import *

##############################################################################
#
#   Definition de la classe de base pour un model
#
    
class model():
    """class model():

    associated methods:

    - __init__
    - listeOptions
    - addOptions
    """
    
    ## @todo 'nom' a changer en 'name' (en fait verifier l'anglais partout)
    def __init__(self, name, physics, element, dimension=None, external_fields=None, external_vfields=None, external_vsizes=None, 
                 user_model_name=None, **kargs):
        """__init__(self, name, physics, element, dimension=None, external_fields=None, external_vfields=None, external_vsizes=None, user_model_name=None, **kargs)

        allow to define a model

        parameters:

        - self: the model itself
        - name: name of the model (5 characters string)
        - physics: the physical model defining which physics will be concerned
        - element: type of element handled by this model
        - dimension: spatial dimension (2, in 2D or 3, in 3D)

        optional parameters:

        - external_fields: list of strings defining external fields (i.e. extra fields
          which are not needed a priori) also stored by the model
        - external_vfields: list of strings defining external vector fields (i.e. extra fields
          which are not needed a priori) also stored by the model
        - external_vsizes: list of size of each vector field
        - user_model_name: name of a user model not understood by the external models lib, and 
          ignored by LMGC90
        - **kargs: a dictionnary used to assigne options to the model (pair 'option'=value)
        """
        # gestion de l'absence de la donnee de la variable "dimension", en vue de garantir la compatibilite
        # ascendante
        
        # si l'utilisateur n'a pas donne la dimension du modele
        if dimension is None:
           # on affiche un message d'erreur
           showError("you must choose the spatial dimension, by setting the variable \"dimension\"!\n")

        # POSSIBILITE DE FAIRE UN TEST SUPPLEMENTAIRE POUR LES DIMENSIONS

        # si le nom du modele n'est pas une chaine de caracteres
        if not isinstance(name, str):
           # on affiche un message d'erreur
           shoWError("name of the model must be a 5 characters string!")

        # si la chaine ne fait pas cinq caracteres 
        if len(name) != 5:
           # on affiche un message d'erreur
           showError("name of the model must be a 5 characters string!")

        # si le nom du modele est correct, on le stocke
        self.nom = name

        # si la dimension du modele est impossible
        if dimension != 2 and dimension != 3:
           # on affiche un message d'erreur
           showError("spatial dimension must be 2 (in 2D) or 3 (in 3D)!")
        # sinon, 
        else:
           # on la stocke
           self.dimension = dimension

        # si on reconnait le type de modele
        if physics in listeModel:
            # on le stocke
            self.physics = physics
        # sinon
        else:
            # on construit un message d'erreur rappelant les types disponibles
            msg = 'Unknown model type\n the elements available for a ' + str(self.dimension) + 'D model are:\n'
            for i in listeModel:
                msg+=i+'\n'
            # on l'affiche
            showError(msg)

        # si on reconnait le type d'element fini
        if element in listeElement:
            # on le stocke
            self.element = element
            # si le type de l'element n'est pas compatible avec la dimension consideree
            if not self.element in dimension2element[self.dimension]:
               # on construit un message d'erreur rappelant les types compatibles
               msg = "Element type incompatible with a " + str(self.dimension) + "D model\n the type must be among:\n"
               for i in dimension2element[self.dimension]:
                   msg+=i+'\n'
               # on l'affiche
               showError(msg)

            # si le nombre de ddl par noeud est defini pour
            # le type de modele et le type d'element considere
            if self.physics in element2ddl[self.element]:
               # on le recupere
               self.nbdof = element2ddl[self.element][self.physics]
            # sinon
            else:
               # on indique que le type d'element considere ne gere pas le type de
               # modele considere
               showError("the elements of type " + self.element + " are not usable by a physical model of type " + self.physics)
        # sinon, s'il s'agit d'un element externe (i.e. non calcule par LMGC90)
        elif element.startswith('EXT'):
            # on verifie la coherence du nom

            # * chaine de 5 caracteres
            if len(element) != 5:
               showError("An external element name must be a 5 characters string! (" + element + " is invalid)")

            # * la fin de la chaine donne le nombre de noeuds

            # on recupere la chaine correspondant au nombre de ddl
            str_nb_nodes = element[3:]
            # on tente de convertir la chaine en entier
            try:
               nbdof = int(str_nb_nodes)
            except:
               # si la conversion e choue, le nom de l'element est caduc
               showError("The end of an external element name must give the number dof! (" + str_nb_nodes + " is invalid)") 

            # ici, on est sur que l'element est bien defini

            # on stocke son nom et le nombre de ddl associe
            self.element = element
            self.nbdof = nbdof
        # sinon
        else:
            # on construit un message d'erreur rappelant les types disponibles
            msg = "Unknown finite element type\n it must be among:\n"
            for i in listeElement:
                msg+=i+'\n'
            # on l'affiche
            showError(msg)

        # si on a donne une liste de champs externes
        if ( external_fields ):
           # on verifie son integrite

           # si l'objet n'est pas une liste
           if not isinstance(external_fields, list):
              # on affiche un message d'erreur
              showError("external_fields must be a list of strings!")
 
           # pour chaque champ de la liste
           for i, field in enumerate(external_fields):
              # si le champ courant n'est pas une chaine
              if not isinstance(field, str):
                 # on affiche un message d'erreur
                 showError("external field: " + str(i + 1) + " is not a string!")

              # si le champ est une chaine de plus de 30 caracteres
              if len(field) > 30: 
                 # on affiche un message d'erreur
                 showError("external field: " + str(i + 1) + " contains more than 30 caracters!")

           # si tout est bon, on stocke la liste de champs externes
           self.external_fields=external_fields
        # sinon,
        else:
           # on inidque qu'il n'y aura pas de champs externes
           self.external_fields=[]

        # si on a donne une liste de champs externes vectoriels
        if ( external_vfields ) and ( external_vsizes ):

          if not isinstance(external_vfields, list):
            showError("external_vfields must be a list of strings!")
 
          if not isinstance(external_vsizes, list):
            showError("external_vsizes must be a list of integers!")

          if len(external_vsizes) != len(external_vfields) :
            showError("external_vsizes must be a list of same size than external_vfields!")

          for i, field in enumerate(external_vfields):
            if not isinstance(field, str):
              showError("external vfield: " + str(i + 1) + " is not a string!")

            if len(field) > 30: 
              showError("external vfield: " + str(i + 1) + " contains more than 30 caracters!")

            if not isinstance(external_vsizes[i], int):
              showError("external vsize: " + str(i + 1) + " is not an integer!")

          self.external_vfields = external_vfields
          self.external_vsizes  = external_vsizes
        else:
          self.external_vfields = []
          self.external_vsizes  = []

        # si on a donne un nom de modele utilisateur
        if user_model_name != None:
           # on verifie son integrite

           # si le champ le nom de modele utilisateur n'est pas une chaine
           if not isinstance(user_model_name, str):
              # on affiche un message d'erreur
              showError("user_model_name is not a string!")

           # si le nom de modele utilisateur est une chaine de plus de 50 caracteres
           if len(user_model_name) > 50: 
              # on affiche un message d'erreur
              showError("user_model_name contains more than 50 caracters!")

           # si tout est bon, on stocke le nom de modele utilisateur
           self.user_model_name=user_model_name

        # on recupere la liste des options
        cles = list(kargs.keys())

        # on ajoute les options au modele

        # pour chaque option
        for cle in cles:
           # si l'option courante n'est pas prevue par le modele
           if not cle in list(modelOptions[self.physics].keys()):
              # on affiche un warning
              msg="the option \"" + cle + "\" is not compatible with a model of type " + self.physics
              showWarning(msg)
              # on passe a la suivante
              continue
           # test de la valeur de l'option

           # si la valeur donnee est admissible
           if kargs[cle] in modelOptions[self.physics][cle]:
              # on ajoute l'option au modele
              setattr(self, cle, kargs[cle])
           # sinon,
           else:
              # on construit un message d'erreur rappelant les valeurs disponibles
              msg = "Non valid value for the option \"" + cle + "\"\n it must be among:\n"
              for i in modelOptions[self.physics][cle]:
                 msg+=i+'\n'
              # on l'affiche
              showError(msg)

        # si le modele s'appuie sur un element rigide ou si le modele s'appuie sur un element externe
        if self.element in rigidElements or self.element.startswith('EXT'):
           # on n'a pas de test de coherence a faire et on quitte la fonction
           return

        # verification des tests de coherence, pour les modeles deformables geres par LMGC90

        # si le modele s'appuie sur un element discret
        if self.element in discreteElements:
           # si le modele n'est pas decrit comme discret
           if not 'discrete' in cles or kargs['discrete'] != 'yes__':
              # on affiche un message d'erreur
              showError("A model based on a discrete element must define the option : discrete='yes__'!")

           # on verifie simplement que le modele n'utilise pas un materiau externe
           
           # si le modele n'indique pas s'il utilise un materiau externe ou non
           if not 'external_model' in cles:
              # on affiche un message d'erreur
              showError("the option : \"external_model\" has not been added to this model.")
           # si le modele utilise un materiau externe
           if kargs['external_model'] != 'no___':
              # on affiche un message d'erreur
              showError("in case of a discrete model, there must be: external_model=\"no___\".")

        elif self.element in jointElements:
          # si le modele n'indique pas s'il utilise un materiau externe ou non
           if not 'external_model' in cles:
              # on affiche un message d'erreur
              showError("the option : \"external_model\" has not been added to this model.")
           # si le modele utilise un materiau externe
           if kargs['external_model'] != 'no___':
              # on affiche un message d'erreur
              showError("in case of a joint model, there must be: external_model=\"no___\".")

        # sinon,
        else:
           # si le modele est decrit come discret
           if 'discrete' in cles and kargs['discrete'] == 'yes__':
              # on affiche un message d'erreur
              showError("A model base on a discrete element must not define the option 'discrete' or verify: discrete='no___'!")

           # si le modele est un modele utilsateur
           if hasattr(self, 'user_model_name'):
              # on verifie simplement que le modele n'utilise pas un materiau externe

              # si le modele n'indique pas s'il utilise un materiau externe ou non
              if not hasattr(self, 'external_model'):
                 # on affiche un message d'erreur
                 showError("the option : \"external_model\" has not been added to this model.")
              # si le modele n'utilise pas un materiau externe
              if self.external_model != 'MatL_' and self.external_model != 'Demfi' and self.external_model != 'Umat_':
                 # on affiche un message d'erreur
                 showError("in case of a user modele, there must be: external_model=\"MatL_\" or \"Demfi\" or \"Umat_\".")

              # on verifie la presence des autres options indispensables (les options elementaires)

              # pour chaque option indispensable
              for option in ['kinematic', 'mass_storage']:
                 # si l'option n'a pas ete donnee
                 if not hasattr(self, option):
                    # on affiche un message d'erreur
                    showError("in case of a user model, the option : \"" + option + "\" must be defined")
           # sinon,
           else: 
              # on verifie que toutes les options necessaires ont ete affectees au modele
              self.check()
        
    # fonction qui verifie que toutes les options obligatoires pour le modele ont bien ete affectees
    def check(self):
        """check(self)

           checks if mandatory options are present 

        """ 
        # on commence la verification a partir des fils de l'arbre associe au type du modele, qui sont
        # tous des options
        for child in checkModelOptions[self.physics].childs:
           self._check(child, "option")

    # fonction qui verifie la presence d'options obligatoires pour le modele, prises dans un sous-ensemble 
    # donne
    def _check(self, tree, root_type):
        # si la racine de l'arbre est une option
        if root_type == "option":
           # on recupere l'option dans a la racine de l'arbre
           option = tree.root
           # si cette option n'a pas ete definie pour le modele
           if not hasattr(self, option):
              # on affiche un message d'erreur
              showError("the option : \"" + option + "\" has not been affected to this model.")
           # sinon, on recupere la valeur de l'option, pour le modele
           value=getattr(self, option)

           # si l'arbre n'a pas de fils
           if len(tree.childs) == 0:
              # on a fini la verification
              return

           # sinon, on cherche l'arbre fils qui contient cette valeur dans sa racine

           # on indique qu'on ne l'a pas encore trouve
           found_child = None
           # pour chaque arbre fils de l'arbre
           for child in tree.childs:
              # si le fils courant contient la valeur cherchee
              if child.root == value:
                 # on stocke l'arbre fils
                 found_child = child
                 # on quitte la boucle
                 break
           # si on a trouve l'arbre qui contient la valeur
           if found_child != None:
              # on continue la verification en descendant dans par cet arbre
              self._check(found_child, "value")
           # sinon,
           else:
              # on est tombe sur une valeur innatendue de l'option!
              # on contruit un message d'erreur pour afficher la liste des valeurs attendues
              msg = "Unexpected value for the option \"" + option +"\"\n" + \
                       "the possible values are:\n"
              for child in tree.childs:
                 msg += child.root + "\n"
              # on l'affiche
              showError(msg)
        # sinon, si la racine de l'arbre est une valeur
        elif root_type == "value":
           # pour chaque fils de l'arbre
           for child in tree.childs:
              # on poursuit la verification avec les options suivantes
              self._check(child, "option") 
        # sinon,
        else:
           # on affiche un message d'erreur
           showError("unknow root type!")

    def listeOptions(self):
        
        listeOptions  =  []
        
        for k in list(vars(self).keys()):
            if k not in ['dimension', 'nom', 'physics', 'element', 'nbdof', 'external_fields', 'external_vfields', 'external_vsizes', 'user_model_name']:
                listeOptions.append(k)
                
        return listeOptions
    
    def __str__(self):
        impr   = '\tModel :\t%s\n\tType of model\t:%s\t for the elements:\t%s\n' % (self.nom,
                                                                                       self.physics,self.element)
        impr+='\t\tDefined options:\n'
        
        for cle in self.listeOptions():
            impr+='\t\t\t'+cle+' : '+ getattr(self, cle)
        return impr

 
#######################################################################
#
#  Test pour le fichier 'models.py'
#
    
if __name__=='__main__':
    momo=models()
    mt=model('T3DNX','THERx','T3xxx',capaStorage='lump_')
    mm=model('M3DNL','MECAx','T3xxx',kinematic='small',anisotropy='iso__')

    momo.addModel(mt,mm)

    mm.check()
