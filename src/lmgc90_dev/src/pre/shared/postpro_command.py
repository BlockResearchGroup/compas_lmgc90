
from ..utilities.error    import *

from ..config.lmgc90dicts import *
from ..avatar.avatar import *

class CLxxx_set():
    """class CLxxx_set():
       this class defines a list of CLxxx belonging to a given meshed avatar
       methods:
          - __init__: constructor
    """
    def __init__(self, body, group, predicate=None):
       """ __init__(self, body, group, predicate=None)
           this method build a new set of CLxxx
           parameters:
              - self: the objetc itself
              - body: a given avatar
              - group: a given group of the avatar
              - predicate=None: a predicate used to select the considered
                CLxxx ; ilf predicate=None all the CLxxx of the contactor
                are selected
       """
       # si l'avatar donne n'est pas un avatar
       if not isinstance(body, avatar):
          # on affiche in message d'erreur
          showError("\"body\" must be an avatar!")
       # si l'avatar n'est pas 2D
       if body.dimension != 2:
          # on affiche in message d'erreur
          showError("\"body\" must be a 2D avatar!")
       # si l'avatar donne n'est pas un avatar maille
       if body.atype != 'MAILx':
          # on affiche in message d'erreur
          showError("\"body\" must be a meshed avatar!")
       # si l'avatar ne porte pas un modele mecanique
       if body.modelType != 'MECAx':
          # on affiche in message d'erreur
          showError("\"body\" must be an avatar involving a mechanical model!")

       # si tout est bon, on stocke l'avatar donne
       self.avatar = body

       # si le nom de groupe donne n'est pas un groupe de l'avatar
       if not group in list(self.avatar.groups.keys()):
          # on affiche un message d'erreur
          showError("\"group\" must be a group ot the given avatar!")

       # on verifie que le groupe possede un et un seul contacteur CLxxx

       # on initialise le nombre de contacteurs CLxxx a 0
       nb_CLxxx=0
       # on initalise a None le contacteur CLxxx du groupe
       tact_CLxxx = None
       # pour chaque contacteur de l'element
       for tact in self.avatar.groups[group].contactors:
          # si le contacteur est un CLxxx
          if tact.shape == 'CLxxx':
             # on incremente le nombre de contacteur CLxxx du groupe
             nb_CLxxx += 1
             # on stocke une reference vers le contacteur
             tact_CLxxx = tact
       # si le groupe ne possede pas de contacteur CLxxx
       if nb_CLxxx == 0:
          # on affiche un message d'erreur
          showError("the considered group doesn't involve any CLxxx contactor!")
       # si le groupe possede plusieurs contacteurs CLxxx
       if nb_CLxxx > 1:
          # on affiche un message d'erreur
          showError("the considered group involve too many CLxxx!")

       # et la reference vers le contacteur CLxxx
       self.tact=tact_CLxxx

       # on initialise la liste des indices des points candidats du CLxxx concernes
       # a vide
       self.indices=[]
       # si aucun predicat n'a ete donne
       if predicate is None:
          # on stocke les indices de tous les points du contacteur dans la liste
          self.indices=list(range(len(self.tact.elements)))
       # sinon,
       else:
          # on enumere les points candidats
          for ie, ele in enumerate(self.tact.elements):
             # on recupere les deux noeuds sommets de l'element ligne courant
             n1 = self.avatar.nodes[ele.connectivity[0]]
             n2 = self.avatar.nodes[ele.connectivity[1]]
             # on recupere le poids associe au point candidat courant
             w=self.tact.weights[ie]
             # on calcule les coordonnees du point candidat courant
             coor = (1. - w)*n1.coor + w*n2.coor

             # on essaye de tester les coordonnees du point candidat courant
             try:
                # on teste si les coordonnees du point candidat courant verifient
                # le predicat
                is_verified = predicate(coor)
             # si on echoue
             except:
                # on affiche un message d'erreur
                showError("Applying the given predicate on the candidate points of the group " + group + " of this avatar raised an exception!\n" + \
                          "Please check your predicate (pay attention to the dimension)")

             # ici, on est sur que le predicat a pu etre applique et on peu analyser le resultat
             
             # si la position point candidat courant verifie le predicat
             if is_verified:
                # on ajoute l'indice du point candidat courant dans
                # a la liste
                self.indices.append(ie)
          # si aucun point candidat ne verifie le predicat
          if len(self.indices) == 0:
             # on affiche un message d'erreur
             showError("no candidate point of the considered CLxxx verify the given predicate!")

class postpro_command():
    """class postpro_command()
       this class builds a command for the postprocessor, which have to
       be defined in 'commandList'
       methods:
          - __init__: constructor
          - addOption: method that adds a new option to the command
    """
    def __init__(self, name, step=1, **kargs):
        """ __init__(self, name, **kargs)
           this method build a new command for the postprocessor
           parameters:
              - self: the command itself
              - name: name of the command, defined in 'commandOptions'
              - kargs: dictionnary involving all the keywords needed to define the command
           optional parameters:
              - step=1: a during computation command will be run each 'step' time steps
        """
        # si le type de la commande n'est pas reconnu
        if not name in commandList:
           # on construit un message d'erreur rappelant les types disponibles
           msg = 'Unknown post-processing command\n the type must be among:\n'
           for i in commandList:
               msg+=i+'\n'
           # on l'affiche
           showError(msg)

        # on donne son type a la commande
        self.name = name

        # on verifie la periode d'utilisation de la commande

        # si ce n'est pas un entier,
        if not isinstance(step, int):
           # on affichage un message d'erreur
           showError("the frequency of post-processing command must be an integer value (of time steps)")

        # on donne la periode d'utilisation de la commande
        self.step = step

        # on recupere la liste des options passees a la commande
        cles =  list(kargs.keys())
        # pour chaque option passe a la commande
        for cle in cles:
           # si l'option courante n'est compatible avec la commande
           if not cle in commandOptions[self.name]:
              # on affiche un warning
              msg="the option \"" + cle + "\"is not compatible with a command of type" + self.name
              showWarning(msg)
              # on passe a la suivante
              continue

           # test de la valeur de l'option

           # on recupere la valeur de l'option
           value=kargs[cle]

           # selon le nom de l'option
           if cle == 'mecax_sets': # cas des ensembles de noeuds de corps mailles
              # si la valeur passee n'est pas une liste
              if not isinstance(value, list):
                 # on affiche un message d'erreur
                 showError("The mecax_sets option of a command of type " + \
                    self.name + " must be a list of pairs " + \
                    "(meca meshed avatar, group name)\n")
              # pour chaque mecax_set, i.e. liste de couples (avatar maille, 
              # nom de groupe)
              for i, mecax_set in enumerate(value):
                 # si le mecax_set courant n'est pas une liste
                 if not isinstance(mecax_set, list):
                    # on affiche un message d'erreur
                    showError("The meca_sets option of a command type " + \
                       self.name + " must be a list of a list of pair " + \
                       "(meca meshed avatar, group name),\n" + \
                       "but object at index " + str(i) + " of the list " + \
                       "is not a list of pairs (meca meshed avatar, group name)\n")
                 # pour chaque couple (avatar, groupe) du mecax_set courant
                 for ic, couple in enumerate(mecax_set):
                    # si le couple courant n'est pas un tuple
                    if not isinstance(couple, tuple):
                       # on affiche un message d'erreur
                       showError("The meca_sets option of a command type " + \
                          self.name + " must be a list of a list of pair " + \
                          "(meca meshed avatar, group name),\n" + \
                          "but the pair at index " + str(ic) + \
                          " of the object at index " + str(i) + " of the list " + \
                          "is not a pair (meca meshed avatar, group name)\n")
                    # si le couple n'est pas un tuple de taille 2
                    if not len(couple):
                       # on affiche un message d'erreur
                       showError("The meca_sets option of a command type " + \
                          self.name + " must be a list of a list of pair " + \
                          "(meca meshed avatar, group name),\n" + \
                          "but the pair at index " + str(ic) + \
                          " of the object at index " + str(i) + " of the list " + \
                          "is not a pair (meca meshed avatar, group name)\n")
                    # ici, on est sur que l'objet courant est un couple
                    # et on separe les deux membres du couple
                    body = couple[0]
                    entity = couple[1]
                    # si le premier membre du couple n'est pas avatar
                    if not isinstance(body, avatar):
                       # on affiche un message d'erreur
                       showError("The meca_sets option of a command type " + \
                          self.name + " must be a list of a list of pair " + \
                          "(meca meshed avatar, group name),\n" + \
                          "but the first element of the pair at index " + str(ic) + \
                          " of the object at index " + str(i) + " of the list " + \
                          "is not an avatar\n")
                    # si le premier membre du couple n'est pas avatar maille
                    if body.atype != 'MAILx':
                       # on affiche un message d'erreur
                       showError("The meca_sets option of a command type " + \
                          self.name + " must be a list of a list of pair " + \
                          "(meca meshed avatar, group name),\n" + \
                          "but the first element of the pair at index " + str(ic) + \
                          " of the object at index " + str(i) + " of the list " + \
                          "is not a meshed avatar\n")
                    # si le premier membre du couple n'est pas avatar maille, avec un
                    # modele de mecanique
                    if body.modelType != 'MECAx':
                       # on affiche un message d'erreur
                       showError("The meca_sets option of a command type " + \
                          self.name + " must be a list of a list of pair " + \
                          "(meca meshed avatar, group name),\n" + \
                          "but the first element of the pair at index " + str(ic) + \
                          " of the object at index " + str(i) + " of the list " + \
                          "is not a meca meshed avatar\n")
                    # si le deuxieme membre du couple n'est pas un nom de groupe de
                    # l'avatar (constituant le premier membre du groupe)
                    if not body.hasGroup(entity):
                       # on affiche un message d'erreur
                       showError("The meca_sets option of a command type " + \
                          self.name + " must be a list of a list of pair " + \
                          "(meca meshed avatar, group name),\n" + \
                          "but the second element of the pair at index " + str(ic) + \
                          " of the object at index " + str(i) + " of the list " + \
                          "is not a group of the meca meshed avatar\n")

           elif cle == 'rigid_sets': # cas des ensembles de corps rigides
              # si la valeur passee n'est pas une liste
              if not isinstance(value, list):
                 # on affiche un message d'erreur
                 showError("The rigid_sets option of command type " + \
                    self.name + " must be a list of list of rigid avatars\n")
              # pour chaque liste de corps de rigides de la liste
              for i, rigid_list in enumerate(value):
                 # si la liste courante n'est pas valide
                 if not _check_rigid_list(rigid_list):
                    # on affiche un message d'erreur
                    showError("The rigid_sets option of command type " + \
                       self.name + " must be a list of list of rigid avatars,\n" + \
                       "but the object at index " + str(i) + " of the list " + \
                       "is not a list of rigid avatars\n")

           elif cle == 'rigid_set': # cas d'un ensemble de corps rigides
              # si la liste de rigides n'est pas valide
              if not _check_rigid_list(value):
                 # on affiche un message d'erreur
                 showError("The rigid_set option of command type " + \
                    self.name + " must be a list of rigid avatars\n")

           elif cle == 'CLxxx_sets': # cas d'un ensemble de CLxxx_set
               # si la valeur passee n'est pas une liste
              if not isinstance(value, list):
                 # on affiche un message d'erreur
                 showError("The CLxxx_sets option of command type " + \
                    self.name + " must be a list of CLxxx_set\n")
              # pour chaque ensemble de la liste
              for i, clxxx_set in enumerate(value):
                 # si l'ensemble courant n'est pas un CLxxx_set
                 if not isinstance(clxxx_set, CLxxx_set):
                    # on affiche un message d'erreur
                    showError("The CLxxx_sets option of command type " + \
                       self.name + " must be a list of CLxxx_set,\n" + \
                       "but the object at index " + str(i) + " of the list " + \
                       "is not a list of CLxxx_set\n")
         
           elif cle == 'doublets':
              # si la liste de doublets n'est pas une liste
              if not isinstance(value, list):
                 # on affiche un message d'erreur
                 showError("The doublets option of a command type " + \
                    self.name + " must be a list of pairs of rigid avatars\n")
              # pour chaque element de la liste
              for i, doublet in enumerate(value):
                 # si le doublet courant n'est pas un tuple
                 if not isinstance(doublet, tuple):
                    # on affiche un message d'erreur
                    showError("The doublets option of a command type " + \
                       self.name + " must be a list of tuples,\n" + \
                       "but the object at index " + str(i) + " of the list " + \
                       "is not a tuple\n")
                 # si le doublet courant ne contient pas deux elements
                 if len(doublet) != 2:
                    # on affiche un message d'erreur
                    showError("The doublets option of a command type " + \
                       self.name + " must be a list of pairs,\n" + \
                       "but the tuple at index " + str(i) + " of the list " + \
                       "does not hold exactly two elements\n")
                 # verification des avatars
                 if not _check_rigid_list(list(doublet)):
                    # on affiche un message d'erreur
                    showError("The doublets option of a command type " + \
                       self.name + " must be a list of pairs or rigid avatars,\n" + \
                       "but the tuple at index " + str(i) + " of the list " + \
                       "does not hold only rigid avatars\n")
           
           # ici, on est sur que la valeur de l'option courante est valide

           # on stocke l'option dans la commande
           setattr(self, cle, value)

# fonction qui teste la validite d'une liste de rigides
def _check_rigid_list(rigid_list):
    """_check_rigid_list(rigid_list):
       this function returns "True" iff the given list is a
       a list of rigid bodies
    """
    # si la liste n'est pas une liste
    if not isinstance(rigid_list, list):
       # elle ne saurait etre une liste de corps rigides
       return False

    # pour chaque corps de la liste
    for rigid in rigid_list:
       # si le corps rigide courant n'est pas un avatar
       if not isinstance(rigid, avatar):
          # la liste ne saurait etre une liste de corps
          # rigides
          return False
       # si le corps courant n'est pas un rigide
       if rigid.atype != 'RBDY2' and rigid.atype != 'RBDY3':
          # la liste ne saurait etre une liste de corps
          # rigides
          return False

    # ici, on est sur de la validite de la liste
    return True
