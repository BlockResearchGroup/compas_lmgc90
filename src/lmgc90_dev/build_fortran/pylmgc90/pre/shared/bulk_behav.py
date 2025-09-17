import numpy

from ..utilities.error    import *
from ..config.lmgc90dicts import *

class material():
    """class material()
        class allowing to define a material
        associated methods:
        - __init__
        - addProperties
        - addProperty
    """
    def __init__(self,name='acier',materialType='ELAS',**args):
        """__init__(self,name='acier',materialType='ELAS',**args)
          create a material
          '**args' is a set of key,value describing the material
        """
        # si le nom du materiau n'est pas une chaine de caracteres
        if not isinstance(name, str):
           # on affiche un message d'erreur
           showError("name of the material must be a 5 characters string!")

        # si la chaine ne fait pas cinq caracteres 
        if len(name) != 5:
           # on affiche un message d'erreur
           showError("name of the material must be a 5 characters string!")

        # si le nom du material est correct, on le stocke
        self.nom  = name

        # on recupere le type du materiau (ecrit en majuscules)
        materialType = materialType.upper()

        # si on reconnait le type du materiau
        if materialType in listeBulkBehav:
            # on le stocke
            self.materialType = materialType
        # sinon
        else:
            # on construit un message d'erreur rappelant les types disponibles
            msg = 'Unknown material\n type must be one of:\n'
            for i in listeBulkBehav:
                msg+=i+'\n'
            # on l'affiche
            showError(msg)

        # si le type a ete reconnu, on stocke les options le concernant

        # on initialise les variables utlisees pour verifier la coherence des matrices d'un modele
        # discret
        dim_matrix=None # dimension de la premiere matrice

        # pour chaque option
        for cle in list(args.keys()):
           # si l'option courante n'est compatible avec le materiau
           if not cle in bulkBehavOptions[self.materialType]:
              # on affiche un warning
              msg="option \"" + cle + "\"is not available for a material of type " + self.materialType
              showWarning(msg)
              # on passe a la suivante
              continue
           # test de la valeur de l'option

           # si l'option admet un ensemble predefini de valeurs
           if cle in list(matcle2option.keys()):
               # si la valeur donnee n'est pas admissible
               if not args[cle] in list(matcle2option[cle].keys()):
                  # on construit un message d'erreur listant les valeurs possibles
                  msg = "Invalid value for the option \"" + cle + "\"\n the value must be among:\n"
                  for opt, valeur in list(matcle2option[cle].items()):
                     msg+='%s : %s\n' % (opt, valeur)
                  # on l'affiche
                  showError(msg)
           # sinon (i.e. l'option peut prendre une valeur quelconque)
           else:
              # si l'option concerne un materiau discret (i.e. est une matrice)
              if cle in ('masses', 'stiffnesses', 'viscosities'):
                 # on tente de le convertir en matrice
                 try:
                    args[cle]=numpy.array(args[cle])
                 # si on echoue
                 except:
                    # on affiche un message d'erreur
                    showError("option \"" + cle + "\" is expecting a vector!")

                 # ici, on est sur que l'option est un tableau
                 
                 # si le tableau n'a pas la bonne forme
                 if args[cle].shape != (2,) and args[cle].shape != (3,):
                    # on affiche un message d'erreur
                    showError("option \"" + cle + "\" must a vector of the same size as space dimension (3 in 3D, or 2 in 2D)!")
                 # si la matrice est la premiere rencontree
                 if dim_matrix is None:
                    # on stocke sa dimension
                    dim_matrix = numpy.size(args[cle])
                 # sinon, si la matrice courante n'a pas la meme dimension que la premiere 
                 elif numpy.size(args[cle]) != dim_matrix:
                    # on affiche un message d'erreur
                    showError("incompatible dimensions between different of options of the material")

              elif cle == 'consolidation':
                 try:
                    args[cle]=numpy.array(args[cle])
                 # si on echoue
                 except:
                    # on affiche un message d'erreur
                    showError("option \"" + cle + "\" is expecting a vector!")
                 # si le tableau n'a pas la bonne forme
                 if args[cle].shape != (2,):
                    # on affiche un message d'erreur
                    showError("option \"" + cle + "\" must be a vector of size 2 !")

              elif cle == 'mc':
                 try:
                    args[cle]=numpy.array(args[cle])
                 # si on echoue
                 except:
                    # on affiche un message d'erreur
                    showError("option \"" + cle + "\" is expecting a vector!")
                 # si le tableau n'a pas la bonne forme
                 if args[cle].shape != (4,) :
                    # on affiche un message d'erreur
                    showError("option \"" + cle + "\" must be a vector of size 4 !")
                    
              elif cle == 'fczm':
                 try:
                    args[cle]=numpy.array(args[cle])
                 # si on echoue
                 except:
                    # on affiche un message d'erreur
                    showError("option \"" + cle + "\" is expecting a vector!")
                 # si le tableau n'a pas la bonne forme
                 if args[cle].shape != (10,) :
                    # on affiche un message d'erreur
                    showError("option \"" + cle + "\" must be a vector of size 10 !")                     
                  
              # si l'option concerne un materiau utilisateur (i.e. est un nom de fichier)
              elif cle == 'file_mat':
                 # si le nom de fichier materiau n'est pas une chaine
                 if not isinstance(args[cle], str):
                    # on affiche un message d'erreur
                    showError("file_mat is not a string!")

                 # si le nom de fichier materiau est une chaine de plus de 50 caracteres
                 if len(args[cle]) > 50: 
                    # on affiche un message d'erreur
                    showError("file_mat contains more than 50 caracters!")
              # sinon,
              elif args[cle] == 'field':
                  args[cle]= 'field'

              # orthotrope   
              elif (cle =='young' or cle =='nu' or cle =='G') and args['anisotropy'] == 'orthotropic':   
                 try:
                    args[cle]=numpy.array(args[cle])
                 # si on echoue
                 except:
                    # on affiche un message d'erreur
                    showError("option \"" + cle + "\" is expecting a vector since material is orthotropic!")
                    
              else:
                 # on est dans le cas general : l'option doit etre un reel

                 # on tente de convertir la valeur en reel
                 try:
                    args[cle]=float(args[cle])
                 # si on echoue
                 except:
                    # on affiche un message d'erreur
                    showError("option \"" + cle + "\" is expecting a real value!")
 
           # ici, on est sur que la valeur de l'option courante est valide

           # on stocke l'option dans le materiau
           setattr(self, cle, args[cle])

        # on verifie que toutes les options necessaires ont ete affectees au materiau
        self.check()
        
    # fonction qui verifie que toutes les options obligatoires pour le materiau ont bien ete affectees
    def check(self):
        # on commence la verification a partir des fils de l'arbre associe au type du materiau, qui sont
        # tous des options
        for child in checkBulkBehavOptions[self.materialType].childs:
           self._check(child, "option")

    # fonction qui verifie la presence d'options obligatoires pour le materiau, prises dans un sous-ensemble 
    # donne
    def _check(self, tree, root_type):
        # si la racine de l'arbre est une option
        if root_type == "option":
           # on recupere l'option dans a la racine de l'arbre
           option = tree.root
           # si cette option n'a pas ete definie pour le materiau
           if not hasattr(self, option):
              # on affiche un message d'erreur
              showError("option : \"" + option + "\" is not assigned to this material.")
           # sinon, on recupere la valeur de l'option, pour le materiau
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
              msg = "Unexptected value for the option \"" + option +"\"\n" + \
                       "possibles values are:\n"
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

    def __str__(self):
        impr='Material:%s\n\tBehavior type\t:%s\n\tProperties :\n'% \
              (self.nom,self.materialType)
        for cle in list(vars(self).keys()):
            if cle in bulkBehavOptions[self.materialType]:
                if cle in list(matcle2option.keys()):
                    impr+='\t\t%20s\t:\t%s\n' %(cle, matcle2option[cle][getattr(self, cle)])
                else:
                    impr+='\t\t%20s\t:\t%s\n' %(cle, getattr(self, cle))
        return impr
 
