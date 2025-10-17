from copy import deepcopy
import numpy

from .contactor         import contactor
from ...utilities.error import *

# liste des etiquettes des noeuds d'un contacteur
varNode=('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')

class meshedContactor(contactor):
   """class meshedContactor(contactor):
      this class is the base class used to define meshed contactors, in 2D or 3D.
      N.B.: this is an abstract class, an object of this class cannot be instanciated!
   """

   def __init__(self, elements, shape, color, reverse=False):
      """__init__(self, elements, shape, color, reverse=False):
         allows to define a meshed contactor.
         parameters:
            - self: the contactor itself
         optional parameters:
            - reverse=False: if reverse is True, the connectivity of the elements is reversed
      """
      # on appelle le constructeur de la classe generique, pour stocker le type, les elements support 
      # et la couleur du contacteur
      contactor.__init__(self, elements, shape, color)

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

# contacteurs portes par une ligne
class clxxx(meshedContactor):
   """class clxxx(meshedContactor):
      this class defines the candidate lines.
      static attribute:
         - shape='CLxxx'
      attributes:
         - weights: weights used to place contactors on each element of the contactor 
   """
   # type : CLxxx
   shape='CLxxx'

   def __init__(self, elements, color, weights=None, reverse=False, **options):
      """__init__:
         allows to define a candidate line.
         parameters:
            - self: the candidate line itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
         optional parameters:
            - weights=None: weights used to place contactors for each element of the contactor.
                 If weights=None, weights are automatically computed in order to place the contactors on the nodes
                 of the line.
            - reverse=False: if reverse is True, the connectivity of the elements is reversed
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      to_warn = False

      # on copie les elements
      list_ele = []
      for ele in elements:
         if ele.etype == 'S3xxx':
             to_warn = True
         list_ele.append(deepcopy(ele))

      if to_warn:
          warning = "A quadratic element is used to add contactors which is not recommended, because:\n"
          warning+= "    1/ the quadratic node is ignored for detection\n"
          warning+= "    2/ the contact resolution may conduct to hazardous results\n"
          showWarning(warning)

      # si on doit les retourner, on le fait ici
      if reverse :
         for ele in list_ele:
            if ele.etype == 'S3xxx':
                ele.connectivity[0], ele.connectivity[1] = ele.connectivity[1], ele.connectivity[0]
            else:
                ele.connectivity.reverse()

      # si on ne donne pas de poids pour placer le contacteur
      if weights is None :
         # on utilise l'affectation automatique : un contacteur sur chaque sommet 
           
         # on construit une liste pour stocker les sommets visites
         visited = []
         # on initialise a vide la liste des elements (qui porteront un CLxxx)
         tact_elements=[]
         # on initialise la liste des poids a vide
         tact_weights=[]
         # pour chaque element de la liste
         for bulk in list_ele:
            # si le premier sommet du contacteur n'a pas ete visite
            if not bulk.connectivity[0] in visited:
               # on indique que le premier sommet a ete visite
               visited.append(bulk.connectivity[0]) 
               # on ajoute un CLxxx sur l'element courant :
               #    * on ajoute l'element a la liste des elements qui porteront le CLxxx
               tact_elements.append(bulk)
               #    * on lui associe un poids de 0
               tact_weights.append(0.)
            # si c'est le deuxieme sommet du contacteur qui n'a pas ete visite
            if not bulk.connectivity[1] in visited:
               # on indique que le deuxieme sommet a ete visite
               visited.append(bulk.connectivity[1])
               # on ajoute un CLxxx sur l'element courant :
               #    * on ajoute l'element a la liste des elements qui porteront le CLxxx
               tact_elements.append(bulk)
               #    * on lui associe un poids de 1
               tact_weights.append(1.)

      # sinon,
      else:
         try:
            # on s'assure que c'est bien un tableau numpy
            weights = numpy.array(weights, 'd')
            # si la forme du tableau stockant les poids associes aux points n'est pas
            # celle attendue
            if len(weights.shape) != 1:
               # on affiche un message d'erreur
               showError("The weights of a CLxxx must be stored in a vector and not a matrix!")
         # si la conversion echoue
         except Exception:
            # on affiche un message d'erreur
            showError("the weights of a CLxxx must be stored in a numpy array of reals")

         # on distribue les poids sur les elements du groupe

         # on initialise a vide la liste des elements (qui porteront un CLxxx)
         tact_elements = []
         # on initialise la liste des poids associes a vide
         tact_weights=[]
         # pour chaque element de la liste passee par l'utilisateur
         for ele in list_ele: 
            # pour chaque poids considere
            for w in weights:
               # on ajoute l'element a la liste des elements qui porteront le CLxxx
               tact_elements.append(ele)
               # on ajoute le poids dans la liste des poids
               tact_weights.append(w)

      # ici, on a remplit la liste des elements qui portent un CLxxx, et les poids associes

      # on convertit la liste des poids en tableau numpy
      self.weights=numpy.array(tact_weights, 'd')
        
      # on appelle le constructeur generique de constructeur de contacteur maille, en lui
      # donnant la liste des elements qui portent un CLxxx
      # N.B.: les elements ont deja ete retournes si necessaire
      meshedContactor.__init__(self, tact_elements, self.shape, color, reverse=False)

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the candidate line itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      line=''
      # on indique que le prochain element est le premier du contacteur
      is_first = True
      # pour chaque element du contacteur
      for ie, ele in enumerate(self.elements):
         # si l'element est le premier
         if is_first:
            # on commence la ligne decrivant l'element courant par un blanc
            line+=' '
            # on indique que le prochain ne sera plus le premier
            is_first=False
         # sinon,
         else:
            # on commence la ligne decrivant l'element courant par un '+'
            line+='+'

         # on ecrit le type, le numero et la couleur du contacteur
         line+='%5s  %5s  color  %5s' % (self.shape, number, self.color)
         # on ecrit la connectivite de l'element courant
         for ic, num in enumerate(ele.connectivity):
            line+='  nod' + varNode[ic] + '=%5d' % num
         # on ajoute le poids associe a l'element courant
         line+='  apab=%14.7E\n' % self.weights[ie]
      
      # on renvoie le texte decrivant le contacteur
      return line

class alpxx(meshedContactor):
   """class alpxx(meshedContactor):
      this class defines the antagonist lines.
      static attribute:
         - shape='ALpxx'
   """
   # type : ALpxx
   shape='ALpxx'

   def __init__(self, elements, color, reverse=False, **options):
      """__init__:
         allows to define an antagonist line.
         parameters:
            - self: the antagonist line itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
         optional parameters:
            - reverse=False: if reverse is True, the connectivity of the elements is reversed
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      to_warn = False

      # on copie les elements
      list_ele = []
      for ele in elements:
         if ele.etype == 'S3xxx':
             to_warn = True
         list_ele.append(deepcopy(ele))

      if to_warn:
          warning = "A quadratic element is used to add contactors which is not recommended, because:\n"
          warning+= "    1/ the quadratic node is ignored for detection"
          warning+= "    2/ the contact resolution may conduct to hazardous results"
          showWarning(warning)

      # si on doit les retourner, on le fait ici
      if reverse :
         for ele in list_ele:
            if ele.etype == 'S3xxx':
                ele.connectivity[0], ele.connectivity[1] = ele.connectivity[1], ele.connectivity[0]
            else:
                ele.connectivity.reverse()
 
      # on trie les elements, pour les ordonner (continuite des noeuds)

      # on definit deux dictionnaires :
      #  * un donnant l'element precedent un element donne
      prec_ = dict()
      #  * un donnant l'element suivant un element donne
      next_ = dict()
      # on remplit ces dictionnaires 

      # pour chaque paire d'element (ele1, ele2)
      for i, ele1 in enumerate(list_ele):
        for j, ele2 in enumerate(list_ele):
          # si l'element ele1 precede ele2
          if ele1.connectivity[1] == ele2.connectivity[0]:
             # alors ele1 precede ele2
             prec_[j] = i
             next_[i] = j

      # on recupere le premier element de la liste
      begin_list=0
      begin=[]
      
      # on boucle dessus pour trouver les premiers elements i.e. ceux qui n'ont pas
      # de predecesseur :
      # si le premier element de la liste n'a pas de predecesseur
      if begin_list not in list(prec_.keys()):
        # c'est le premier element que l'on cherche
        begin = begin_list
      # sinon
      else:
        # on initialise au precedent
        begin = prec_[begin_list]
        while True:
          # si l'element courant n'a pas d'element precedent, ou si on 
          # est revenu au premier, on s'arrete
          if begin not in list(prec_.keys()) or begin == begin_list:
             # on a trouve le premier et on s'arrete
             break
          # sinon, on passe a son predecesseur
          begin = prec_[begin]

      # on peut alors construire la liste triee :
      # on part d'une liste vide
      sorted_list = [] 
      # on ajoute le premier element a la liste
      sorted_list.append(list_ele[begin])
      # on amorce la boucle avec le suivant
      if begin in list(next_.keys()):
        ele = next_[begin]
      else:
        ele = None
      # tant qu'on a pas atteint le bout, ou qu'on est pas revenu au premier
      while ele is not None and ele != begin:
        # on ajoute le contacteur courant
        sorted_list.append(list_ele[ele])
        # on passe au suivant
        if ele in list(next_.keys()):
          ele = next_[ele]
        else:
          ele = None

      # on appelle le constructeur generique de constructeur de contacteur maille
      meshedContactor.__init__(self, sorted_list, self.shape, color, reverse=False)

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the antagonist line itself
            - number: index of the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      line=''
      # on indique que le prochain element est le premier du contacteur
      is_first = True
      # pour chaque element du contacteur
      for ie, ele in enumerate(self.elements):
         # si l'element est le premier
         if is_first:
            # on commence la ligne decrivant l'element courant par un blanc
            line+=' '
            # on indique que le prochain ne sera plus le premier
            is_first=False
         # sinon,
         else:
            # on commence la ligne decrivant l'element courant par un '+'
            line+='+'

         # on ecrit le type, le numero et la couleur du contacteur
         line+='%5s  %5s  color  %5s' % (self.shape, number, self.color)
         # on ecrit la connectivite de l'element courant
         for ic, num in enumerate(ele.connectivity):
            line+='  nod' + varNode[ic] + '=%5d' % num
         # on finalise la ligne associee a l'element courant
         line+='\n'
      
      # on renvoie le texte decrivant le contacteur
      return line

# contacteurs circulaires portes par une ligne
class diskl(meshedContactor):
   """class diskl(meshedContactor):
      this class defines the candidate lines.
      static attribute:
         - shape='DISKL'
      attributes:
         - weights: weights used to place contactors in each element of the contactor 
   """
   # type : DISKL
   shape='DISKL'

   def __init__(self, elements, color, data=None,reverse=False, **options):
      """__init__:
         allow to define a candidate line.
         parameters:
            - self: the candidate line itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
         optional parameters:
            - data=None: data[1] is used to place the projection of DISKL center on the element .
                 If shift=None, shift is equal to default value (0.5) in order to place the  projection of diskl
                 center on the line.
            - byrd=None: dlrd is the radius of the diskl attached to the line.
                 If dlrd=None, dlrd is equal to 
            - reverse=False: if reverse is True, the connectivity of the elements is reversed
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      # on copie les elements
      list_ele = []
      for ele in elements:
         list_ele.append(deepcopy(ele))

      # si on doit les retourner, on le fait ici
      if reverse:
         for ele in list_ele:
            ele.connectivity.reverse()

      # si on ne donne pas de poids pour placer le contacteur
      if data is None:
         # on utilise l'affectation automatique : un contacteur sur chaque sommet 
         showError("The weights of a DISKL must be stored in a vector!")

      # sinon,
      else:
         try:
            # on s'assure que c'est bien un tableau numpy
            weights = numpy.array(weights, 'd')
            # si la forme du tableau stockant les poids associes aux points n'est pas
            # celle attendue
            if len(weights.shape) != 1:
               # on affiche un message d'erreur
               showError("The data of a DISKL must stored in a vector and not a matrix!")
         # si la conversion echoue
         except Exception:
            # on affiche un message d'erreur
            showError("The data of a DISKL must be stored in a numpy array of real")

         # on distribue les poids sur les elements du groupe
      # on initialise a vide la liste des elements (qui porteront un CLxxx)
      tact_elements = []
      # on initialise la liste des poids associes a vide
      tact_apabs = [] 
      tact_brpms  = []
      tact_byrds  = []
      
      # pour chaque element de la liste passee par l'utilisateur
      for ele in list_ele:
         # pour chaque poids considere
            for d in data:
               # on ajoute l'element a la liste des elements qui porteront le DISKL
               tact_elements.append(ele)
               # on ajoute le shift dans la liste des shifts
               tact_apabs.append(d[0])
               # on ajoute le shift dans la liste des byrds
               tact_byrds.append(d[1])
               # on ajoute le shift dans la liste des brpm
               tact_brpms.append(d[2])
               
      # on convertit la liste des shifts en tableau numpy
      self.apab=numpy.array(tact_apabs, 'd')
      self.byrd=numpy.array(tact_byrds, 'd')
      self.brpm=numpy.array(tact_brpms, 'd')
      
      # on appelle le constructeur generique de constructeur de contacteur maille, en lui
      # donnant la liste des elements qui portent un CLxxx
      # N.B.: les elements ont deja ete retournes si necessaire
      meshedContactor.__init__(self, tact_elements, self.shape, color, reverse=False)

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the candidate line itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      line=''
      # pour chaque element du contacteur
      for ie, ele in enumerate(self.elements):
         line+=' '
         
         # on ecrit le type, le numero et la couleur du contacteur
         line+='%5s  %5s  color  %5s' % (self.shape, number, self.color)
         # on ecrit la connectivite de l'element courant
         for ic, num in enumerate(ele.connectivity):
            line+='  nod' + varNode[ic] + '=%5d' % num
         # on ajoute le poids associe a l'element courant
         line+='  apab=%14.7E  bdyr=%14.7E  brpm=%14.7E\n' % (self.apab[ie],self.bdyr[ie],self.brpm[ie])
      
      # on renvoie le texte decrivant le contacteur
      return line


class xspxx(meshedContactor):
   """class xspxx(meshedContactor):
      this class is the base class used to define surfacic meshed contactors.
      N.B.: this is an abstract class, an object of this class cannot be instanciated!
   """

   def __init__(self, elements, color, reverse=False, unpatched=False, **options):
      """__init__:
         allow to define an antagonist surfacic meshed contactor.
         parameters:
            - self: the antagonist surface itself
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
         optional parameters:
            - reverse=False: if reverse is True, the connectivity of the elements is reversed
            - unpatched=False: if True, one contactor will be added per elements instead of patch
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """
      to_warn = False

      # on copie les elements
      list_ele = []
      for ele in elements:
         if ele.etype == 'T6xxx' or ele.etype == 'Q8xxx':
             to_warn = True
         list_ele.append(deepcopy(ele))

      if to_warn:
          warning = "A quadratic element is used to add contactors which is not recommended.\n"
          warning+= "The contact resolution may conduct to hazardous results\n"
          showWarning(warning)

      # si on doit changer l orientation des elements
      if reverse:
         # on retourne chaque elemnent 
         for ele in list_ele:
             if ele.etype == 'T6xxx':
                 ele.connectivity[:3]   = ele.connectivity[2::-1]
                 ele.connectivity[3:-1] = ele.connectivity[-2:2:-1]
             elif ele.etype == 'Q8xxx':
                 ele.connectivity[:4]   = ele.connectivity[3::-1]
                 ele.connectivity[4:-1] = ele.connectivity[-2:3:-1]
             else:
                 ele.connectivity.reverse()

      # on appelle le constructeur generique de constructeur de contacteur maille
      meshedContactor.__init__(self, list_ele, self.shape, color, reverse=False)
      self.unpatched = unpatched

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the antagonist surface itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """

      # define node format
      nf = ('5d',' ')
      for ele in self.elements:
        #not working TT
        #if any(ele.connectivity > 99999 ):
        xl = (e>99999 for e in ele.connectivity)
        if any(xl):
          nf = ('6d','')
          break

      line=''
      # on indique que le prochain element est le premier du contacteur
      is_first = True
      # pour chaque element du contacteur
      for ie, ele in enumerate(self.elements):
         # si l'element est le premier
         if is_first:
            # on commence la ligne decrivant l'element courant par un blanc
            line+=' '
            # on indique que le prochain ne sera plus le premier
            if not self.unpatched: is_first=False
         # sinon,
         else:
            # on commence la ligne decrivant l'element courant par un '+'
            line+='+'

         # on ecrit le type, le numero et la couleur du contacteur
         line+='%5s  %5s  color  %5s ' % (self.shape, number, self.color)
         # on ecrit la connectivite de l'element courant
         for ic, num in enumerate(ele.connectivity):
            line+=f" nod{varNode[ic]}={num:{nf[0]}}{nf[1]}"
         line+='\n'
      
      # on renvoie le texte decrivant le contacteur
      return line


class cspxx(xspxx):
   """class cspxx(xspxx):
      this class defines the candidate surfaces
   """
   def __init__(self, elements, color, reverse, **options):
      if 'quadrature' in list(options.keys()):
        self.shape='CSpx'+str(options['quadrature'])
      else:
        self.shape='CSpxx'
      xspxx.__init__(self, elements, color, reverse=reverse)

class aspxx(xspxx):
   """class aspxx(xspxx):
      this class defines the antagoniste surfaces discretized.
      static attribute:
         - shape='ASpxx'
   """
   # type : ASpxx
   shape='ASpxx'

# contacteurs points, poses sur une ligne, idem que clxxx
class pt2dl(meshedContactor):
   """class pt2dl(meshedContactor):
      this class defines the points on lines.
      static attribute:
         - shape='PT2DL'
      attributes:
         - weights: weights used to place contactors in each element of the contactor 
   """
   # type : PT2DL
   shape='PT2DL'

   def __init__(self, elements, color, weights=None, reverse=False, **options):
      """__init__:
         allow to define a point on a line.
         parameters:
            - self: the contactor itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
         optional parameters:
            - weights=None: weights used to place contactors for each element of the contactor.
                 If weights=None, weights are automatically computed in order to place the contactors on the nodes
                 of the line.
            - reverse=False: if reverse is True, the connectivity of the elements is reversed
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """

      # on copie les elements
      list_ele = []
      for ele in elements:
         list_ele.append(deepcopy(ele))

      # si on doit les retourner, on le fait ici
      if reverse:
         for ele in list_ele:
            ele.connectivity.reverse()

      # si on ne donne pas de poids pour placer le contacteur
      if weights is None:
         # on utilise l'affectation automatique : un contacteur sur chaque sommet 
           
         # on construit une liste pour stocker les sommets visites
         visited = []
         # on initialise a vide la liste des elements (qui porteront un CLxxx)
         tact_elements=[]
         # on initialise la liste des poids a vide
         tact_weights=[]
         # pour chaque element de la liste
         for bulk in list_ele:
            # si le premier sommet du contacteur n'a pas ete visite
            if not bulk.connectivity[0] in visited:
               # on indique que le premier sommet a ete visite
               visited.append(bulk.connectivity[0]) 
               # on ajoute un PT2Dl sur l'element courant :
               #    * on ajoute l'element a la liste des elements qui porteront le CLxxx
               tact_elements.append(bulk)
               #    * on lui associe un poids de 0
               tact_weights.append(0.)
            # si c'est le deuxieme sommet du contacteur n'a pas ete visite
            if not bulk.connectivity[1] in visited:
               # on indique que le deuxieme sommet a ete visite
               visited.append(bulk.connectivity[1])
               # on ajoute un PT2Dl sur l'element courant :
               #    * on ajoute l'element a la liste des elements qui porteront le CLxxx
               tact_elements.append(bulk)
               #    * on lui associe un poids de 1
               tact_weights.append(1.)

      # sinon,
      else:
         try:
            # on s'assure que c'est bien un tableau numpy
            weights = numpy.array(weights, 'd')
            # si la forme du tableau stockant les poids associes aux points n'est pas
            # celle attendue
            if len(weights.shape) != 1:
               # on affiche un message d'erreur
               showError("The weights of a PT2DL must be stored in a vector and not a matrix!")
         # si la conversion echoue
         except Exception:
            # on affiche un message d'erreur
            showError("The weights of a PT2DL must be stord in a numpy array of reals")

         # on distribue les poids sur les elements du groupe

         # on initialise a vide la liste des elements (qui porteront un PT2Dl)
         tact_elements = []
         # on initialise la liste des poids associes a vide
         tact_weights=[]
         # pour chaque element de la liste passee par l'utilisateur
         for ele in list_ele: 
            # pour chaque poids considere
            for w in weights:
               # on ajoute l'element a la liste des elements qui porteront le CLxxx
               tact_elements.append(ele)
               # on ajoute le poids dans la liste des poids
               tact_weights.append(w)

      # ici, on a remplit la liste des elements qui portent un PT2Dl, et les poids associes

      # on convertit la liste des poids en tableau numpy
      self.weights=numpy.array(tact_weights, 'd')
        
      # on appelle le constructeur generique de constructeur de contacteur maille, en lui
      # donnant la liste des elements qui portent un PT2Dl
      # N.B.: les elements ont deja ete retournes si necessaire
      meshedContactor.__init__(self, tact_elements, self.shape, color, reverse=False)

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the antagonist line itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      line=''
      # on indique que le prochain element est le premier du contacteur
      is_first = True
      # pour chaque element du contacteur
      for ie, ele in enumerate(self.elements):
         # si l'element est le premier
         if is_first:
            # on commence la ligne decrivant l'element courant par un blanc
            line+=' '
            # on indique que le prochain ne sera plus le premier
            is_first=False
         # sinon,
         else:
            # on commence la ligne decrivant l'element courant par un '+'
            line+='+'

         # on ecrit le type, le numero et la couleur du contacteur
         line+='%5s  %5s  color  %5s' % (self.shape, number, self.color)
         # on ecrit la connectivite de l'element courant
         for ic, num in enumerate(ele.connectivity):
            line+='  nod' + varNode[ic] + '=%5d' % num
         # on ajoute le poids associe a l'element courant
         line+='  apab=%14.7E\n' % self.weights[ie]
      
      # on renvoie le texte decrivant le contacteur
      return line


class pt2tl(meshedContactor):
   """class pt2tl(meshedContactor):
      this class defines the another kind of points on lines for thermal analysis.
      static attribute:
         - shape='PT2TL'
      attributes:
         - weights: weights used to place contactors in each element of the contactor 
   """
   # type : PT2TL
   shape='PT2TL'

   def __init__(self, elements, color, weights=None, reverse=False, **options):
      """__init__:
         allow to define a point on a line.
         parameters:
            - self: the contactor itself
            - shape: lmgc90 tact type (char[5])
            - elements: a bulk list
            - color: lmgc90 color for tact_behav (char[5])
         optional parameters:
            - weights=None: weights used to place contactors for each element of the contactor.
                 If weights=None, weights are automatically computed in order to place the contactors on the nodes
                 of the line.
            - reverse=False: if reverse is True, the connectivity of the elements is reversed
            - **options: this dictionnary catch unexpected arguments. It can be used to catch old options name
                 and tell to the user why this option is obsolete and what option should replace this one.
      """

      # on copie les elements
      list_ele = []
      for ele in elements:
         list_ele.append(deepcopy(ele))

      # si on doit les retourner, on le fait ici
      if reverse:
         for ele in list_ele:
            ele.connectivity.reverse()

      # si on ne donne pas de poids pour placer le contacteur
      if weights is None:
         # on utilise l'affectation automatique : un contacteur sur chaque sommet 
           
         # on construit une liste pour stocker les sommets visites
         visited = []
         # on initialise a vide la liste des elements (qui porteront un CLxxx)
         tact_elements=[]
         # on initialise la liste des poids a vide
         tact_weights=[]
         # pour chaque element de la liste
         for bulk in list_ele:
            # si le premier sommet du contacteur n'a pas ete visite
            if not bulk.connectivity[0] in visited:
               # on indique que le premier sommet a ete visite
               visited.append(bulk.connectivity[0]) 
               # on ajoute un PT2tl sur l'element courant :
               #    * on ajoute l'element a la liste des elements qui porteront le CLxxx
               tact_elements.append(bulk)
               #    * on lui associe un poids de 0
               tact_weights.append(0.)
            # si c'est le deuxieme sommet du contacteur n'a pas ete visite
            if not bulk.connectivity[1] in visited:
               # on indique que le deuxieme sommet a ete visite
               visited.append(bulk.connectivity[1])
               # on ajoute un PT2tl sur l'element courant :
               #    * on ajoute l'element a la liste des elements qui porteront le CLxxx
               tact_elements.append(bulk)
               #    * on lui associe un poids de 1
               tact_weights.append(1.)

      # sinon,
      else:
         try:
            # on s'assure que c'est bien un tableau numpy
            weights = numpy.array(weights, 'd')
            # si la forme du tableau stockant les poids associes aux points n'est pas
            # celle attendue
            if len(weights.shape) != 1:
               # on affiche un message d'erreur
               showError("The weights of PT2TL must be stored in a vector and not a matrix!")
         # si la conversion echoue
         except Exception:
            # on affiche un message d'erreur
            showError("The weights of PT2TL must be stored in a numpy array of reals")

         # on distribue les poids sur les elements du groupe

         # on initialise a vide la liste des elements (qui porteront un PT2tl)
         tact_elements = []
         # on initialise la liste des poids associes a vide
         tact_weights=[]
         # pour chaque element de la liste passee par l'utilisateur
         for ele in list_ele: 
            # pour chaque poids considere
            for w in weights:
               # on ajoute l'element a la liste des elements qui porteront le CLxxx
               tact_elements.append(ele)
               # on ajoute le poids dans la liste des poids
               tact_weights.append(w)

      # ici, on a remplit la liste des elements qui portent un PT2tl, et les poids associes

      # on convertit la liste des poids en tableau numpy
      self.weights=numpy.array(tact_weights, 'd')
        
      # on appelle le constructeur generique de constructeur de contacteur maille, en lui
      # donnant la liste des elements qui portent un PT2tl
      # N.B.: les elements ont deja ete retournes si necessaire
      meshedContactor.__init__(self, tact_elements, self.shape, color, reverse=False)

   def strInBodiesFile(self, number):
      """strInBodiesFile(self, number):
         this function returns a string used to represent the contactor in the BODIES.DAT file.
         parameters:
            - self: the antagonist line itself
            - number: index oh the contactor
         returned value: a string used to represent the contactor in the BODIES.DAT file.
      """
      line=''
      # on indique que le prochain element est le premier du contacteur
      is_first = True
      # pour chaque element du contacteur
      for ie, ele in enumerate(self.elements):
         # si l'element est le premier
         if is_first:
            # on commence la ligne decrivant l'element courant par un blanc
            line+=' '
            # on indique que le prochain ne sera plus le premier
            is_first=False
         # sinon,
         else:
            # on commence la ligne decrivant l'element courant par un '+'
            line+='+'

         # on ecrit le type, le numero et la couleur du contacteur
         line+='%5s  %5s  color  %5s' % (self.shape, number, self.color)
         # on ecrit la connectivite de l'element courant
         for ic, num in enumerate(ele.connectivity):
            line+='  nod' + varNode[ic] + '=%5d' % num
         # on ajoute le poids associe a l'element courant
         line+='  apab=%14.7E\n' % self.weights[ie]
      
      # on renvoie le texte decrivant le contacteur
      return line


if __name__=='__main__':
   # test a la con...
   from build_avatar.mesh import *
   from avatar.bulk.element import *

   # maillage d'une ligne, immerge dans l'espace 2D
   # 1    2    3    4
   # +----+----+----+
   # 0    i         x
   # +---->---------- 
   m = mesh(2)
   m.addNode( node( coor=numpy.array([0., 0.]), number=1 ) )
   m.addNode( node( coor=numpy.array([1., 0.]), number=2 ) )
   m.addNode( node( coor=numpy.array([2., 0.]), number=3 ) )
   m.addNode( node( coor=numpy.array([3., 0.]), number=4 ) )
   m.addBulk( element(elem_dim=1, connectivity=[1, 2]) )
   m.addBulk( element(elem_dim=1, connectivity=[2, 3]) )
   m.addBulk( element(elem_dim=1, connectivity=[3, 4]) )

   # on initialise une liste vide
   list_ele = []
   # pour chaque element du maillage
   for ele in m.bulks: 
      # on ajoute l'element a la liste
      list_ele.append(ele)

   #for ele in list_ele:
   #   print ele.connectivity

   c=clxxx(list_ele, 'BLUEx', reverse=True)

   print(c.strInBodiesFile(1))

   a=alpxx(list_ele, 'BLUEx', reverse=True)

   print(a.strInBodiesFile(2))

   # maillage d'un carre, en T2, immerge dans l'espace 3D
   #     
   # y |  4       3
   #   |  +-------+
   #   |  |\     /|
   #   |  | \   / |
   #   |  |  \5/  |
   #   |  |   +   |
   #   |  |  / \  |
   #   |  | /   \ |
   #   |  |/     \|
   #   +  *-------*
   #   0  1       2
   #      +------->---------- 
   #      0       i         x
   m2 = mesh(3)
   m2.addNode( node( coor=numpy.array([0. , 0. , 0.]), number=1 ) )
   m2.addNode( node( coor=numpy.array([1. , 0. , 0.]), number=2 ) )
   m2.addNode( node( coor=numpy.array([1. , 1. , 0.]), number=3 ) )
   m2.addNode( node( coor=numpy.array([0. , 1. , 0.]), number=4 ) )
   m2.addNode( node( coor=numpy.array([0.5, 0.5, 0.]), number=5 ) )
   m2.addBulk( element(elem_dim=2, connectivity=[1, 2, 5]) )
   m2.addBulk( element(elem_dim=2, connectivity=[2, 3, 5]) )
   m2.addBulk( element(elem_dim=2, connectivity=[3, 4, 5]) )
   m2.addBulk( element(elem_dim=2, connectivity=[4, 1, 5]) )

   # on initialise une liste vide
   list_ele = []
   # pour chaque element du maillage
   for ele in m2.bulks: 
      # on ajoute l'element a la liste
      list_ele.append(ele)

   #for ele in list_ele:
   #   print ele.connectivity

   cs=csxx3(list_ele, 'BLUEx')

   print(cs.strInBodiesFile(1))

   csp=cspx3(list_ele, 'BLUEx')

   print(csp.strInBodiesFile(1))

   as3=aspx3(list_ele, 'BLUEx')

   print(as3.strInBodiesFile(1))

   # maillage d'un carre, en Q3, immerge dans l'espace 3D
   #     
   # y |    7   8   9
   #   |    +---+---+
   #   |    |   |   |
   #   |    |   |   |
   #   |    |   5   |
   #   |  4 +---+---+ 6
   #   |    |   |   |
   #   |    |   |   |
   #   |    |   |   |
   #   +    *---+---*
   #   0    1   2   3
   #        +--->-------------- 
   #        0   i             x
   m3 = mesh(3)
   m3.addNode( node( coor=numpy.array([0. , 0. , 0.]), number=1 ) )
   m3.addNode( node( coor=numpy.array([1. , 0. , 0.]), number=2 ) )
   m3.addNode( node( coor=numpy.array([2. , 0. , 0.]), number=3 ) )
   m3.addNode( node( coor=numpy.array([0. , 1. , 0.]), number=4 ) )
   m3.addNode( node( coor=numpy.array([1. , 1. , 0.]), number=4 ) )
   m3.addNode( node( coor=numpy.array([2. , 1. , 0.]), number=6 ) )
   m3.addNode( node( coor=numpy.array([0. , 2. , 0.]), number=7 ) )
   m3.addNode( node( coor=numpy.array([1. , 2. , 0.]), number=8 ) )
   m3.addNode( node( coor=numpy.array([2. , 2. , 0.]), number=9 ) )
   m3.addBulk( element(elem_dim=2, connectivity=[1, 2, 5, 4]) )
   m3.addBulk( element(elem_dim=2, connectivity=[2, 3, 6, 5]) )
   m3.addBulk( element(elem_dim=2, connectivity=[4, 5, 8, 7]) )
   m3.addBulk( element(elem_dim=2, connectivity=[5, 6, 9, 8]) )

   # on initialise une liste vide
   list_ele = []
   # pour chaque element du maillage
   for ele in m3.bulks: 
      # on ajoute l'element a la liste
      list_ele.append(ele)

   #for ele in list_ele:
   #   print ele.connectivity

   cs4=csxx4(list_ele, 'BLUEx')

   print(cs4.strInBodiesFile(1))

   csp4=cspx4(list_ele, 'BLUEx')

   print(csp4.strInBodiesFile(1))

   as4=aspx4(list_ele, 'BLUEx')

   print(as4.strInBodiesFile(1))
   
   as8=aspx8(list_ele, 'BLUEx')

   print(as8.strInBodiesFile(1))
   
   cs8=csxx8(list_ele, 'BLUEx')

   print(cs8.strInBodiesFile(1))

   csp8=cspx8(list_ele, 'BLUEx')

   print(csp8.strInBodiesFile(1))

   csp6=cspx6(list_ele, 'BLUEx')

   print(csp6.strInBodiesFile(1))
