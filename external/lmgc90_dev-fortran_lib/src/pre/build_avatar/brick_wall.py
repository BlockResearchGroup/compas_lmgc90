# module definissant des classes "murs de brique"
from ..avatars import *
from ..utilities.error import *

from .brick_row import *

# classe definissant un mur construit avec un apareil type paneresse generique, i.e. la classe fournit
# toutes les fonctions pour donner ou calculer le nombre de rangees, la hauteur du mur, la taille du joint
# mais pas la methode de construction
class paneresse_generic():
   """class paneresse_generic():
      this class defines an objet representing a wall, using the so called "apareil en paneresses"
   """

   # costructeur
   def __init__(self, brick_ref, disposition):
      """__init__(self, brick_ref, disposition):

      this function defines a new wall

      parameters:

      - self: the wall itself
      - brick_ref: a brick object describing the kind of brick used to build the wall
      - disposition: disposition of the brick in the wall, possible values are "paneresse", "boutisse" and "chant"
      """

      # on stocke la brique de reference
      self.brick_ref = brick_ref
      # on stocke la disposition des briques dans le mur
      self.disposition = disposition

      # on calcule la hauteur d'une rangee de brique, en fonction de la disposition choisie
      
      # si la disposition est en paneresse ou en boutisse
      if self.disposition == "paneresse" or self.disposition == "boutisse":
         # la hauteur d'une rangee est la hauteur de la brique de reference
         self.row_height = brick_ref.lz
      # si la disposition est sur chant
      elif self.disposition == "chant":
         # la hauteur d'une rangee est la largeur de la brique de reference
         self.row_height = brick_ref.ly
      # cas general
      else:
         # on affiche un message d'erreur
         showError("unknown disposition!")

      # la valeurs des autres attributs ne sont pas encore connus
      self.nb_rows = None           # le nombre de rangees de briques
      self.height = None            # la hauteur du mur (briques + joints)
      self.joint_thickness = None # l'epaisseur du joint entre deux rangees, superposees

      self.even_row = None          # objet rangee de brique, d'indice pair
      self.odd_row = None           # objet rangee de brique, d'indice impair

   # donnee de la hauteur du mur, en nombre de briques
   # N.B.: le nombre de briques doit etre entier
   def setNumberOfRows(self, nb_rows, rtol=1e-5):
      """setNumberOfRows(self, nb_rows, rtol=1e-5):

      this function allows to set the number of rows in the wall.

      parameters:

      - self: wall itself
      - nb_rows: the given number of rows; this number must be an integer, but could be represented as a float

      optional parameters:

      - rtol: relative tolerance used in floatting number comparaisons
      """

      # on calcule la tolerance absolue pour evaluer les valeurs reelles, a partir de la tolerance relative 
      # et la hauteur de la brique de reference
      atol = rtol*self.row_height

      # si on passe un entier
      if isinstance(nb_rows, int):
         # on le stocke tel quel
         self.nb_rows = nb_rows
      # si on passe un reel
      elif isinstance(nb_rows, float):
         # si c'est un entier stocke sous forme d'un reel
         if math.fabs(nb_rows - math.floor(nb_rows)) <= atol:
            # on le sotcke sous la forme d'un entier
            self.nb_rows = int(nb_rows)
         # sinon,
         else:
            # on affiche un avertissement
            showWarning("the given number of rows has been skipped, since it's not an integer")

   # donnee de l'epaisseur du joint entre deux rangees
   def setJointThicknessBetweenRows(self, joint_thickness):
      """setJointThicknessBetweenRows(self, joint_thickness):

      this function allows to set the joint thickness between two brick rows.

      parameters:

      - self: the wall itself
      - joint_thickness: the given joint thickness
      """

      self.joint_thickness = joint_thickness

   # donnee de la hauteur du mur
   def setHeight(self, height):
      """setHeight(self, length):

      this function allows to set the height for the wall.

      parameters:

      - self: the wall itself
      - length: the given height
      """

      self.height = height

   # evaluation de la hauteur du mur, a partir d'un nombre de briques et d'une epaisseur de joint
   # passees en argument
   def evaluateHeight(self, nb_rows, joint_thickness):
      """evaluateHeight(self, nb_rows, joint_thickness):

      this function evaluates ans returns the height for the wall, using given number of rows and joint thickness.

      parameters:

      - self: the wall itself
      - nb_rows: the given number of rows; this number must be an integer, but could be represented as a float
      - joint_thickness: the given joint thickness

      returned value: the evaluated height
      """

      # il y a autant de joints que le nombre de rangees de briques
      nb_joints = nb_rows

      # on en deduit la hauteur du mur
      height = nb_rows*self.row_height + nb_joints*joint_thickness

      # on renvoie la hauteur de mur calculee
      return height

   # calcul de la hauteur du mur, a partir du nombre de rangees et de l'epaisseur des joints
   # precondition : le nombre de rangees et le joint on ete renseignes
   def computeHeight(self):
      """computeHeight(self):

      this function computes and stores the height for the wall, using the number of rows and joint thickness
      defined as attributs of the wall.

      parameters:

      - self: the wall itself
      """

      # si les donnees requises sont absentes
      if self.nb_rows is None or self.joint_thickness is None:
         # on affcihe un avertissement
         showWarning("data is missing to compute the wall height")
         # et on quitte la fonction
         return
 
      # on calcule la hauteur du mur
      self.height = self.evaluateHeight(self.nb_rows, self.joint_thickness)

   # calcul de l'epaisseur du joint, a partir du nombre de rangees et de la hauteur du mur
   # precondition : le nombre de briques et la hauteur du mur on ete renseignes
   def computeJointThickness(self):
      """computeJointThickness(self):
      this function computes and stores the joint thickness for the wall, using the number of rows and height
      defined as attributs of the wall.

      parameters:

      - self: the wall itself
      """

      # si les donnees requises sont absentes
      if self.nb_rows is None or self.height is None:
         # on affiche un avertissement
         showWarning("data is missing to compute joint thickness")
         # et on quitte la fonction
         return

      # il y a autant de joints que le nombre de rangees de briques
      nb_joints = self.nb_rows
      # on en deduit l'epaisseur du joint
      self.joint_thickness = (self.height - self.nb_rows*self.row_height)/nb_joints

   # calcule deux nombre de rangees fournissant des murs dont la hauteur encadre une hauteur desiree,
   # passee en argument, connsaissant l'epaisseur du joint souhaitee
   def limitNbRows(self, height, joint_thickness):
      """limitNbRows(self, height, joint_thickness):

      this function computes and returns two number of rows, using given joint thickness and height.
      Each number of rows coresponds to a wall which the height is close to the given height.

      parameters:

      - self: the wall itself
      - height: the given height
      - joint_thickness: the given joint thickness

      returned value: the couple of number of rows computed
      """

      # on calcule le nombre de rangees que peut contenir le mur
      nb_rows_min = math.floor(height/(self.row_height + joint_thickness)) 

      # l'encadrement consiste a ne pas ajouter de rangee, ou en ajouter une de pluse
      return [nb_rows_min, nb_rows_min + 1]

   # calcul du nombre de rangees, a partir de la hauteur du mur, de l'epaisseur des joints
   # et d'une tendance de variation pour l'epaisseur du joint. En effet, la donnee d'une hauteur de mur
   # et d'une epaisseur de joint peuvent aboutir a deux nombre de rangees, approchant la hauteur du mur
   # par valeur superieures et inferieures, sans l'atteindre. Aussi, la liberte de changer l'epaisseur du
   # joint, en indiquant si l'on peut l'augmenter ou la diminuer, permet de retrouver un mur a la bonne hauteur
   # precondition : la hauteur du mur et l'epaisseur d'un joint ont ete renseignes
   def computeNbRows(self, trend="max", rtol=1e-5):
      """commputeNbRows(self, trend="max", rtol=1e-5):

      this function computes an optimal number of rows, using the joint thickness and height
      defined as attributs of the wall. It computes bounds using the function evaluateNbRows.
      If one of the bounds is the exact solution, it's stored, else the joint thickness is altered to
      find a solution. The joint thickness is increased or decreased depending on user's choice.

      parameters:

      - self: the wall itself

      optional parameters:

      - trend: this parameter is used to choose how the joint thickness can change

           - "max": if the joint thickness could only be increased
           - "min": if the joint thickness could only be decreased

      - rtol: relative tolerance used in floatting number comparaisons
      """

      # si les donnees requises sont absentes
      if self.height is None or self.joint_thickness is None:
         # on affiche un avertissement
         showWarning("data is missing to compute the required number of rows")
         # et on quitte la fonction
         return
     
      # on evalue les deux nombre de rangees possibles
      [min_nb_rows, max_nb_rows] = self.limitNbRows(self.height, self.joint_thickness) 

      # on teste si on tombe dans le cas particulier ou la solution exacte existe
      
      # on calcule la tolerance absolue pour evaluer les valeurs reelles, a partir de la tolerance relative 
      # et la hauteur d'une brique
      atol = rtol*self.row_height

      # on evalue la hauteur du mur, en utilisant le plus petit nombre de rangees
      min_height = self.evaluateHeight(min_nb_rows, self.joint_thickness)
      # si on trouve la hauteur souhaitee pour le mur
      if math.fabs(min_height - self.height) < atol:
         # on choisit ce nombre de rangees
         self.setNumberOfRows(min_nb_rows)
         # on a fini
         return

      # on evalue la hauteur du mur, en utilisant le plus grand nombre de rangees
      max_height = self.evaluateHeight(max_nb_rows, self.joint_thickness)
      # si on trouve la hauteur souhaitee pour le mur
      if math.fabs(max_height - self.height) < atol:
         # on choisit ce nombre de rangees
         self.setNumberOfRows(max_nb_rows)
         # on a fini
         return

      # sinon, on choisit par rapport a la tendance de modification de la taille du joint

      # si on souhaite garantir une epaisseur de joint minimale
      if trend == "max":
         # on choisit le plus petit nombre de ranges
         self.setNumberOfRows(min_nb_rows)
      # si on prefere diminuer l'epasisseur du joint
      elif trend == "min":
         # on choisit le plus grand nombre de rangees
         self.setNumberOfRows(max_nb_rows)
      else:
         # si le choix est impossible, on affiche une erreur
         showError("unknown trend")

      # on calcule la nouvelle epaisseur du joint
      self.computeJointThickness()
   
   # caracterise la premiere rangee, par le type de premiere brique, l'epaisseur du joint 
   # entre deux briques de la rangee et le nombre de briques de la rangee
   # N.B.: le nombre de briques peut etre fractionnaire, e.g 2 briques entiere plus un quart de brique, donnent une longueur de 2.25
   def setFirstRowByNumberOfBricks(self, first_brick_type, nb_bricks, joint_thickness):
      """setFirstRowByNumberOfBricks(self, first_brick_type, nb_bricks, joint_thickness):

      this function sets the first row of the wall by giving the type of the first brick of this row, 
      the number of bricks in this row and the joint thickness

      parameters:

      - self: the wall itself
      - first_brick_type: describe the kind of brick begining the first row:

        - "1": for a whole brick
        - "1/2": for a half of a brick

      - nb_bricks: the given number of bricks; this number could be fractional, for exemple it's 2.5 for two bricks and a half
      - joint_thickness: the given joint thickness for the first row
      """

      # la premiere rangee est de type paire (indice 0)

      # on la construit
      self.even_row = brick_row(brick_ref=self.brick_ref, disposition=self.disposition, first_brick_type=first_brick_type)     
      # on lui attribue son nombre de briques
      self.even_row.setNumberOfBricks(nb_bricks)
      # on lui attribue son epaisseur de joints
      self.even_row.setJointThickness(joint_thickness)
      # on calcule sa longeur
      self.even_row.computeLength()
 
      # la deuxieme rangee est de type impair (indice 1)
 
      # on determine sa premiere brique, a partir du choix de la premiere brique du mur

      # si la premiere brique est entiere
      if first_brick_type == "1":
         # on prend une demi-brique
         first_brick_type_second = "1/2"
      # si la premiere brique est une demi-brique
      elif first_brick_type == "1/2":
         # on prend une brique entiere
         first_brick_type_second = "1"
      # sinon,
      else:
         # cas incompatible avec l'apareil en paneresses
         showError("the given first brick type is not allowed")

      # on la construit
      self.odd_row = brick_row(brick_ref=self.brick_ref, disposition=self.disposition, first_brick_type=first_brick_type_second)
      # on lui attribue son nombre de briques
      self.odd_row.setNumberOfBricks(nb_bricks)
      # on lui attribue la longueur de la premiere rangee
      self.odd_row.setLength(self.even_row.length)
      # on calcule son epaisseur de joints
      self.odd_row.computeJointThickness()

   # caracterise la premiere rangee, par le type de premiere brique, l'epaisseur du joint 
   # entre deux briques de la rangee et la longueur de la rangee
   def setFirstRowByLength(self, first_brick_type, length, joint_thickness):
      """setFirstRowByLength(self, first_brick_type, length, joint_thickness):

      this function sets the first row of the wall by giving the type of the first brick of this row, 
      the length of this row and the joint thickness

      parameters:

      - self: the wall itself
      - first_brick_type: describe the kind of brick begining the first row:

        - "1": for a whole brick
        - "1/2": for a half of a brick

      - length: the given length for the first row
      - joint_thickness: the given joint thickness for the first row
      """

      # la premiere rangee est de type paire (indice 0)

      # on la definit
      self.even_row = brick_row(brick_ref=self.brick_ref, disposition=self.disposition, first_brick_type=first_brick_type)
      # on lui attribue sa longueur
      self.even_row.setLength(length)
      # on lui attribue son epaisseur de joints
      self.even_row.setJointThickness(joint_thickness)
      # on calcule le nombre de briques necessaires
      self.even_row.computeNbBricks(trend="max", rtol=1e-5)
  
      # la deuxieme rangee est de type impair (indice 1)
 
      # on determine sa premiere brique, a partir du choix de la premiere brique du mur

      # si la premiere brique est entiere
      if first_brick_type == "1":
         # on prend une demi-brique
         first_brick_type_second = "1/2"
      # si la premiere brique est une demi-brique
      elif first_brick_type == "1/2":
         # on prend une brique entiere
         first_brick_type_second = "1"
      # sinon,
      else:
         # cas incompatible avec l'apareil en paneresses
         showError("the given first brick type is not allowed") 

      # on la definit
      self.odd_row = brick_row(brick_ref=self.brick_ref, disposition=self.disposition, first_brick_type=first_brick_type_second)
      # on lui attribue sa longueur
      self.odd_row.setLength(length)
      # on lui attribue son epaisseur de joints
      self.odd_row.setJointThickness(joint_thickness)
      # on calcule le nombre de briques necessaires
      self.odd_row.computeNbBricks(trend="max", rtol=1e-5)

   # fonction qui renvoie la longueur du mur
   def getLength(self):
      """getLength(self):

      this function returns the length of the wall

      parameters:

      - self: the wall itself
      """ 
 
      # si une des rangees n'est pas definie
      if self.even_row is None or self.odd_row is None:
         # la premiere rangee de briques n'a pas ete donnee et ne peut donc dire la longueur du mur
         showError("wall length is undefined, since the row in undefined")

      # on renvoie la longueur de la rangee de brique paire
      return self.even_row.getLength()

   # fonction qui renvoie l'epaisseur du mur
   def getThickness(self):
      """getThickness(self):

      this function returns the thickness of the wall

      parameters:

     - self: the wall itself
      """ 
 
      # si une des rangees n'est pas definie
      if self.even_row is None or self.odd_row is None:
         # la premiere rangee de briques n'a pas ete donnee et ne peut donc dire la longueur du mur
         showError("wall thickness is undefined, since the row in undefined")

      # on renvoie la longueur de la rangee de brique paire
      return self.even_row.getThickness()

   # pose le mur de briques sous la forme de corps rigides (modele, materiau, couleurs), par rapport a une origine donnee 0
   # le parement exterieur se trouve dans le plan xOz, et les briques sont posees suivant l'axe Ox
   def buildRigidWall(self, origin, model, material, colors=['BLUEx', 'REDxx'], rtol=1e-5):
      """buildRigidWall(self, origin, model, material, color, rtol=1e-5):

      this function builds the wall, as it generates a list of rigid avatars representing bricks of the wall

      parameters:

      - self: the wall itself
      - origin: location of origin of the wall
      - model: rigid model for the bricks
      - material: the bricks are made of this material
      - color: color of the contactors

      optional parameters:

      - rtol: relative tolerance used in floatting number comparaisons
      """

      # dans cette classe mere, cette methode n'est pas definie
      raise NotImplementedError("the method to build the wall is implemented in this super class")

# classe definissant un mur construit avec un apareil type paneresse simple, i.e. permet de construire un
# mur avec apareil en paneresse simple, pour des briques disposees en paneresse, un apraeil en boutisse
# pour des briques disposees en boutisse, et sur chant pour des briques disposees sur chant
class paneresse_simple(paneresse_generic):
   """class paneresse_simple:

      this class defines an objet representing a brick wall, using the so called "apareil en paneresses, simple"
   """

   # costructeur
   def __init__(self, brick_ref, disposition):
      """__init__(self, brick_ref, disposition):
         this function defines a new wall
         parameters:
            - self: the wall itself
            - brick_ref: a brick object describing the kind of brick used to build the wall
            - disposition: disposition of the brick in the wall, possible values are "paneresse", "boutisse" and "chant"
      """

      # on appelle le constructeur de la classe mere
      paneresse_generic.__init__(self, brick_ref, disposition)

   # pose le mur de briques sous la forme de corps rigides (modele, materiau, couleurs), par rapport a une origine donnee 0
   # le parement exterieur se trouve dans le plan xOz, et les briques sont posees suivant l'axe Ox
   def buildRigidWall(self, origin, model, material, colors=['BLUEx', 'REDxx'], rtol=1e-5):
      """buildRigidWall(self, origin, model, material, color, rtol=1e-5):

      this function builds the wall, as it generates a list of rigid avatars representing bricks of the wall

      parameters:

      - self: the wall itself
      - origin: location of origin of the wall
      - model: rigid model for the bricks
      - material: the bricks are made of this material
      - color: color of the contactors

      optional parameters:

      - rtol: relative tolerance used in floatting number comparaisons
      """

      # on calcule la tolerance absolue pour evaluer les valeurs reelles, a partir de la tolerance relative 
      # et la hauteur d'une brique
      atol = rtol*self.row_height

      # on cree un conteneur d'avatar pour stocker les briques de la rangees
      bodies=avatars()

      # s'il manque des donnees 
      if self.height is None or self.nb_rows is None or self.joint_thickness is None:
         # on affiche un avertissement
         showWarning("the wall can't be built since data are missing, an empty container is returned")
         # on sort de la fonction en renvoyant un container vide
         return bodies     

      # on initialise les coordonnees de l'origine de la rangee courante
      x = origin[0]
      y = origin[1]
      # on tient compte de la premeire epaisseur de joint
      z = origin[2] + self.joint_thickness

      # pour chaque rangee de brique
      for i in range(0, self.nb_rows):
         # on choisit la rangee de brique a poser, en fonction de la parite de l'indice
         if i % 2 == 0:
            # cas pair
            bodies += self.even_row.buildRigidRow(origin=[x, y, z], model=model, material=material, color=colors[0], rtol=rtol)
         else:
            # cas impair
            bodies += self.odd_row.buildRigidRow(origin=[x, y, z], model=model, material=material, color=colors[1], rtol=rtol)
         # on incremente la hauteur de l'origine de la prochaine rangee de brique, i.e. on l'augmente de la hauteur de la rangee
         # que l'on vient de poser + l'epaisseur du joint entre deux rangees
         z += self.row_height + self.joint_thickness

      # on renvoie la liste de corps generee
      return bodies

   # pose le mur de briques sous la forme de corps rigides (modele, materiau, couleurs), par rapport a une origine donnee 0
   # le parement exterieur se trouve dans le plan xOz, et les briques sont posees suivant l'axe Ox
   # le mur genere par cette focntion est pret pour le harpage, puisque les demi-briques pouvant aparaitre au debut ou la fin de
   # chaque rangee ne sont pas posees
   def buildRigidWallWithoutHalfBricks(self, origin, model, material, colors=['BLUEx', 'REDxx'], rtol=1e-5):
      """buildRigidWallWithoutHalfBricks(self, origin, model, material, color, rtol=1e-5):

      this function builds the wall, as it generates a list of rigid avatars representing bricks of the wall ; 
      the built wall is "harpage" ready, since half bricks have been removed 

      parameters:

      - self: the wall itself
      - origin: location of origin of the wall
      - model: rigid model for the bricks
      - material: the bricks are made of this material
      - color: color of the contactors

      optional parameters:

      - rtol: relative tolerance used in floatting number comparaisons
      """

      # on calcule la tolerance absolue pour evaluer les valeurs reelles, a partir de la tolerance relative 
      # et la hauteur d'une brique
      atol = rtol*self.row_height

      # on construit des rangees semblables aux rangees paires et impaires, ou les demi-briques ont ete retirees
      #   * pour la rangee paire
      even_row_whithout_half_bricks, even_row_shift = self.even_row.sameRowWithoutHalfBricks(rtol)
      #   * pour la rangee impaire
      odd_row_whithout_half_bricks, odd_row_shift = self.odd_row.sameRowWithoutHalfBricks(rtol)

      # on cree un conteneur d'avatar pour stocker les briques de la rangees
      bodies=avatars()

      # s'il manque des donnees 
      if self.height is None or self.nb_rows is None or self.joint_thickness is None:
         # on affiche un avertissement
         showWarning("the wall can't be built since data are missing, an empty container is returned")
         # on sort de la fonction en renvoyant un container vide
         return bodies     

      # on initialise les coordonnees de l'origine de la rangee courante
      x = origin[0]
      y = origin[1]
      # on tient compte de la premeire epaisseur de joint
      z = origin[2] + self.joint_thickness

      # pour chaque rangee de brique
      for i in range(0, self.nb_rows):
         # on choisit la rangee de brique a poser, en fonction de la parite de l'indice
         if i % 2 == 0:
            # cas pair
            bodies += even_row_whithout_half_bricks.buildRigidRow(origin=[x + even_row_shift, y, z], 
                         model=model, material=material, color=colors[0], rtol=rtol)
         else:
            # cas impair
            bodies += odd_row_whithout_half_bricks.buildRigidRow(origin=[x + odd_row_shift, y, z], 
                         model=model, material=material, color=colors[0], rtol=rtol)
         # on incremente la hauteur de l'origine de la prochaine rangee de brique, i.e. on l'augmente de la hauteur de la rangee
         # que l'on vient de poser + l'epaisseur du joint entre deux rangees
         z += self.row_height + self.joint_thickness

      # on renvoie la liste de corps generee
      return bodies
       
# classe definissant un mur construit avec un apareil type paneresse simple, i.e. permet de construire un
# mur avec apareil en paneresse simple, pour des briques disposees en paneresse et sur chant pour des briques disposees sur chant
class paneresse_double(paneresse_generic):
   """class paneresse_double:

   this class defines an objet representing a brick wall, using the so called "apareil en paneresses, double"
   """

   # costructeur
   def __init__(self, brick_ref, disposition):
      """__init__(self, brick_ref, disposition):

      this function defines a new wall

      parameters:

      - self: the wall itself
      - brick_ref: a brick object describing the kind of brick used to build the wall
      - disposition: disposition of the brick in the wall, possible values are "paneresse", "boutisse" and "chant"
      """

      # dans le cas d'un apareil en paneresses a double epaisseur, on ne peut utiliser la disposition en boutisse
      if disposition == "boutisse":
         # si l'utilisateur le demande, on affiche un message d'erreur
         showError("this disposition is incompatible with this kind of wall")

      # on appelle le constructeur de la classe mere
      paneresse_generic.__init__(self, brick_ref, disposition)

   # pose le mur de briques sous la forme de corps rigides (modele, materiau, couleurs), par rapport a une origine donnee 0
   # le parement exterieur se trouve dans le plan xOz, et les briques sont posees suivant l'axe Ox
   def buildRigidWall(self, origin, model, material, colors=['BLUEx', 'REDxx', 'JAUNE', 'VERTx'], rtol=1e-5):
      """buildRigidWall(self, origin, model, material, colors, rtol=1e-5):

         this function builds the wall, as it generates a list of rigid avatars representing bricks of the wall

         parameters:

         - self: the wall itself
         - origin: location of origin of the wall
         - model: rigid model for the bricks
         - material: the bricks are made of this material
         - color: color of the contactors

         optional parameters:

         - rtol: relative tolerance used in floatting number comparaisons
      """

      # on calcule la tolerance absolue pour evaluer les valeurs reelles, a aprtir de la tolerance relative 
      # et la hauteur d'une brique
      atol = rtol*self.row_height

      # on cree un conteneur d'avatar pour stocker les briques de la rangees
      bodies=avatars()

      # s'il manque des donnees 
      if self.height is None or self.nb_rows is None or self.joint_thickness is None:
         # on affiche un avertissement
         showWarning("the wall can't be built since data are missing, an empty container is returned")
         # on sort de la fonction en renvoyant un container vide
         return bodies     

      # on initialise les coordonnees de l'origine de la rangee courante
      x = origin[0]
      y = origin[1]
      # on tient compte de la premeire epaisseur de joint
      z = origin[2] + self.joint_thickness

      # pour chaque rangee de brique
      for i in range(0, self.nb_rows):
         # on choisit dans quel ordre poser les briques, en fonction de la parite de l'indice
         if i % 2 == 0:
            # cas pair
            
            # on pose une rangee paire de type pair
            bodies += self.even_row.buildRigidRow(origin=[x, y, z], model=model, material=material, color=colors[0], rtol=rtol)
            # puis une rangee de type impair, derriere
            bodies += self.odd_row.buildRigidRow(origin=[x, y + self.even_row.getThickness() + self.joint_thickness, z], 
                                                 model=model, material=material, color=colors[1], rtol=rtol)
         else:
            # cas impair

            # on pose une rangee paire de type impair
            bodies += self.odd_row.buildRigidRow(origin=[x, y, z], model=model, material=material, color=colors[2], rtol=rtol)
            # puis une rangee de type pair, derriere
            bodies += self.even_row.buildRigidRow(origin=[x, y + self.even_row.getThickness() + self.joint_thickness, z], 
                                                 model=model, material=material, color=colors[3], rtol=rtol)

         # on incremente la hauteur de l'origine de la prochaine rangee de brique, i.e. on l'augmente de la hauteur de la rangee
         # que l'on vient de poser + l'epaisseur du joint entre deux rangees
         z += self.row_height + self.joint_thickness

      # on renvoie la liste de corps generee
      return bodies
       

