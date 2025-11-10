# module definissant une classe "rangee de briques"
import math

from ..avatars import *
from ..utilities.error import *

from .brick import *

class brick_row():
   """class brick_row():
      this class defines an objet represnting a brick row
   """

   # constructeur
   def __init__(self, brick_ref, disposition, first_brick_type):
      """__init__(self, brick_ref, disposition, first_brick_type):
         this function defines a new brick row
         parameters:
            - self: the brick row itself
            - brick_ref: a brick object describing the kind of brick used to build the brick row
            - disposition: disposition of the brick in the wall, possible values are "paneresse", "boutisse" and "chant"
            - first_brick_type: describe the kind of brick begining the row:
                 - "1": for a whole brick
                 - "1/2": for a half of a brick
                 - "1/4": for a quarter of a brick
      """
      # si la disposition des briques dans la rangee est connue
      if disposition == "paneresse" or disposition == "boutisse" or disposition == "chant":
         # on stocke la disposition des briques dans la rangee
         self.disposition = disposition
      # sinon,
      else:
         # on affiche un message d'erreur
         showError("unknown disposition!")

      # construction de la bibliotheque de briques
      # fonction de la disposition des briques
      #    * cas de la disposition en paneresse
      if self.disposition == "paneresse":
         self.bricks = { "1" : brick3D(name="1", lx=brick_ref.lx, ly=brick_ref.ly, lz=brick_ref.lz),           # brique entiere : copie de la brique de reference 
                         "1/2" : brick3D(name="1/2", lx=0.5*brick_ref.lx, ly=brick_ref.ly, lz=brick_ref.lz),   # demi-brique
                         "1/4" : brick3D(name="1/4", lx=0.25*brick_ref.lx, ly=brick_ref.ly, lz=brick_ref.lz),  # quart de brique
                         "3/4" : brick3D(name="3/4", lx=0.75*brick_ref.lx, ly=brick_ref.ly, lz=brick_ref.lz) } # trois quart de brique
      #    * cas de la disposition en boutisse
      elif self.disposition == "boutisse":
         self.bricks = { "1" : brick3D(name="1", lx=brick_ref.ly, ly=brick_ref.lx, lz=brick_ref.lz),          # brique entiere : copie de la brique de reference, tournee de pi/2, autour de
                                                                                                            # de l'axe G + e_z 
                         "1/2" : brick3D(name="1/2", lx=0.5*brick_ref.ly, ly=brick_ref.lx, lz=brick_ref.lz) } # demi-brique
         # N.B. les quart et trois quarts de brique n'existent pas pour cette disposition
      #    * cas de la disposition de chant
      elif self.disposition == "chant":
         self.bricks = { "1" : brick3D(name="1", lx=brick_ref.lx, ly=brick_ref.lz, lz=brick_ref.ly),          # brique entiere : copie de la brique de reference, tournee de pi/2, autour de
                                                                                                            # de l'axe G + e_x 
                         "1/2" : brick3D(name="1/2", lx=0.5*brick_ref.lx, ly=brick_ref.lz, lz=brick_ref.ly) } # demi-brique
         # N.B. les quart et trois quarts de brique n'existent pas pour cette disposition
       
      # on stocke le type de de la premiere brique de la rangee
      
      # si le type existe 
      if first_brick_type in list(self.bricks.keys()):
         # on le stocke
         self.first_brick_type = first_brick_type
      # sinon,
      else:
         # on affiche un message d'erreur
         showError("this brick type can't exist in this row")

      # la valeurs des autres attributs ne sont pas encore connus
      self.nb_bricks = None       # le nombre de briques dans la rangee
      self.joint_thickness = None # l'epaisseur du joint
      self.length = None          # la longueur totale de la rangee (briques + joints)

   # donnee de la longueur de la rangee, par la donnee du nombre de briques dans la rangee
   # N.B.: le nombre de briques peut etre fractionnaire, e.g 2 briques entiere plus un quart de brique, donnent une longueur de 2.25
   def setNumberOfBricks(self, nb_bricks, rtol=1e-5):
      """setNumberOfBricks(self, nb_bricks, rtol=1e-5):
         this function allows to set the number of bricks in the brick row.
         parameters:
            - self: the brick row itself
            - nb_bricks: the given number of bricks; this number could be fractional, for exemple it's 2.5 for two bricks and a half
         optional parameters:
            - rtol: relative tolerance used in floatting number comparaisons
      """

      # on calcule la tolerance absolue pour evaluer les valeurs reelles, a partir de la tolerance relative 
      # et la longueur d'une brique entiere
      atol = rtol*self.bricks["1"].lx

      # on verifie le nombre de briques

      # on calcule la partie fractionnaire du nombre de briques
      frac = nb_bricks - math.floor(nb_bricks)

      # si elle n'est pas egale a 0 ou 0.5
      if math.fabs(frac) > atol and math.fabs(frac - 0.5) > atol:
         # si on est pas dans le cas 1/4 ou 3/4 de la disposition "paneresse"
         if self.disposition != "paneresse" or (math.fabs(frac - 0.25) > atol and math.fabs(frac - 0.75) > atol):
            # on affiche un message d'erreur
            showError("number of bricks incompatible with the choosen disposition")

      # si tout va bien, on enregistre le nombre de briques de la rangee
      self.nb_bricks = nb_bricks

   # donnee de l'epaisseur du joint
   def setJointThickness(self, joint_thickness):
      """setJointThickness(self, joint_thickness):
         this function allows to set the joint thickness for the brick row.
         parameters:
            - self: the brick row itself
            - joint_thickness: the given joint thickness
      """

      self.joint_thickness = joint_thickness

   # donnee de la longueur de la rangee
   def setLength(self, length):
      """setLength(self, length):
         this function allows to set the length for the brick row.
         parameters:
            - self: the brick row itself
            - length: the given length
      """

      self.length = length

   # evaluation de la longueur de la rangee, a partir d'un nombre de briques et d'une epaisseur de joint
   # passees en argument
   def evaluateLength(self, nb_bricks, joint_thickness):
      """evaluateLength(self, nb_bricks, joint_thickness):
         this function evaluates ans returns the length for the brick row, using given number of bricks and joint thickness.
         parameters:
            - self: the brick row itself
            - nb_bricks: the given number of bricks; this number could be fractional, for exemple it's 2.5 for two bricks and a half
            - joint_thickness: the given joint thickness
         returned value: the evaluated length
      """

      # calcul du nombre de joints
 
      # on calcule le nombre de briques pris la premiere brique
      if self.first_brick_type == "1":
         nb_bricks_first = 1
      elif self.first_brick_type == "1/2":
         nb_bricks_first = 0.5
      elif self.first_brick_type == "1/4":
         nb_bricks_first = 0.25
      elif self.first_brick_type == "3/4":
         nb_bricks_first = 0.75
      else:
         showError("unknown brick type")

      # on calcule le nombre de briques entieres a poser apres la premiere
      nb_remaining_bricks = nb_bricks - nb_bricks_first
      # on clacule sa partie entiere
      nb_remaining_bricks_floor = math.floor(nb_remaining_bricks)

      # si ce nombre est entier
      if nb_remaining_bricks == nb_remaining_bricks_floor:
         # il constitue le nombre de joints
         nb_joints = nb_remaining_bricks
      # sinon,
      else:
         # il faut prendre sa partie entiere, augmentee de 1 (morceau de brique a coller a la fin)
         nb_joints =  nb_remaining_bricks_floor + 1.

      # on en deduit la longueur du mur
      length = nb_bricks*self.bricks["1"].lx + nb_joints*joint_thickness

      # on renvoie la longueur de mur calculee
      return length

   # calcule de la longueur de la rangee, a partir du nombre de briques et de l'epaisseur des joints
   # precondition : le nombre de briques et le joint on ete renseignes
   def computeLength(self):
      """computeLength(self):
         this function computes and stores the length for the brick row, using the number of bricks and joint thickness
         defined as attributs of the brick row.
         parameters:
            - self: the brick row itself
      """

      # si les donnees requises sont absentes
      if self.nb_bricks is None or self.joint_thickness is None:
         # on affcihe un avertissement
         showWarning("data is missing to compute the row length")
         # et on quitte la fonction
         return
 
      # on calcule la longueur du mur
      self.length = self.evaluateLength(self.nb_bricks, self.joint_thickness)

   # calcule de l'epaisseur du joint, a partir du nombre de briques et de la longueur du mur
   # precondition : le nombre de briques et la longueur du mur on ete renseignes
   def computeJointThickness(self):
      """computeJointThickness(self):
         this function computes and stores the joint thickness for the brick row, using the number of bricks and length
         defined as attributs of the brick row.
         parameters:
            - self: the brick row itself
      """

      # si les donnees requises sont absentes
      if self.nb_bricks is None or self.length is None:
         # on affcihe un avertissement
         showWarning("data is missing to compute joint thickness")
         # et on quitte la fonction
         return
 
      # calcul de la longueur de la rangee

      # calcul du nombre de joints
 
      # on calcule le nombre de briques pris la premiere brique
      if self.first_brick_type == "1":
         nb_bricks_first = 1
      elif self.first_brick_type == "1/2":
         nb_bricks_first = 0.5
      elif self.first_brick_type == "1/4":
         nb_bricks_first = 0.25
      elif self.first_brick_type == "3/4":
         nb_bricks_first = 0.75
      else:
         showError("unknown brick type")

      # on calcule le nombre de briques entieres a poser apres la premiere
      nb_remaining_bricks = self.nb_bricks - nb_bricks_first
      # on clacule sa partie entiere
      nb_remaining_bricks_floor = math.floor(nb_remaining_bricks)

      # si ce nombre est entier
      if nb_remaining_bricks == nb_remaining_bricks_floor:
         # il constitue le nombre de joints
         nb_joints = nb_remaining_bricks
      # sinon,
      else:
         # il faut prendre sa partie entiere, augmentee de 1 (morceau de brique a coller a la fin)
         nb_joints =  nb_remaining_bricks_floor + 1.

      # on en deduit l'epaisseur du joint
      self.joint_thickness = (self.length - self.nb_bricks*self.bricks["1"].lx)/nb_joints

   # calcule deux nombre de briques fournissant des murs dont la longueur encadre une longueur desiree,
   # passee en argument, connsaissant l'epaisseur du joint souhaitee
   def limitNbBricks(self, length, joint_thickness):
      """limitNbBricks(self, length, joint_thickness):
         this function computes and returns two number of bricks, using given joint thickness and length.
         Each number of bricks coresponds to a brick row which the length is close to the given length.
         parameters:
            - self: the brick row itself
            - length: the given length
            - joint_thickness: the given joint thickness
         returned value: the couple of number of bricks computed
      """

      # on calcule le nombre de briques pris la premiere brique
      if self.first_brick_type == "1":
         nb_bricks_first = 1
      elif self.first_brick_type == "1/2":
         nb_bricks_first = 0.5
      elif self.first_brick_type == "1/4":
         nb_bricks_first = 0.25
      elif self.first_brick_type == "3/4":
         nb_bricks_first = 0.75
      else:
         showError("unknown brick type")

      # on recupere la longueur de la premiere brique du mur
      first_brick_length = self.bricks[self.first_brick_type].lx

      # on calcule le nombre de briques entieres que peut contenir le mur, apres la pose de la premiere brique
      nb_whole_bricks = math.floor((length - first_brick_length)/(self.bricks["1"].lx + joint_thickness)) 

      # on en deduit le nombre de briques dans le mur, sans compter la derniere
      nb_first_bricks = nb_bricks_first + nb_whole_bricks

      # on calcule la longeur que devrait avoir la derniere brique, pour atteindre exactement la longueur voulue
      last_brick_length = length - first_brick_length - nb_whole_bricks*(self.bricks["1"].lx + joint_thickness) 

      # on cherche un encadrement de  la taille de la derniere brique, par dichotomie

      # l'encadrement le plus grossier, consiste a ne pas en ajouter ou en ajouter une entiere
      min_nb = 0.
      max_nb = 1.

      # si on obtient un mur plus long que desire en ajoutant une 1/2 brique
      if last_brick_length <= self.bricks["1/2"].lx:
         # on reduit la borne superieur de l'encadrement a une demi brique
         max_nb = 0.5
         # si on est dans le cas de la disposition paneresse
         if self.disposition == "paneresse":
            # on ajuste au 1/4 de brique pres

            # si on obtient un mur plus long que desire en ajoutant un 1/4 de brique
            if last_brick_length <= self.bricks["1/4"].lx:
               # on reduit la borne superieur de l'encadrement a un 1/4 de brique
               max_nb = 0.25
            # sinon
            else: 
               # on remonte la borne superieure de l'encadrement a 1/4 de brique
               min_nb = 0.25
      # sinon,
      else:
         # on remonte la borne superieure de l'encadrement a 1/2 brique
         min_nb = 0.5
         # si on est dans le cas de la disposition paneresse
         if self.disposition == "paneresse":
            # on ajuste au 1/4 de brique pres

            # si on obtient un mur plus long que desire en ajoutant un 3/4 de brique
            if last_brick_length <= self.bricks["3/4"].lx:
               # on reduit la borne superieur de l'encadrement a un 3/4 de brique
               max_nb = 0.75
            # sinon
            else: 
               # on remonte la borne superieure de l'encadrement a 3/4 de brique
               min_nb = 0.75

      # on en deduit l'enadrement du nombre de brique du mur
      return [nb_first_bricks + min_nb, nb_first_bricks + max_nb]

   # calcul du nombre de briques de la rangee, a partir de la longueur du mur, de l'epaisseur des joints
   # et d'une tendance de variation pour l'epaisseur du joint. En effet, la donnee d'une longueur de mur
   # et d'une epaisseur de joint peuvent aboutir a deux nombre de briques, approchant la longueur du mur
   # par valeur superieures et inferieures, sans l'atteindre. Aussi, la liberte de changer l'epaisseur du
   # joint, en indiquant si l'on peut l'augmenter ou la diminuer, permet de retrouver un mur a la bonne longueur
   # precondition : la longueur du mur et l'epaisseur d'un joint ont ete renseignes
   def computeNbBricks(self, trend="max", rtol=1e-5):
      """commputeNbBricks(self, trend="max", rtol=1e-5):
         this function computes an optimal number of bricks, using the joint thickness and length
         defined as attributs of the brick row. It computes bounds using the function evaluateNbBricks.
         If one aof the bounds is the exact solution, it's stored, else the joint thickness is altered to
         find a solution. The joint thickness is increased or decreased depending on user's choice.
         parameters:
            - self: the brick row itself
         optional parameters:
            - trend: this parameter is used to choose how the joint thickness can change
                 - "max": if the joint thickness could only be increased
                 - "min": if the joint thickness could only be decreased
            - rtol: relative tolerance used in floatting number comparaisons
      """

      # si les donnees requises sont absentes
      if self.length is None or self.joint_thickness is None:
         # on affiche un avertissement
         showWarning("data is missing to compute the required number of bricks")
         # et on quitte la fonction
         return
     
      # on evalue les deux nombre de briques possibles
      [min_nb_bricks, max_nb_bricks] = self.limitNbBricks(self.length, self.joint_thickness) 

      # on teste si on tombe dans le cas particulier ou la solution exacte existe
      
      # on calcule la tolerance absolue pour evaluer les valeurs reelles, a partir de la tolerance relative 
      # et la longueur d'une brique entiere
      atol = rtol*self.bricks["1"].lx

      # on evalue la longeur du mur, en utilisant le plus petit nombre de briques
      min_length = self.evaluateLength(min_nb_bricks, self.joint_thickness)
      # si on trouve la longueur souhaitee pour le mur
      if math.fabs(min_length - self.length) < atol:
         # on choisit ce nombre de briques
         self.setNumberOfBricks(min_nb_bricks)
         # on a fini
         return

      # on evalue la longeur du mur, en utilisant le plus grand nombre de briques
      max_length = self.evaluateLength(max_nb_bricks, self.joint_thickness)
      # si on trouve la longueur souhaitee pour le mur
      if math.fabs(max_length - self.length) < atol:
         # on choisit ce nombre de briques
         self.setNumberOfBricks(max_nb_bricks)
         # on a fini
         return

      # sinon, on choisit par rapport a la tendance de modification de la taille du joint

      # si on souhaite garantie une epaisseur de joint minimale
      if trend == "max":
         # on choisit le plus petit nombre de briques
         self.setNumberOfBricks(min_nb_bricks)
      # si on prefere diminuer l'epasisseur du joint
      elif trend == "min":
         # on choisit le plus grand nombre de briques
         self.setNumberOfBricks(max_nb_bricks)
      else:
         # si le choix est impossible, on affiche une erreur
         showError("unknown trend")

      # on calcule la nouvelle epaisseur du joint
      self.computeJointThickness()

   # renvoie la longueur de la rangee
   def getLength(self):
      """getLength(self):
         this function returns the stored length of the brick row
         parameters:
            - self: the brick row itself
      """

      # si la longeur de la rangee n'a pas ete calculee, ou donnee
      if self.length is None:
         # on affiche un avertissement
         showWarning("row length is unknown")
     
      # on renvoie la longueur de la rangee
      return self.length

   # renvoie l'epaisseur du joint
   def getJointThickness(self):
      """getJointThickness(self):
         this function returns the stored joint thickness of the brick row
         parameters:
            - self: the brick row itself
      """

      # si la longeur de l'epaisseur du joint n'a pas ete calculee, ou donnee
      if self.joint_thickness is None:
         # on affiche un avertissement
         showWarning("joint thickness is unknown")
     
      # on renvoie l'epaisseur du joint
      return self.joint_thickness

   # renvoie l'epaisseur de la rangee
   def getThickness(self):
      """getThickness(self):
         this function returns the thickness of the brick row
         parameters:
            - self: the brick row itself
      """

      # on renvoie la largeur d'une brique
      return self.bricks["1"].ly

   # fonction qui construit une nouvelle rangee, semblable a la rangee de brique consideree (i.e. meme epaisseur de joint, meme longueur), 
   # mais ou les demi-briques seraient remplacees par des vides
   def sameRowWithoutHalfBricks(self, rtol=1e-5):
      """sameRowWithoutHalfBricks(self, rtol=1e-5):
         this function builds and returns a new brick row, which is similar to the considered brick row (same length, same joint thickness)
         but where half bricks have been removed
         parameters:
            - self: the brick row itself
         optional parameters:
            - rtol: relative tolerance used in floatting number comparaisons
         returned value: the new brick row and a shift used to remove the first half brick, if any
      """

      # si la disposition est en boutisse
      if self.disposition == "boutisse":
         # il semble inutile d'utitliser cette methode
         showError("this function is incompatible with the disposition")

      # si le nombre de briques ou l'epaisseur de joint ne sont pas definies
      if self.nb_bricks is None or self.joint_thickness is None:
         # on ne peut pas utiliser cette methode
         showError("the brick row must be totally defined to use with function")

      # on calcule la tolerance absolue pour evaluer les valeurs reelles, a partir de la tolerance relative 
      # et la longueur d'une brique entiere
      atol = rtol*self.bricks["1"].lx

      # on verifie le nombre de briques

      # on calcule la partie fractionnaire du nombre de briques
      frac = self.nb_bricks - math.floor(self.nb_bricks)

      # si elle n'est pas egale a 0 ou 0.5, i.e. si le mur contient 1/4 ou 3/4 de brique
      if math.fabs(frac) > atol and math.fabs(frac - 0.5) > atol:
         # on ne peut appliquer cette methode a la rangee
         showError("half bricks can't be removed from a wall begining by involving 1/4 or 3/4 brick")
 
      # si la rangee commence par 1/4 ou 3/4 de brique
      if self.first_brick_type == "1/4" or self.first_brick_type == "3/4":
         # on ne peut appliquer cette methode
         showError("half bricks can't be removed from a wall begining by a 1/4 or 3/4 brick")

      # ici, on est sur que la rangee commence pae une brique, ou une demi-brique et que sa longueur
      # est un nombre entier de brique, ou un nombre entier de nrique plus une demi

      # si la premiere brique de la rangee est une demi-brique
      if self.first_brick_type == "1/2":
         # on va utiliser un shift pour la supprimer

         # la longueur du shift et la longueur d'une demi-brique, augmentee d'une epaisseur de joint
         shift = self.bricks["1/2"].lx + self.joint_thickness

         # on calcule le nombre de briques dans la nouvelle rangee
         new_nb_bricks = math.floor(self.nb_bricks - 0.5)
      # si la pemiere brique est entiere
      elif self.first_brick_type == "1":
         # on n'a pas besoin de shift
         shift = 0.e0

         # on calcule le nombre de briques dans la nouvelle rangee
         new_nb_bricks = math.floor(self.nb_bricks) 
      # sinon,
      else:
         showError("unknown brick type")

      # on reconstruit la brique de reference utilisee pour ce mur, en fonction de la disposition
      #    * cas de la disposition en paneresse
      if self.disposition == "paneresse":
         brick_ref = brick3D(name="1", lx=self.bricks["1"].lx, ly=self.bricks["1"].ly, lz=self.bricks["1"].lz)
      #    * cas de la disposition sur chant
      elif self.disposition == "chant":
         brick_ref = brick3D(name="1", lx=self.bricks["1"].lx, ly=self.bricks["1"].lz, lz=self.bricks["1"].ly)
      else:
         showError("unknown brick disposition")

      # on construit la nouvelle rangee de brique, en conservant la brique de reference, la disposition, mais
      # en partant necessairement d'une brique entiere
      new_row = brick_row(brick_ref=brick_ref, disposition=self.disposition, first_brick_type="1")
      # on lui on attribue le nombre de briques qu'on a calcule
      new_row.setNumberOfBricks(new_nb_bricks)
      # on on lui attribue la meme epaisseur de joint que la rangee courante
      new_row.setJointThickness(self.joint_thickness)
      # on calcule sa longueur
      new_row.computeLength()

      # on renvoie la nouvelle rangee de briques et le shift associe
      return new_row, shift

   # pose la rangee de brique sous la forme de corps rigides (modele, materiau, couleur donnes), par rapport a une origine donnee 0
   # le parement exterieur se trouve dans le plan xOz, et les briques sont posees suivant l'axe Ox
   def buildRigidRow(self, origin, model, material, color, rtol=1e-5):
      """buildRigidRow(self, origin, model, material, color, rtol=1e-5):
         this function builds the row, as it generates a list of rigid avatars represnting bricks of the row
         parameters:
            - self: the brick row itself
            - origin: location of origin of th brick row
            - model: rigid model for the bricks
            - material: the bricks are made of this material
            - color: color of the contactors
         optional parameters:
            - rtol: relative tolerance used in floatting number comparaisons
      """

      # on calcule la tolerance absolue pour evaluer les valeurs reelles, a aprtir de la tolerance relative 
      # et la longueur d'une brique entiere
      atol = rtol*self.bricks["1"].lx

      # on cree un conteneur d'avatar pour stocker les briques de la rangees
      bodies=avatars()

      # s'il manque des donnees 
      if self.length is None or self.nb_bricks is None or self.joint_thickness is None:
         # on affiche un avertissement
         showWarning("the row can't be built since data are missing, an empty container is returned")
         # on sort de la fonction en rnvoyant un container vide
         return bodies     

      # on initialise la position sur l'axe Ox du centre d'inertie de la brique courante
      x = origin[0]
      # on calcule la position sur l'axe Oy du centre d'inertie des briques de la rangee
      # i.e. l'origine plus la demi-largeur d'une brique
      y = origin[1] + 0.5*self.bricks["1"].ly
      # on calcule la position sur l'axe Oz du centre d'inertie des briques de la rangee
      # i.e. l'origine plus la demi-hauteur d'une brique
      z = origin[2] + 0.5*self.bricks["1"].lz

      # on pose la premiere brique de la rangee

      # on recupere la premiere brique
      first_brick = self.bricks[self.first_brick_type]

      # on incremente la position sur l'axe Ox du centre d'inertie de la brique courante
      x += 0.5*first_brick.lx
      # on peut alors poser la premiere brique
      bodies += first_brick.rigidBrick(center=[x, y, z], model=model, material=material, color=color)
      # on incremente l'abscisse du centre d'inertie de la prochaine brique, 
      # pour obtenir la position du bord gauche de la prochaine brique
      x += 0.5*first_brick.lx + self.joint_thickness

      # on complete avec des briques entieres, jusqu'au moment de poser la derniere
      
      # on recupere la brique entiere
      whole_brick = self.bricks["1"]

      # tant qu'on a la place d'ajouter plus qu'une brique entiere
      while math.fabs(self.length + origin[0] - (x + whole_brick.lx)) > atol and self.length + origin[0] - (x + whole_brick.lx) > 0 :
         # on incremente la position sur l'axe Ox du centre d'inertie de la brique courante
         x += 0.5*whole_brick.lx
         # on pose une nouvelle brique entiere
         bodies += whole_brick.rigidBrick(center=[x, y, z], model=model, material=material, color=color)
         # on incremente l'abscisse du centre d'inertie de la prochaine brique, 
         # pour obtenir la position du bord gauche de la prochaine brique
         x += 0.5*whole_brick.lx + self.joint_thickness

      # ici, il ne reste la place que pour une seule brique, entiere ou non

      # on calcule la taille de la derniere brique a placer (i.e. brique entiere, 1/2 brique, 1/4 brique ou 3/4 brique)
      last_brick_length = self.length + origin[0] - x           

      # on cherche la derniere brique dans la liste des briques disponibles
      
      # au depart on n'a pas trouve la derniere brique
      last_brick = None
      # pour chaque brique de la liste
      for key in list(self.bricks.keys()):
         # si la longueur de la brique permet de finir (exactement) la rangee
         if math.fabs(last_brick_length - self.bricks[key].lx) < atol:
            # on a trouve la derniere brique,
            # on la stocke
            last_brick = self.bricks[key]
            # et on quitte la boucle
            break
     
      # si on n'a pas trouve la brique su'il fallait
      if last_brick is None:
         # on affiche un message d'erreur (probleme de consistance)
         showError("impossible to find a brick to finish the row")

      # on ajoute la derniere brique

      # on incremente la position sur l'axe Ox du centre d'inertie de la brique courante
      x += 0.5*last_brick.lx
      # on pose la derniere brique
      bodies += last_brick.rigidBrick(center=[x, y, z], model=model, material=material, color=color)

      # on renvoie la liste de corps generee
      return bodies
        
