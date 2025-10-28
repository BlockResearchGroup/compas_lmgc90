
from ..utilities.error import *

# classe arbre n-aire
class nTree():
   """class nTree():
      yet another n-Tree implementation
   """

   # initialisateur
   def __init__(self, value=None):
      """__init__(self, value=None):
         this function initializes a new n-tree.
         The given value is stored and the childs (trees)
         are undefined.
         parameters:
            - self: the n-tree itself
            - value=None: the value to be stored in the root of the n-tree
      """
      # on stocke la valeur a la racine de l'arbre
      self.root = value
      # on initialise a vide la liste des fils
      self.childs = []

   # ajout d'un arbre comme un nouveau fils
   def addChild(self, tree):
      """addChild(self, tree):
         this function adds a n-tree to another, as a new child of it.
         parameters:
            - self: the n-tree itself
            - tree : the tree to be added as a child of the given tree
      """
      # on ajoute le nouvel arbre fils
      self.childs.append(tree)

      # on renvoie une reference a l'objet (utile ppour la construction de l'arbre par emboitement)
      return self

   # ajout de plusieurs arbres comme nouveau fils
   def addChilds(self, trees):
      """addChilds(self, trees):
         this function adds n-trees to another, as a new childs of it.
         parameters:
            - self: the n-tree itself
            - trees : the trees to be added as a childs of the given tree
      """
      # si la liste des futurs arbres fils n'est pas une liste
      if not isinstance(trees, list):
         # on affiche un message d'erreur
         showError("the given list of n-trees is not a list!")
      # on ajoute les nouveaux arbres fils
      
      # pour chaque nouvel arbre fils
      for tree in trees:
         self.childs.append(tree)

      # on renvoie une reference a l'objet (utile ppour la construction de l'arbre par emboitement)
      return self

