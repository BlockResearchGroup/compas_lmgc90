from .node.node import *

## @class nodes(dict)
#  node iterator
class nodes(dict):
    """ node container

    inherits from container class 'iterator'
    key is the number of the node
    """

    def addNode(self,noeud):
        """addNode(self,noeud)

        add a node to the container and index it with its number
        usage: nodes.addNode(node)
        """
        if isinstance(noeud,node):
          self[noeud.number]=noeud
        else: 
           warn='Only a node can be added in a node container\n'
           showWarning(warn)

        return self

    def applyAffineMap(self, q, center):
        for n in self.values():
            n.applyAffineMapToCoor(q, center)

    def applyTranslate(self, trans):
        for n in self.values():
            n.coor += trans

    def sortedKeys(self):
        return sorted(self.keys())

    def __iter__(self):
        return iter(self.values())
