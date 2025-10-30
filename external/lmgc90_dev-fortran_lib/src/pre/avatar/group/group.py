from ..nodes import *
from ..bulks import *
from ..contactors import *
#--------------------------------------------------------------
#     G R O U P ( S )
#..............................................................

# g r o u p
            
class group():
    """ class defining the basic group entities
        methodes associees:
        - __init__
        - addElement
        - addNode
    """
    
    ## @todo on vire dimension ?
    def __init__(self,name,dimension=1):
        """__init__(self,name,dimension=1)
           create a group of elements, nodes and contactors
        """
        self.name         = name
        #self.dimension    = dimension
        self.bulks        = bulks()
        self.nodes        = nodes()
        self.contactors   = contactors()
         

    def addBulk(self,bulk):
        """addBulk(self,bulk)
           add a bulk to the group
        """

        self.bulks.addBulk(bulk)
            
            
    def addNode(self,node):
        """addNode(node)
          add a node to the group
        """
        self.nodes.addNode(node)

    def addContactor(self,contactor):
        """addNode(node)
          add a contactor to the group
        """
        self.contactors.addContactor(contactor)


