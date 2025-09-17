from .bulk.bulk import *
from .groups import *

## @class bulks(list)
# bulk iterator
class bulks(list):
    """ bulk container

    inherits from container class 'sequence_container'
    i.e. keys are automatically attributed
    """

    def addBulk(self,new):
        """addBulk(self,new)
           add a bulk to the container and index it with its number
        """
        # si l'objet passe en argument est bien un bulk
        if isinstance(new,bulk):
           # on l'ajoute dans le container
           self.append(new)
        else: 
           warn='Only bulk can be added to a bulk container\n'
           showWarning(warn)
            
    def draw(self,Nodes,fen):
        pass

