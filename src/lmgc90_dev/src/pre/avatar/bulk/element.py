from ...utilities.error import *

from ...shared.model import *

from .bulk import *

# E l e m e n t 

class element(bulk):
    
    """ class element(bulk) :
        define an element
        attributs:
        methodes associees:
        - get
    """
    
    def __init__(self, elem_dim, connectivity, physicalEntity='1', geometricalEntity='1', number=None, nbNodes=None):
        """ __init__(self, elem_dim, connectivity, physicalEntity='1', geometricalEntity='1', number=None, nbNodes=None)
           this function initializes a new element
           parameters:
              - self: the bulk itself
              - elem_dim: dimension of the element (volumic=3, surfacic=2, lineic=1, point=0)
              - connectivity: connectivity of the element
           optional parameters:
              - physicalEntity=1: physical entity at which belongs the element; used to define groups
                of the avatar belonging the element
              - geometricalEntity='1': geometrical entity to which belongs the element (defined in gmsh meshes files only);
                useful to differentiate several bodies stored in one mesh file
        """
        bulk.__init__(self, elem_dim, connectivity, physicalEntity, geometricalEntity, number=number, nbNodes=nbNodes)
 
    def get(self,attribut):
        try:
            sortie = getattr(self, attribut)
        except:
            sortie = None
            print('no attribut %s this element %s\n' % (attribut, self.number))
            
        return sortie 

