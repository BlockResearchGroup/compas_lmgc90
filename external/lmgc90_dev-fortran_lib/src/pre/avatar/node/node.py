## module node
# define node class

import sys
import numpy

from ...config.lmgc90dicts import *
from ...utilities.error    import *

from ...models       import *
from ...shared.model import *

from .dof import *


## @class node
#  Class describing a node\n
#  \n
# attributs: \n
# number its number (as an integer)\n
# coor coordinates \n
# dofs degrees of freedom \n 
# \n
# methods: \n
#  __init__ constructor \n
#  defineDof: defines the dofs associated to the node (depends on the model) \n
#  imposeDrivenDof: imposes degrees of freedom\n
#  relaxDrivenDof: frees a dof previously imposed\n
#  translate: translate the node\n
class node():
    """
    Class defining the node entity
    """
    ## node constructor
    #
    def __init__(self,coor=None,number=0):
        """ __init__(self,coor=None,number=0):

        initialize a node entity.
        coor an array defining the corrdinates
        """
        self.number = number
        if coor is None:
            self.coor   = []
        else :
            self.coor   = coor

        self.ntype = dimensionTypeNode[coor.shape[0]]

        self.dof   = None

        # necessaire pour garder le lien avec le maillage de depart
        if number != 0:    
          self.originalnumber=number
        else:
          self.originalnumber=None

        if not self.checkType():
            msg = 'instanciating node: uncompatible type/dimension '
            msg += self.ntype + '\t' + str(self.coor.size)
            showError(msg)

    ## build the dof container of the node
    def defineDof(self,mod):
        
        if not isinstance(mod,model):
            print("\tTo define a model on a node an entity 'model' is needed ")
            sys.exit(1)
                  
        if self.dof is None:
            self.dof = dof(mod.nbdof, self.coor.shape)
        else:
            if self.dof.nbdof != mod.nbdof:
                print('\t A node belonging to two elements has different number of dof from one to the other')
                sys.exit(1)
                    
    ## impose dofs
    def imposeDrivenDof(self,component,description='predefined',ct=0.,amp=0.,omega=0.,
                        phi=0.,rampi=0.,ramp=0.,evolutionFile='unknown',dofty='unknown'):
        
        if self.dof != None:
            self.dof.imposeDrivenDof(component,description,ct,amp,omega,phi,rampi,ramp,evolutionFile,dofty)
        else:
            msg='Cannot impose dof on node %s' % self.number
            showWarning(msg)
            
    # relache des ddls
    def relaxDrivenDof(self,composantes):
       # si une condition limite (ou initiale) a ete imposee
       if self.dof != None:
          # on relcahe les conditions limites concernant les composantes
          # passees en argument
          self.dof.relaxDrivenDof(composantes)
        
    def imposeInitValue(self,composantes,values):
        if self.dof != None:
            self.dof.imposeInitValue(composantes,values)

    ## \brief check type and size consistency
    def checkType(self):
        res = True
        dim = -1
        for key in dimensionTypeNode:
            if self.ntype in dimensionTypeNode[key]:
                dim = key
        if dim == -1:
            msg = '[checkType] unknown node type ' + self.ntype
            showError(msg)
        if dim != self.coor.size :
            res = False
        return res

    ## @brief tranlate the node
    #  The coordinates to change depends on type
    def translate(self, dx, dy, dz):
        if numpy.size(self.coor) == 2:
            self.coor[0] += dx
            self.coor[1] += dy
        elif numpy.size(self.coor) == 3:
            self.coor[0] += dx
            self.coor[1] += dy
            self.coor[2] += dz
        else:
            msg = '[translate]: do not know how to apply translation to a node with ' + numpy.size(self.coor) + 'coordinates'
            showWarning(msg)

    ## @brief scale the node
    #  The coordinates to change depends on type
    def scale(self, scale):
        self.coor *= scale
        
    ## @brief apply an affine map to the node coordinates
    def applyAffineMapToCoor(self, q, center):
        """applyAffineMapToCoor(self, q, center):

        this function applies an affine map to the node coordinates.

        parameters:

        - self: the node itself
        - q: a linear map expressed as a 3x3 matrix
        - center: coordinates of the reference point of the affine map
        """

        assert self.coor.size == center.size, 'wrong size of center input'

        self.coor = center + numpy.dot( q, self.coor-center )

