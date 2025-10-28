import numpy
import math

from ...utilities.error import *

from ...shared.model      import *
from ...shared.bulk_behav import *

from .bulk import *

## @class rigid3d
# 
class rigid3d(bulk):
    """ classe rigid3d :
        define the bulk of a 3D rigid
        attributs:
        - avrd : gyration radius
        - inertia : inertia terms ([I1,I2,I3])
        - axis   : axis of the frame of the element
        methodes associees:
    """

    ## @brief constructor
    # @param number (1 by default) 
    def __init__(self, avrd=0., inertia=None, number=None):
        """ __init__(self, avrd=0., inertia=None, number=None):
            this function initializes a new rigid bulk, in 3D
            parameters:
               - self: the rigid bulk itself
               - avrd: average radius, i.e. the volume of the rigid avatar belonging the considered bulk is 4/3*pi*avrd^3
               - inertia: 3x3 inertia matrix for the rigid avatar belonging the considered bulk 
        """
        # un corps rigide est un element fini dont le support geometrique est un point
        bulk.__init__(self, elem_dim=0, connectivity=[1], physicalEntity='1', number=number)

        self.avrd    = avrd
        if inertia is None :
          self.inertia = numpy.array([0., 0., 0.])
        else :
          self.inertia = inertia
        # frame of the element
        self.axis   = numpy.array([ [1., 0., 0.], [0., 1., 0.], [0., 0., 1.] ])

    def setVolume(self, volume):
        """ bulk.setVolume(volume):
            this function sets the volume of the rigid
        """
        if volume < 0.:
           showError("a volume must be nonnegative!")
      
        self.avrd=pow(3.*volume/(4.*math.pi),1./3.)

    def setInertia(self, inertia):
        """ bulk.setInertia(inertia)
            where inertia is 3 elements list corresponding to [I1, I2, I3]
        """
        if inertia.shape == (3,) :
            self.inertia = numpy.array(inertia)
        else :
            msg = '[setInertia] inertia is of wrong size, must be 3 and is '+ str(inertia.size)
            showWarning(msg)
            return

    ## @brief set the frame of the bulk
    def setFrame(self, axis):
        """ bulk.setFrame(axis)
            axis   : axis of the frame (3 3D vectors)
        """
        if axis.shape != (3,3):
            msg = '[setFrame] axis is of wrong size, must be (3,3) and is '+ str(axis.shape)
            showWarning(msg)
            return

        self.axis   = numpy.array(axis)

    def applyLinearMapToFrame(self, q):
       """applyLinearMapToFrame(self, q):
          this function applies a linear map to the frame of the object.
          parameters:
             - self: the rigid bulk itself
             - q: the linear map expressed as a 3x3 matrix
       """
       self.axis = numpy.dot(q, self.axis)

