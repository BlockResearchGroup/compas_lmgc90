
import numpy

from ...utilities.error import *

from .bulk import *

## @class rigid2d
# 
class rigid2d(bulk):
    """ classe rigid2d(bulk) :
        define the bulk of a 2D rigid
        attributs:
        - avrd : average radius
        - gyrd : gyration radius
        - axis   : axis of the frame of the element
        methods :
    """

    ## @brief constructor
    # @param number (1 by default) 
    def __init__(self, avrd=0., gyrd=0., number=None):
        """ __init__(self, avrd=0., gyrd=0., number=None):
            this function initializes a new rigid bulk, in 2D
            parameters:
               - self: the rigid bulk itself
               - avrd: average radius, i.e. the area of the rigid avatar belonging the considered bulk is pi*avrd^2
               - gyrd: gyration radius, i.e. the inertia of the rigid avatar belonging the considered bulk is 
                    m*gyrd^2, where m is the mass of the avatar
        """
        # un corps rigide est un element fini dont le support geometrique est un point
        bulk.__init__(self, elem_dim=0, connectivity=[1], physicalEntity='1', number=number)

        self.avrd    = avrd
        self.gyrd    = gyrd
        # frame of the element
        self.axis   = numpy.array([ [1., 0.], [0., 1.] ])

    ## @brief set the frame of the bulk
    def setFrame(self, axis):
        """ bulk.setFrame(axis)
            axis   : axis of the frame (2 2D vectors)
        """
        if axis.shape != (2,2):
            msg = '[setFrame] axis is of wrong size, must be (2,2) and is '+ str(axis.shape)
            showWarning(msg)
            return

        self.axis = axis

    def applyLinearMapToFrame(self, q):
        """applyLinearMapToFrame(self, q):
           this function applies a linear map to the frame of the object.
           parameters:
              - self: the rigid bulk itself
              - q: the linear map expressed as a 3x3 matrix
        """
        axis = numpy.array([ [self.axis[0,0], self.axis[0,1], 0.],
                             [self.axis[1,0], self.axis[1,1], 0.],
                             [0., 0., 1.] ])
        new_axis=numpy.dot(q, axis)
        self.axis =  numpy.array([ [new_axis[0,0], new_axis[0,1]],
                                   [new_axis[1,0], new_axis[1,1]] ])

