
from .avatar.avatar import avatar

from .utilities.error import *

## @class avatars : avatar list
class avatars(list):
    """ avatar container class

    Methods:

    - addAvatar
    - getFemAvatar
    - getRigidAvatar
    """
    
#    ## add a list of avatars to container
#    def addAvatar(self,*Avatar):
#        """addAvatar(self,avatar)
#           add the input list of avatar to the container
#        """
#        try:
#          for avatar in Avatar:
#            self.__dict__[avatar.number] = avatar
#        except: # si l objet Part n est pas iterable il doit
#            if len(Avatar) == 1:
#                self.__dict__[Avatar[0].number] = Avatar[0]
#            else:
#                msg='addAvatar method does not apply'
#                showWarning(msg)

    ## add an avatar to container
    def addAvatar(self, av):
        '''addAvatar(self,av):

        this function adds the input avatar to the container

        parameters:

        - self: the container itself
        - av: a given avatar
        '''

        assert isinstance(av,avatar), "%r is not an avatar"%av

        # on atribue a l'avatar le prochain numero disponible
        # N.B.: si l'avatar provient d'un autre container, son
        #       son numero est ecrase, et renvoie au dernier 
        #       container ou il a ete ajoute
        av.number = len(self)
        # on ajoute l'avatar dans le container
        self.append(av)
   
    ## add one avatar, or a list of avatars, to the container
    def __iadd__(self,object):
        '''__iadd__(self,object):

        overloads operator +=

        parameters:

        - self: the container itself
        - object: two cases:

          - if object is an avatar, it is added to the container (self)
          - if object is a container of avatars, each avatar of this
            container is added to the container (self)'''

        # en fonction du type de l'objet
        if isinstance(object,avatar): # cas d'un avatar
            # on l'ajoute dans le conteneur
            self.addAvatar(object)
        elif isinstance(object,avatars): # cas d'un conteneur d'avatars
            # on ajoute les avatars contenus dans ll'objet, un par un
            for body in object:
               self.addAvatar(body)
        # sinon, on affiche un warning 
        else:
            warn='Only one avatar object, or a list of avatars, can be added to an avatar container\n'
            showWarning(warn)
            sys.exit(-1)

        # on renvoie le conteneur modifie
        return self

    def getFemAvatar(self):
        """ getFemAvatar(self)

        get the list of avatar of MAILx type
        """
        return [ a for a in self if a.atype == 'MAILx' ]

    ## 
    def getRigidAvatar(self):
        """getRigidAvatar(self)
           
        get the list of avatar of RBDY2 or RBDY3 type
        """
        return [ a for a in self if a.atype != 'MAILx' ]

    ## translate a set of avatars
    #  @param dx,dy, dz tranlation vector
    def translate(self, dx=0., dy=0., dz=0.):
        """ usage self.tranlsate(dx=0., dy=0., dz=0.)

        where dx, dy, dz are components of translation vector
        """
        # on translate chaque avatar de l'objet
        for avatar in self:
             avatar.translate(dx=dx,dy=dy,dz=dz)

    ## translate a set of avatars
    def rotate(self, description='Euler', phi=0., theta=0., psi=0., alpha=0., axis=[0., 0., 1.], center=[0., 0., 0.]):
        """rotate(self, description='Euler', phi=0., theta=0., psi=0., alpha=0., axis=[0., 0., 1.], center=[0., 0., 0.])

        this function rotates every avatar in the set, according to the given rotation parameters and a
        rotation center. Supported rotation paramters are: Euler's angles or an axis and an angle

        parameters:

        - self: the avatar itself
        - description='Euler': defines the rotation parameters:

          - if description = 'Euler', the rotation uses Euler's angles, consequently only phi, theta, psi and
            center are considered
          - if description = 'axis', the rotation uses an axis and an angle, consequently only axis, alpha and
            center are considered

        - phi: first Euler's angle (rotation with respect to z-axis)
        - theta: second Euler's angle (rotation with respect to x-axis)
        - psi: third Euler's angle (rotation with respect to z-axis, the only one admissible in 2D)
        - axis: a 3D vector defining rotation axis (colinear to z-axis in 2D)
        - angle: rotation angle
        - center: rotation center
        """

        # on applique la rotation a chaque avatar du conteneur
        for avatar in self:
            avatar.rotate(description=description, phi=phi, theta=theta, psi=psi, alpha=alpha, axis=axis, center=center)


    def updateReferenceConfig(self):
        """Update reference coordinates of all avatars
        """
        for avatar in self:
            avatar.updateReferenceConfig()


    def renumber(self):
        """
        Renumber in a contiguous manner all avatars
        """

        counting = { 'RBDY2' : 0,
                     'RBDY3' : 0,
                     'MAILx' : 0,
                   }
        m_counting = { ('RBDY2','MECAx',) : 0,
                       ('RBDY3','MECAx',) : 0,
                       ('MAILx','MECAx',) : 0,
                       ('MAILx','THERx',) : 0,
                       ('MAILx','POROx',) : 0,
                       ('MAILx','MULTI',) : 0,
                     }

        for av in self:
            counting[ av.atype ] += 1
            m_counting[ (av.atype,av.modelType,) ] += 1
            av.number  = counting[ av.atype ]
            av.m_num   = m_counting[ (av.atype,av.modelType) ]

