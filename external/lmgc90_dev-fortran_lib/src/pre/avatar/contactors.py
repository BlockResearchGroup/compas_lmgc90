from .contactor.contactor import *

## @class contactors(list)
# contactor iterator
class contactors(list):
    """ contactor container

    inherits from container class 'sequence container'
    i.e. keys are automatically attributed
    """

    def addContactor(self,tact):
        """addcontactor(self,tact)

        assign a number to a contactor and add it to the container
        """
        # si l'obejt passe en argument est bien un contacteur
        if isinstance(tact,contactor):
          # on l'ajoute dans le container
          self.append(tact)
        else: 
           warn='Only a contactor can be added to a contactor container\n'
           showWarning(warn)

        return self

