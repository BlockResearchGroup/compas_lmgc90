from .shared.postpro_command import *

class postpro_commands(list):
    """class postpro_commands: postprocessing commands container
       methods:
          - addCommand: add a command to the container
    """
    def addCommand(self, command):
        """addCommand(self, command):
           this function adds a command to the container
           parameter:
              - command: postprocessing command to be added
        """
        # si l'objet passe en parametre est bien une commande preprocessing
        if isinstance(command, postpro_command):
            # on l'ajoute au conteneur
            self.append(command)

    def __iadd__(self, command):
        self.addCommand(command)
        return self

