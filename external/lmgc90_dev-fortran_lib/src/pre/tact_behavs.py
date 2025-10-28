from .shared.tact_behav import *

class see_tables(list):
    """classe see_tables derive de 'sequence_container'
       methodes definies:
       -addSeeTable
    """
    def addSeeTable(self,st):
        """addSeeTable(self,st)
           ou st est une table de visu
        """
        if isinstance(st,see_table):
            self.append(st)

    def __iadd__(self,st):
        self.addSeeTable(st)
        return self

class tact_behavs(dict):
    """classe behavs : conteneur de comportement
       methodes associees:
       -addBehav
    """
    def addBehav(self,bebe):
        """addBehav(self,bebe)
           ajoute un comportement au conteneur
        """
        if isinstance(bebe,tact_behav):
            self[bebe.nom] = bebe
    def __add__(self,bebe):
        self.addBehav(bebe)
        return self
    
