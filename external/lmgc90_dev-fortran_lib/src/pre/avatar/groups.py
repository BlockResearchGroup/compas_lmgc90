
class groups(dict):
    """ class 'groups' inheriting from 'mapping_container'
       allows to store groups
    """
    def addGroup(self,group):
        """addGroup(self,group)
           Allows to add the group by its attribute '.name'           
        """
        self[group.name] = group
            
    def modifieNoms(self,dico):
        """ modifiesNoms(self,dico)
            change name of the group thanks to 'dico' which has the old names as keys and new names as values
        """
        for k in list(dico.keys()):
            self.modifieNom(k,dico[k])
            
    def modifieNom(self,old,new):
        """modifieNom(self,old,new)
           Allows to change the name of a group of name and elements
            groups[nom].nodes et groups[nom].elements
        """
        try:
            gr=self[old]
            gr.nom = new
            delattr(self, old)
            self.addGroup(gr)
        except:
            print('\nThe group %s is not defined\n' % old)

    def __iter__(self):
        return iter(self.values())
