class _Materials:
    def __init__(self):
        self._materials = []
    def addMaterial(self, material):
        self._materials.append(material)
    def __len__(self):
        return len(self._materials)

class _TactBehavs:
    def __init__(self):
        self._behavs = []
    def addBehav(self, behav):
        self._behavs.append(behav)
    def __len__(self):
        return len(self._behavs)

class _SeeTables:
    def __init__(self):
        self._tables = []
    def addSeeTable(self, table):
        self._tables.append(table)
    def __len__(self):
        return len(self._tables)

class _Material:
    def __init__(self, name, materialType='RIGID', density=0.0):
        self.name = name
        self.materialType = materialType
        self.density = density

class _TactBehav:
    def __init__(self, name, law, fric=0.5):
        self.name = name
        self.law = law
        self.fric = fric

class _SeeTable:
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)

# API functions mimicking pylmgc90.pre

def materials():
    return _Materials()

def tact_behavs():
    return _TactBehavs()

def see_tables():
    return _SeeTables()

def material(name, materialType='RIGID', density=0.0):
    return _Material(name, materialType, density)

def tact_behav(name, law, fric=0.5):
    return _TactBehav(name, law, fric)

def see_table(**kwargs):
    return _SeeTable(**kwargs)
