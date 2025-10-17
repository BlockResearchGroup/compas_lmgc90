from compas_dem.templates import BarrelVaultTemplate
from compas_dem.elements import Block
from compas_dem.models import BlockModel
from compas.colors import Color
import numpy as np
from compas_viewer import Viewer

from compas_lmgc90 import _lmgc90


import os, sys

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# import des modules
import math, numpy
from pylmgc90 import pre

# definition des conteneurs:
#   * de corps
bodies = pre.avatars()
#   * de materiaux
mats = pre.materials()
mods = pre.models()
#   * de lois de contacts
tacts = pre.tact_behavs()
#   * de tables de visibilite
svs = pre.see_tables()

# exemple 3D
dim = 3

# definition d'un modele rigide
mR3D = pre.model(name='rigid', physics='MECAx', 
                 element='Rxx3D', dimension=dim)
mods.addModel(mR3D)

# definition du materiau pour les blocs
stone = pre.material(name='STONE', materialType='RIGID', 
                     density=2750.)
# ajout des materiaux dans le conteneur
mats.addMaterial(stone)



# =============================================================================
# Block Geometry
# =============================================================================

arch = BarrelVaultTemplate(span=10,length=4,thickness=0.2,vou_span=20,vou_length=4,rise=3)
#arch = BarrelVaultTemplate()

# =============================================================================
# Block Model
# =============================================================================

model = BlockModel()
for block in arch.blocks():
    block = Block.from_mesh(block)
    model.add_element(block)

for element in model.elements():
  
    v=[]
    f=[]
    
    v=[element.geometry.vertex_coordinates(vkey) for vkey in element.geometry.vertices()]
    cnt=[element.geometry.face_vertices(fkey) for fkey in element.geometry.faces()]
    #t3 face
    for p in cnt :    
      f.append(p[:-1])
      f.append([p[0],p[2],p[3]])
    print(f)
    
    npv=numpy.zeros([len(v), 3],dtype=np.float64)
    for i,c in enumerate(v):
      npv[i,:] = c
    print(npv)
    
    npf=numpy.zeros([len(f), 3],dtype=np.int32)
    for i,t3 in enumerate(f):
      npf[i,:] = t3
      npf[i,:]+=1
    print(npf)    
    
    # creation d'un nouvel avatar rigide pour le bloc

    if element.point.z < 0.31 :
      b = pre.rigidPolyhedron(model=mR3D, material=stone, generation_type='full',\
                              vertices=npv, faces=npf, color='GRND_')
      b.imposeDrivenDof(component=[1,2,3,4,5,6], dofty='vlocy')
    else:
      b = pre.rigidPolyhedron(model=mR3D, material=stone, generation_type='full',\
                              vertices=npv, faces=npf, color='REDxx')

    # ajout du bloc a l'ensemble des corps
    bodies.addAvatar(b)

# definition d'une loi de contact frottant, avec pre-gap
iqs=pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.5)
# ajout de la loi dans le conteneur de lois
tacts.addBehav(iqs)

# definition d'une table de visibilite pour le
# contact polyedre-polyedre (i.e. entre blocs)
sv1 = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='REDxx',\
                   CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx',\
                   behav=iqs, alert=1e-3)
# ajout de la table de visibilite dans le conteneur
# de tables de visibilite
svs.addSeeTable(sv1)

sv2 = pre.see_table(CorpsCandidat='RBDY3', candidat='POLYR', colorCandidat='REDxx',\
                   CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='GRND_',\
                   behav=iqs, alert=1e-3)
# ajout de la table de visibilite dans le conteneur
# de tables de visibilite
svs.addSeeTable(sv2)

post = pre.postpro_commands()
nlgs = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(nlgs)

# ecriture des fichiers de donnees pour LMGC90
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass

