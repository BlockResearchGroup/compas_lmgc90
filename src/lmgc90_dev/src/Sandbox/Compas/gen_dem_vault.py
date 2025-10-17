
import pathlib 

import numpy as np

from compas_dem.templates import BarrelVaultTemplate
from compas_dem.elements import Block
from compas_dem.models import BlockModel
from compas.colors import Color
from compas_viewer import Viewer

#from compas_lmgc90 import _lmgc90



datbox = pathlib.Path('./DATBOX')
datbox.mkdir(exist_ok=True)

from pylmgc90 import pre

bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
tacts  = pre.tact_behavs()
sees   = pre.see_tables()

# space dim
dim = 3

mR3D = pre.model(name='rigid', physics='MECAx', 
                 element='Rxx3D', dimension=dim)
mods.addModel(mR3D)

stone = pre.material(name='STONE', materialType='RIGID', density=2750.)
mats.addMaterial(stone)



# =============================================================================
# Block Geometry
# =============================================================================

arch = BarrelVaultTemplate(span=10,length=4,thickness=0.2,vou_span=20,vou_length=4,rise=3)

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
    
    npv=np.zeros([len(v), 3],dtype=np.float64)
    for i,c in enumerate(v):
      npv[i,:] = c
    
    npf=np.zeros([len(f), 3],dtype=np.int32)
    for i,t3 in enumerate(f):
      npf[i,:] = t3
      npf[i,:]+=1
    
    # new rigid avatar creation for the block
    if element.point.z < 0.31 :
      b = pre.rigidPolyhedron(model=mR3D, material=stone, generation_type='full',\
                              vertices=npv, faces=npf, color='GRND_')
      b.imposeDrivenDof(component=[1,2,3,4,5,6], dofty='vlocy')
    else:
      b = pre.rigidPolyhedron(model=mR3D, material=stone, generation_type='full',\
                              vertices=npv, faces=npf, color='REDxx')

    bodies.addAvatar(b)


# some contact law with friction and pre-gap
iqs=pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.5)
tacts.addBehav(iqs)

# visibility table definition between blocks
sv1 = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='POLYR', colorCandidat='REDxx'   ,\
                    CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='REDxx',\
                    behav=iqs, alert=1e-3)
sees.addSeeTable(sv1)

# visibility table definition between block and ground
sv2 = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='POLYR', colorCandidat   ='REDxx',\
                    CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='GRND_',\
                    behav=iqs, alert=1e-3)
sees.addSeeTable(sv2)

# some post-processing commands
post = pre.postpro_commands()
nlgs = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(nlgs)

# ecriture des fichiers de donnees pour LMGC90
pre.writeDatbox(dim, mats, mods, bodies, tacts, sees, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass

