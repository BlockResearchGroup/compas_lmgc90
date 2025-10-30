
import pathlib 

import numpy as np

from compas_dem.templates import BarrelVaultTemplate
from compas_dem.elements import Block
from compas_dem.models import BlockModel
from compas.colors import Color
from compas_viewer import Viewer

from pylmgc90 import pre, chipy

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

############################
##### Computation part #####
############################

chipy.Initialize()
chipy.checkDirectories()

dim = 3
mhyp = 0

dt = 1e-3
theta = 0.5
nb_steps = 10

Rloc_tol = 5.e-2

# nlgs parameters
tol = 1.666e-4
relax = 0.1
norm = 'QM/16'
gs_it1 = 50
gs_it2 = 500
solver_type='Stored_Delassus_Loops         '

freq_write   = 1
freq_display = 1

chipy.nlgs_3D_DiagonalResolution()

#chipy.PRPRx_SetCundallNeighbor(1e-1)
chipy.PRPRx_UseCpCundallDetection(200)
chipy.PRPRx_LowSizeArrayPolyr(10)
#chipy.PRPRx_VerboseF2F(9,10)

# Set space dimension
chipy.SetDimension(dim,mhyp)
#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
#
##chipy.ReadDatbox(deformable=False)
chipy.ReadBehaviours(mats=mats, tacts=tacts, sees=sees)
#chipy.ReadBodies(bodies)
#chipy.ReadDrivenDof()
chipy.ReadBodiesFromContainer(bodies)
chipy.LoadBehaviours()
#chipy.ReadIniDof()
chipy.LoadTactors()
#chipy.ReadIniVlocRloc()
chipy.inter_handler_3D_redoNbAdj(chipy.PRPRx_ID)
chipy.StockRloc()

chipy.WriteBodies()
chipy.WriteDrivenDof()
chipy.WriteBehaviours()

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

#
# simulation part ...
#

# ... calls a simulation time loop
# since constant compute elementary mass once
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

for k in range(0,nb_steps):
  #
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()

  chipy.utilities_logMes('COMPUTE Fext')
  chipy.ComputeFext()
  chipy.utilities_logMes('COMPUTE Fint')
  chipy.ComputeBulk()
  chipy.utilities_logMes('COMPUTE Free Vlocy')
  chipy.ComputeFreeVelocity()

  chipy.utilities_logMes('SELECT PROX TACTORS')
  chipy.SelectProxTactors()

  chipy.utilities_logMes('RESOLUTION' )
  chipy.RecupRloc(Rloc_tol)

  chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
  chipy.UpdateTactBehav()

  chipy.StockRloc()

  chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
  chipy.ComputeDof()

  chipy.utilities_logMes('UPDATE DOF, FIELDS')
  chipy.UpdateStep()

  chipy.utilities_logMes('WRITE OUT')
  chipy.WriteOut(freq_write)

  chipy.utilities_logMes('VISU & POSTPRO')
  chipy.WriteDisplayFiles(freq_display)
  chipy.WritePostproFiles()

#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()
