import numpy as np

from compas.geometry import Frame, Vector
from compas.datastructures import Mesh
from compas_viewer import Viewer
from compas.geometry import Transformation

from pylmgc90 import pre, chipy

#
## Compas Viewer
def cview(blocks):
  #Compas view with ugly global variables
  viewer = Viewer()
  for i, b in enumerate(blocks):
    viewer.scene.add(b, name=f"Body {i}")
  viewer.show()


def update(blocks, init_coor, init_frame):
  trans = np.zeros( [4,4] )
  trans[3,3] = 1.0
  for i, b in enumerate(blocks):
    new_coor  = chipy.RBDY3_GetBodyVector('Coorb', i+1)[:3]
    new_frame = chipy.RBDY3_GetBodyMatrix('IFbeg', i+1)
  
    df = np.matmul( new_frame.T, init_frame[i] )
    dc = new_coor - np.matmul( df, init_coor[i] )
  
    trans[:3,:3] = df[:,:]
    trans[:3,3]  = dc[:]
  
    b.transform(trans)

    # do not forget to update
    init_frame[i][:,:] = new_frame[:,:]
    init_coor[i][:]    = new_coor[:]

# Hardcoded geometry from C example
faces = np.array([[0, 1, 2,],
                  [0, 1, 3,],
                  [1, 2, 3,],
                  [2, 0, 3,],
                  ] )

vertices0 = np.array( [[0.0, 0.0, 0.0,],
                       [1.0, 0.0, 0.0,],
                       [0.0, 0.7, 0.3,],
                       [0.3,-0.1, 0.9,],
                      ] )

vertices1 = np.array( [[0.0, 0.0, 0.0,],
                       [1.0, 0.0, 0.0,],
                       [0.0, 1.0, 0.0,],
                       [0.3, 0.3,-1.0,],
                      ] )

pos0 = np.array( [0.0, 0.0, 1.0] )
pos1 = np.array( [0.0, 0.0, 0.8] )

# Create meshes
mesh0 = Mesh.from_vertices_and_faces(vertices0, faces)
mesh1 = Mesh.from_vertices_and_faces(vertices1, faces)
blocks = [mesh0, mesh1]

nb_steps = 50

mats   = pre.materials()
tacts  = pre.tact_behavs()
sees   = pre.see_tables()

gravity = np.array( [0., 0., -9.81] )
stone = pre.material(name='STONE', materialType='RIGID', density=2750.)
mats.addMaterial(stone)
iqs=pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.5)
tacts.addBehav(iqs)
sv1 = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='POLYR', colorCandidat   ='REDxx',\
                    CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='GRND_',\
                    behav=iqs, alert=1e-3)
sees.addSeeTable(sv1)

chipy.Initialize()
chipy.checkDirectories()
chipy.nlgs_3D_DiagonalResolution()
chipy.PRPRx_UseCpCundallDetection(200)
chipy.PRPRx_LowSizeArrayPolyr(10)
chipy.SetDimension(3,0)
chipy.TimeEvolution_SetTimeStep(1e-2)
chipy.Integrator_InitTheta(0.5)
#
chipy.ReadBehaviours(mats=mats, tacts=tacts, sees=sees, gravy=gravity)

# what is done in the C function, but with more detail
# Set the number of rigid bodies
chipy.RBDY3_setNb(2)
# add a body with:    inertia center, number of contactor, number of velocity driven dof, number of force driven dof
chipy.RBDY3_addOne(pos0, 1, 0, 0)
chipy.RBDY3_addOne(pos1, 1, 6, 0)
# them for the second body, setting null velocity on all dofs
drv_values = np.zeros( [6] )
for idof in range(1,7):
  chipy.RBDY3_addDrvDof(2, True, idof, drv_values)

idata = np.empty( [2+12], dtype='int32' )
idata[0]  = 4
idata[1]  = 4
idata[2:] = np.ravel(faces) + 1
# add polyhedron contactor on each bodies, without providing inertia and vertices in global frame
# body id, contactor id, type of contactor, color,
# volume, inertia, inertia frame, shift (with respect to inertia center coordinates provided in addOne)
# a flat integer array with [nb_vertices, nb_triangle, flatten triangle connectivity] ), a flat coordinates array
chipy.RBDY3_setOneTactor(1, 1, 'POLYR', 'REDxx',
                         0., np.zeros([3]), np.eye(3), np.zeros([3]),
                         idata, np.ravel(vertices0)
                        )
chipy.RBDY3_setOneTactor(2, 1, 'POLYR', 'GRND_',
                         0., np.zeros([3]), np.eye(3), np.zeros([3]),
                         idata, np.ravel(vertices1)
                        )

chipy.RBDY3_setBulk(1, 'STONE', 0., np.zeros([3]), np.eye(3))
chipy.RBDY3_setBulk(2, 'STONE', 0., np.zeros([3]), np.eye(3))

chipy.RBDY3_synchronize()
# end of body loading...

chipy.LoadBehaviours()
chipy.LoadTactors()
chipy.inter_handler_3D_redoNbAdj(chipy.PRPRx_ID)
chipy.StockRloc()


init_coor  = []
init_frame = []
for i in range(1,3):
  init_coor.append(  chipy.RBDY3_GetBodyVector('Coorb', i)[:3] )
  init_frame.append( chipy.RBDY3_GetBodyMatrix('IFbeg', i) )


# view of input blocks from coordinates
#cview(blocks)

# view of input blocks from coordinates and initial position
blocks[0].translate(pos0)
blocks[1].translate(pos1)
#cview(blocks)


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

for k in range(1,nb_steps+1):
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

  if k% (nb_steps//2) == 0:
    update(blocks, init_coor, init_frame)
#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# view of final configuration
cview(blocks)

# this is the end
chipy.Finalize()
