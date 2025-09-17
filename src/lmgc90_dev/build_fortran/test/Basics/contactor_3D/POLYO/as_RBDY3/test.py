import pathlib
import numpy as np

from pylmgc90 import pre, chipy

datbox = pathlib.Path('./DATBOX')
datbox.mkdir(exist_ok=True)


dim = 3

bodies = pre.avatars()
mat    = pre.materials()
mod    = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()
post   = pre.postpro_commands()

stone = pre.material(name='Stone', materialType='RIGID', density=2000.)
mat.addMaterial(stone)

rigid = pre.model(name='Rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mod.addModel(rigid)

# # =============================================================================
# reading block mesh
blocks_data = [ {'vol' : 3.0000000000000013,
                 'OG'  : np.array([1., 1., 2.]),
                 'I'   : np.array([[ 0.921875   , 0.    , -0.18944306],
                                   [ 0.         , 1.5625,  0.        ],
                                   [-0.18944306 , 0.    ,  1.140625  ]]
                                 ),
                },
                {'vol' : 1.5907471269486084,
                 'OG'  : np.array([1.28583177, 1., 0.64944982]),
                 'I'   : np.array([[ 4.30223839e-01, 4.44089210e-16,-4.05668170e-02],
                                   [ 4.44089210e-16, 3.79861760e-01, 2.22044605e-16],
                                   [-4.05668170e-02, 2.22044605e-16, 3.31417231e-01]]
                                 ),
                },
              ]
# above data were, in fact, provided by FreeCAD
#import FreeCAD as F
#import Part as FP
#blocks = FP.read("Blocs.brep").Solids
#for i_block, bloc in enumerate(blocks):
#  vol = bloc.Volume
#  OG  = bloc.centerOfMass
#  I   = np.array( bloc.MatrixOfInertia.A )
#  I.shape = [4,4]
#  I = I[:3,:3]


nb_blocks = 2
for i_block in range(nb_blocks):
    print("bloc charges",i_block)
    data = blocks_data[i_block]
    block = pre.readMesh(f"gmsh/block_{i_block}.msh", dim=dim)
    body  = pre.surfacicMeshToRigid3D(block, rigid, stone, color='BLOCS', is_open=True,
                                      volume=data['vol'], OG=data['OG'], I=data['I'])
    bodies += body

# # =============================================================================
# reading fondation mesh
fonda = pre.readMesh("gmsh/fonda_0.msh", dim=dim)
body  = pre.surfacicMeshToRigid3D(fonda, model=rigid, material=stone, color='FONDA')
body.imposeDrivenDof(component=[1,2,3,4,5,6],dofty='vlocy')
bodies += body



# gestion des interactions :
glissant = pre.tact_behav(name='gliss', law='IQS_CLB', fric=0.1)
tacts += glissant
contact = pre.tact_behav(name='conta', law='IQS_CLB', fric=0.5)
tacts += contact


#   * declaration des tables de visibilite
svs  += pre.see_table(CorpsCandidat   ='RBDY3',    candidat='POLYR',    colorCandidat='FONDA',
                      CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLOCS',
                      behav=glissant, alert=0.01)
#       - entre blocs
svs  += pre.see_table(CorpsCandidat   ='RBDY3',    candidat='POLYR',    colorCandidat='BLOCS',
                      CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='BLOCS',
                      behav=contact, alert=0.01)

# Write all files to compute solution with LMGC90
pre.writeDatbox(dim, mat, mod, bodies, tacts, svs, post=post)

#pre.visuAvatars(bodies)



###  Computation  ###
from pylmgc90.chipy import computation
                    
chipy.Initialize()
chipy.checkDirectories()

# defining some variables
dim  = 3
mhyp = 1

t_final = 2
dt      = 0.01
theta   = 1.0

freq_write   = 10
freq_display = 1

# interaction parameters
freq_detect = 1
Rloc_tol    = 5.e-2

# nlgs parameters
solver_param = {'tol'    : 1e-5   ,
                'relax'  : 1.     ,
                'norm'   : 'Quad ',
                'gs_it1' : 1000   ,
                'gs_it2' : 1000   ,
                'stype'  : 'Stored_Delassus_Loops         ',
               }

# methode de detection des contacts
chipy.PRPRx_UseStoDetection(False,0.70,0.10)
chipy.PRPRx_ShrinkPolyrFaces(0.01)

#
# read and load
#
chipy.SetDimension(dim,mhyp)
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
#
chipy.ReadDatbox(deformable=False)

chipy.OpenDisplayFiles(write_f2f=3)
chipy.OpenPostproFiles()

chipy.ComputeMass()

mass  = chipy.RBDY3_GetAllMass()
inert = chipy.RBDY3_GetAllInertia()
for i_bdyty in range(nb_blocks):
  IFref = chipy.RBDY3_GetBodyMatrix('IFref', i_bdyty+1)

  assert np.allclose( mass[i_bdyty] , blocks_data[i_bdyty]['vol']*stone.density ), 'wrong mass computation for block {i_bdyty}'
  
  I, frame = np.linalg.eigh( blocks_data[i_bdyty]['I'] )
  assert np.allclose( inert[i_bdyty], I*stone.density ), 'wrong inertia computation for block {i_bdyty}'
  ## nothing ensure that frame is the same than the one provided by the eigen vector computation
  # assert np.allclose( IFref, frame ), 'wrong frame computation for block {i_bdyty}'

while chipy.TimeEvolution_GetTime() < t_final :
  computation.one_step(f_write=freq_write, f_display=freq_display, **solver_param)
 
# really to rough for such check...
assert chipy.inter_handler_3D_getNb(chipy.PRPRx_ID) == 4, 'wrong number of PRPRx interaction'
chipy.Finalize()

