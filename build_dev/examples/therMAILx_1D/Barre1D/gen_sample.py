import os

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

# Import python fonction to create LMGC90 model
from pylmgc90 import pre

# ------------------------------------------------------
# To start : Create all pack  of components
# Create the pack of bodies
bodies = pre.avatars()
# Create the pack of physical model
mods = pre.models()
# Create the pack of materials behaviours
mats = pre.materials()
# Create the pack of contact visibility
svs = pre.see_tables()
# Create the pack of contact behaviours
tacts = pre.tact_behavs()
# ------------------------------------------------------
# Defined the model dimension of the physical problem
dimension = 3
# ------------------------------------------------------
# Create a thermal model 
Model_Thermique = pre.model(name='DIFFU',                           # Key to find the model 
                            physics='THERx',                          # Type of the model 
                            element='S2xth',                       # Possible elements S2xth,T3xxx,H8xxx,H20Rx,TE4xx
                            dimension = dimension,
                            external_model='no___', 
                            capacity_storage='lump_',
                            formulation = 'class',
                            external_fields =['SECTION'])
# And save this model in the pack of physical model
mods.addModel(Model_Thermique)
# ------------------------------------------------------
# Create a material
Mat_1           = pre.material(name='Steel', 
                               materialType='THERMO_ELAS', 
                               density=1.0,
                               elas='standard', 
                               young=0.0, 
                               nu=0.0,
                               T_ref_meca = 0.0, 
                               anisotropy='isotropic',
                               dilatation = 0.0,
                               conductivity=0.002,
                               specific_capacity=0.000005)
# And save this material in the pack of materials behaviours
mats.addMaterial(Mat_1)
# ------------------------------------------------------
# Create a geometrical bodies with finite element (RBDY2, RBDY3)
Barre3D = pre.avatar(dimension = dimension)
# Import gmsh mesh for LMGC90
Mailx = pre.readMesh('MESH/Barre1D.msh', dim=dimension, keep_elements=[1])
# Import element connectivity
Barre3D.addBulks(Mailx.bulks)
# Import node position
Barre3D.addNodes(Mailx.nodes)
# Import physical groups
Barre3D.defineGroups()
# ------------------------------------------------------
# Apply physical model on bodies
Barre3D.defineModel(model=Model_Thermique, group = '2')
# Apply material behaviour on physical groups 31
Barre3D.defineMaterial(material=Mat_1, group = '2')
# And save this bodies in the pack of bodies
bodies += Barre3D
# ------------------------------------------------------
# Apply the boundary conditions on physical groups
# predicat
def p1(x):
   return (abs(x[0]) < 5.e-4) and (abs(x[1]) < 5.e-4) and (abs(x[2]) < 5.e-4)
# creation d'un nouveau groupe
Barre3D.addGroupUsingPredicate(name='N1', predicate=p1)
Barre3D.imposeDrivenDof(group='N1', component = 1, dofty='temp', ct = 0.0, rampi = 1.0)

# predicat
def p1(x):
   return (abs(x[0] - 100.0) < 5.e-4) and (abs(x[1] ) < 5.e-4) and (abs(x[2]) < 5.e-4)
# creation d'un nouveau groupe
Barre3D.addGroupUsingPredicate(name='N3', predicate=p1)
Barre3D.imposeDrivenDof(group='N3', component = 1, dofty='temp', ct = 100.0, rampi = 1.0)
# ------------------------------------------------------
# Apply the initial conditions on physical groups
# Imposed initial temperature on the bodies at 0K
Barre3D.imposeInitValue(group='2', component = 1, value = 0.0)
# ------------------------------------------------------

# Write all files to compute solution with LMGC90
pre.writeDatbox(dimension, mats, mods, bodies, gravy=[0., 0., 0.])

try:
  pre.visuAvatars(bodies)
except:
  pass
