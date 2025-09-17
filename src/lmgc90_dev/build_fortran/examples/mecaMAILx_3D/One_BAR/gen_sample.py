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
Model_Meca = pre.model(name='babar',                           # Key to find the model 
                       physics='MECAx',                          # Type of the model 
                       element='BARxx',                       # Possible elements S2xth,T3xxx,H8xxx,H20Rx,TE4xx
                       dimension = dimension,
                       kinematic = 'large',
                       formulation='UpdtL',
                       material='neoh_',
                       anisotropy='iso__',
                       external_model='no___', 
                       mass_storage='lump_',
                       external_fields =['SECTION'])
# And save this model in the pack of physical model
mods.addModel(Model_Meca)
# ------------------------------------------------------
# Create a material
Mat_1      = pre.material(name='Steel', 
                          materialType='ELAS', 
                          density=7800.0,
                          elas='standard', 
                          young=210e9, 
                          nu=0.0,
                          anisotropy='isotropic')
# And save this material in the pack of materials behaviours
mats.addMaterial(Mat_1)
# ------------------------------------------------------
# Import gmsh mesh for LMGC90
Mailx = pre.readMesh('MESH/Barre1D.msh', dim=dimension, keep_elements=[1])
# Create a geometrical bodies with finite element (RBDY2, RBDY3)
Barre3D = pre.avatar(dimension = dimension)
# Import element connectivity
Barre3D.addBulks(Mailx.bulks)
# Import node position
Barre3D.addNodes(Mailx.nodes)
# Import physical groups
Barre3D.defineGroups()
# ------------------------------------------------------
# Apply physical model on bodies
Barre3D.defineModel(model=Model_Meca, group = '2')
# Apply material behaviour on physical group 2
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
Barre3D.imposeDrivenDof(group='N1', component = [1,2,3], dofty='vlocy')

post = pre.postpro_commands()

# Write all files to compute solution with LMGC90
pre.writeDatbox(dimension, mats, mods, bodies, post=post)

try:
  pre.visuAvatars(bodies)
except:
  pass
