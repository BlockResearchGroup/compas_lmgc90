from compas_dem.models import BlockModel
from compas_dem.templates import ArchTemplate
from compas_dem.viewer import DEMViewer
from compas_lmgc90.solver import Solver 


# =============================================================================
# Template
# =============================================================================

template = ArchTemplate(rise=3, span=10, thickness=0.5, depth=0.5, n=20)

# =============================================================================
# Scale meshes for quick test
# =============================================================================

meshes = template.blocks()

for block in meshes:
    centroid = block.centroid()
    block.translate([-centroid[0], -centroid[1], -centroid[2]])
    block.scale(90.0/100.0)
    block.translate(centroid)

# =============================================================================
# Model
# =============================================================================

model = BlockModel.from_boxes(meshes)

# =============================================================================
# Solver
# =============================================================================

# Solver - set parameteres of solver
# solver.Solver(model, tolearance=1e-6, relaxation=0.5) # Default solver  
solver = Solver(model)  # Process model once

# Supports/Boundary Conditions/Settlements - Impose Boundary condition based on Compas DEM Boolean
solver.set_supports(z_threshold=0.4)  # Set support flags
# solver.imposeDrivenDof(block_index, component=[1,2,3,4,5,6], dofty='vlocy')       

# Material
# solver.set_material(density=...)

# Forces
# solver.apply_force(block_index=0, t0=0 sec, rate=[Fx, Fy Fz, Rx, Ry, Rz] N/s, maximum_time=10 sec) 
# solver.apply_force(block_index=0, t0=0 sec, Global_Component = Fx, rate=5 N/s, maximum_time=10 sec) 
# solver.apply_displacement(block_index=0, t0=0 sec, Global_Component = Ux, rate=5 mm/s, maximum_time=10 sec) 

# Contacts - 
# solver.contact_law("name_of_contact_law", coeff)

# Physical Boundaries
# solver.rigid_plane(origin, normal)

solver.preprocess()  # Setup LMGC90
solver.run(nb_steps=100)  # Run simulation
solver.finalize()

# =============================================================================
# Viz - Create model from transformed blocks
# TODO: Vizualize Polygons
# TODO: Vizualize Polygons vertex forces as lines
# TODO: Vizualize Thrust lines
# =============================================================================

viewer = DEMViewer(BlockModel.from_boxes(solver.trimeshes))
# solver.postprocess() # To get the thrust lines and contact polygons

for i, element in enumerate(viewer.model.elements()):
    element.is_support = solver.supports[i]
viewer.setup()
viewer.show()
