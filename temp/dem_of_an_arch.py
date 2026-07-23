import numpy as np

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

# Material density
# Note: LMGC90 currently uses single material type, so only first density is applied
# Per-block density support is planned for future LMGC90 versions
solver = Solver(model, density=2750.0, debug=True)  # Single density for all blocks (kg/m³)

# Example: Per-block densities (API ready, LMGC90 limitation)
# nb_blocks = len(list(model.elements()))
# densities = [1800.0 + i * (2750.0 - 1800.0) / (nb_blocks - 1) for i in range(nb_blocks)]
# solver = Solver(model, density=densities)  # Only first value used currently

# Supports/Boundary Conditions/Settlements - Impose Boundary condition based on Compas DEM Boolean
solver.set_supports(z_threshold=0.4)  # Set support flags
# solver.imposeDrivenDof(block_index, component=[1,2,3,4,5,6], dofty='vlocy')

# Forces
# solver.apply_force(block_index=0, t0=0 sec, rate=[Fx, Fy Fz, Rx, Ry, Rz] N/s, maximum_time=10 sec) 
# solver.apply_force(block_index=0, t0=0 sec, Global_Component = Fx, rate=5 N/s, maximum_time=10 sec) 

# constante imposed velocity during simulation
#solver.apply_velocity(block_index=10, component = "Vz", value= 1e-2 ) 
# imposed velocity of 0. between 0 and 0.5 second and linear increase between 0.5 and 1. second to
# the imposed value of 1e-2
solver.apply_velocity(block_index=10, component = "Vz", value= np.array( [[0., 0.5, 1.0],[0., 1., 0.]] ) ) 
for cmp in ["Vx", "Vy", "Rx", "Ry", "Rz"]:
    solver.apply_velocity(block_index=10, component = cmp, value= 0.) 

# Contacts - 
# solver.contact_law("name_of_contact_law", coeff)
solver.contact_law("IQS_CLB_g0", 0.35, 7e-2)

# meaning of parameters here : https://lmgc90.pages-git-xen.lmgc.univ-montp2.fr/lmgc90_dev/pre_interaction.html
#             dyfr, stfr,   cn,   ct,  S1,  S2,  G1,   G2, Eta (cut of the exponent value)
czm_params = [0.73, 0.73, 1e10, 1e11, 5e4, 2e5, 40., 100., 1e-4]
#solver.contact_law('IQS_EXPO_CZM',czm_params)


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

