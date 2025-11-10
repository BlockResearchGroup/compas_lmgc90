from compas_dem.models import BlockModel
from compas_dem.templates import BarrelVaultTemplate
from compas_dem.viewer import DEMViewer
from compas_lmgc90.solver import Solver 

# =============================================================================
# Template
# =============================================================================

template = BarrelVaultTemplate()

# =============================================================================
# Model and interactions
# =============================================================================

model = BlockModel.from_barrelvault(template)

model.compute_contacts(tolerance=0.001)

# =============================================================================
# Scale meshes for quick test
# =============================================================================

for block in model.blocks():
    mesh = block.modelgeometry
    centroid = mesh.centroid()
    mesh.translate([-centroid[0], -centroid[1], -centroid[2]])
    mesh.scale(99.0/100.0)
    mesh.translate(centroid)

# =============================================================================
# Solver
# =============================================================================

solver = Solver(model)  # Process model once
solver.set_supports_from_model()  # Use supports already set in model
solver.preprocess()  # Setup LMGC90
solver.run(nb_steps=100)  # Run simulation
solver.finalize()

# =============================================================================
# Viz
# =============================================================================

viewer = DEMViewer(BlockModel.from_boxes(solver.trimeshes))

for i, element in enumerate(viewer.model.elements()):
    element.is_support = solver.supports[i]
viewer.setup()
viewer.show()