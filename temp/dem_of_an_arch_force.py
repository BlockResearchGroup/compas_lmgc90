import numpy as np
from compas_dem.models import BlockModel
from compas_dem.templates import ArchTemplate
from compas_lmgc90.solver import Solver

# =============================================================================
# Template
# =============================================================================

template: ArchTemplate = ArchTemplate(rise=4.393, span=21.213, thickness=0.5, depth=3.0, n=100)

# =============================================================================
# Model
# =============================================================================

model = BlockModel.from_template(template)

for block in model.elements():
    if block.point.z < 0.3:
        block.is_support = True
# =============================================================================
# Solver
# =============================================================================

dt = 1e-3
nb_steps = 5000

solver = Solver(model, density=2000.0, dt=dt, debug=True)

# Supports/Boundary Conditions
solver.set_supports_from_model()
# Imposed velocity
# solver.apply_velocity(
#    block_index=10,
#    component="Vz",
#    value=np.array([[0.0, 0.49999, 0.5], [0.0, 0.0, 1e-3]]),
# )

Fz = 155e3
t_max = dt * nb_steps

solver.apply_force(
    block_index=70,
    component="Fz",
    value=np.array([[0.0, 0.9 * t_max, t_max], [0, -Fz, -Fz]]),
)

# Contact law
solver.contact_law("IQS_CLB", 0.6)

# Preprocess
solver.preprocess()

# =============================================================================
# Run Simulation
# =============================================================================

solver.run(nb_steps=nb_steps)

# =============================================================================
# Visualize the model in the DEM Native viewer
# =============================================================================

from compas_dem.viewer import DEMViewer  # noqa: E402

viewer = DEMViewer(model)
viewer.add_solution("LMGC90", solver, scale_force=10e-7, scale_normal=0.0000001)  # Passing the scale KWARGS specific to LMGC90
viewer.show()
solver.finalize()
