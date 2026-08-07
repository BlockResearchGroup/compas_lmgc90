import numpy as np

from compas.colors import Color
from compas_dem.models import BlockModel
from compas_dem.templates import ArchTemplate
from compas_lmgc90.solver import Solver
from compas_viewer import Viewer

# =============================================================================
# Template
# =============================================================================

template = ArchTemplate(rise=3, span=10, thickness=0.5, depth=0.5, n=20)

meshes = template.blocks()

# =============================================================================
# Solver
# =============================================================================

solver = Solver(density=2400.0, dt=0.001)
solver.geometry_from_mesh(meshes)

# Supports/Boundary Conditions
solver.set_supports(z_threshold=0.4)

# Imposed velocity

# solver.apply_velocity(
#    block_index=10,
#    component="Vz",
#    value=np.array([[0.0, 0.49999, 0.5], [0.0, 0.0, 1e-3]]),
# )

# Applied Force

solver.apply_force(
    block_index=10,
    component="Fz",
    value=np.array([[0.0, 0.49999, 0.5], [1e3, 1e3, 0e0]]),
)

# Contact law
solver.contact_law("IQS_CLB", 0.35)

# Preprocess
solver.preprocess()

# =============================================================================
# Interactive Viewer with iterative simulation
# =============================================================================

# Use base Viewer instead of DEMViewer to add solver.trimeshes directly
# This maintains the same mesh references that the solver transforms in-place
viewer = Viewer()

# Define colors for supports and blocks
support_color = Color.red().lightened(50)
block_color = Color.grey().lightened(85)

# Add solver.trimeshes directly to the scene (maintains reference!)
scene_objects = []
for i, mesh in enumerate(solver.trimeshes):
    is_support = solver.supports[i]
    color = support_color if is_support else block_color
    obj = viewer.scene.add(mesh, facecolor=color)
    scene_objects.append(obj)


@viewer.on(interval=100)
def step(frame):
    """Run one simulation step and update the viewer."""
    solver.run(nb_steps=1)

    # Update scene objects - they reference solver.trimeshes which are already
    # transformed in-place by the solver, so we just need to refresh the GPU data
    for obj in scene_objects:
        obj.update(update_data=True)


viewer.show()

# Finalize solver when done
solver.finalize()
