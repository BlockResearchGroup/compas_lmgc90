import pathlib

from compas_dem.elements import Block
from compas_dem.models import BlockModel

import compas
from compas.datastructures import Mesh
from compas.files import OBJ
from compas_lmgc90.solver import Solver

# =============================================================================
# Load Data
# =============================================================================
FILE = pathlib.Path(__file__).parent.parent / "data" / "carbcomn_vault.json"
json_blocks = compas.json_load(FILE)
model = BlockModel()

for mesh in json_blocks:
    mesh.scale(0.001)
    model.add_block_from_mesh(mesh)


for bidx, block in enumerate(model.elements()):
    centroid = block.modelgeometry.centroid()
    if centroid[0] < 0.250:
        block.is_support = True

    elif centroid[0] > 6.000:
        block.is_support = True

# =============================================================================
# Model and interactions
# =============================================================================

# model.compute_contacts(tolerance=0.001)

# =============================================================================
# Solver
# =============================================================================

solver = Solver(model,debug=True)            # Process model once
solver.set_supports_from_model()             # Use supports already set in model
solver.contact_law("IQS_CLB_g0", 0.35, 1e-3)
solver.preprocess()                          # Setup LMGC90
solver.run(nb_steps=20)                      # Run simulation
solver.finalize()

# =============================================================================
# Viz - Create model from transformed blocks
# =============================================================================

# =============================================================================
# Viz
# =============================================================================

viz='lmgc90'

match viz:
  case 'viewer':   
      from compas_dem.viewer import DEMViewer

      viewer = DEMViewer(BlockModel.from_boxes(solver.trimeshes))

      for i, element in enumerate(viewer.model.elements()):
          element.is_support = solver.supports[i]
      viewer.setup()
      viewer.show()
  case 'lmgc90':
      import outbox2display
      outbox2display.run()
  case 'vtk':
      from pyvista_vtk_viewer import vtk_viewer
      vtk_viewer(solver, output_dir=pathlib.Path(__file__).parent, folder="carbcomn_vault")
