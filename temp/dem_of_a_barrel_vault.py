import pathlib
from compas_dem.models import BlockModel
from compas_dem.templates import BarrelVaultTemplate
from compas_lmgc90.solver import Solver

# =============================================================================
# Template
# =============================================================================

template = BarrelVaultTemplate()

# =============================================================================
# Model and interactions
# =============================================================================

model = BlockModel.from_barrelvault(template)

# =============================================================================
# Solver
# =============================================================================

solver = Solver(debug=True)  # Process model once
solver.geometry_from_model(model)
solver.set_supports_from_model()  # Use supports already set in model
solver.contact_law("IQS_CLB_g0", 0.7, 1e-2)
solver.preprocess()  # Setup LMGC90
solver.run(nb_steps=100)  # Run simulation
solver.finalize()

# =============================================================================
# Viz
# =============================================================================

viz = "lmgc90"

match viz:
    case "viewer":
        from compas_dem.viewer import DEMViewer

        viewer = DEMViewer(BlockModel.from_boxes(solver.trimeshes))

        for i, element in enumerate(viewer.model.elements()):
            element.is_support = solver.supports[i]
        viewer.setup()
        viewer.show()
    case "lmgc90":
        import outbox2display

        outbox2display.run()
    case "vtk":
        from pyvista_vtk_viewer import vtk_viewer

        vtk_viewer(solver, output_dir=pathlib.Path(__file__).parent, folder="barrel_vault")
