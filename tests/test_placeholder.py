import pytest

# compas_dem transitively imports compas_cgal.meshing.project_mesh_on_mesh,
# which has been renamed/removed in recent compas_cgal releases. Skip the
# integration test cleanly when that mismatch surfaces — our wheel itself
# is exercised by the cibuildwheel test-command's `import compas_lmgc90`
# smoke check.
try:
    from compas_dem.models import BlockModel
    from compas_dem.templates import ArchTemplate
except ImportError as exc:
    pytest.skip(
        f"compas_dem could not be imported (likely compas_cgal API drift): {exc}",
        allow_module_level=True,
    )

from compas_lmgc90.solver import Solver


def test_solver():
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
        block.scale(90.0 / 100.0)
        block.translate(centroid)

    # =============================================================================
    # Model
    # =============================================================================

    model = BlockModel.from_boxes(meshes)

    # =============================================================================
    # Solver
    # =============================================================================
    solver = Solver()  # Process model once
    solver.geometry_from_model(model)
    solver.set_supports(z_threshold=0.4)  # Set support flags
    solver.preprocess()  # Setup LMGC90
    solver.run(nb_steps=1)  # Run simulation
    solver.finalize()

    assert len(solver.trimeshes) == len(list(model.elements()))
