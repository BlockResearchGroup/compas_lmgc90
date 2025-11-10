from compas_dem.models import BlockModel
from compas_dem.templates import ArchTemplate
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
    solver = Solver(model)  # Process model once
    solver.set_supports(z_threshold=0.4)  # Set support flags
    solver.preprocess()  # Setup LMGC90
    solver.run(nb_steps=1)  # Run simulation
    solver.finalize()

    assert len(solver.trimeshes) == len(list(model.elements()))
