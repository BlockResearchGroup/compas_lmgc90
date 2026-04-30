"""Regression test: re-initializing LMGC90 in the same process must not crash.

This is the test that catches the "Rhino crashes the second time you run the
script" class of bug. LMGC90 is a stateful Fortran library — without proper
clean_memory plumbing in our wrapper's initialize/finalize, a second run on
already-allocated module state segfaults.

We test at two layers:

1. The lowest-level wrapper: ``LMGC90Solver().initialize(...).finalize()`` in
   a loop. Doesn't need any geometry, doesn't need ``compas_dem`` (which is
   why this test runs in cibuildwheel's slim test env). If this segfaults,
   the test process dies and the whole test fails the run.

2. The high-level ``Solver(model)`` lifecycle (skipped if ``compas_dem`` isn't
   importable, same convention as ``test_placeholder.py``).
"""

import pytest

from compas_lmgc90._lmgc90 import LMGC90Solver


def test_reinit_lowlevel():
    """Re-instantiating ``LMGC90Solver`` 5 times in one process must not crash."""
    for _ in range(5):
        s = LMGC90Solver()
        s.initialize(1e-2, 0.5, False)
        s.finalize()
        del s


def test_reinit_lowlevel_implicit_finalize():
    """Drop the solver without explicit finalize() — defensive reset on the
    next ``initialize`` must still produce a clean run."""
    for _ in range(5):
        s = LMGC90Solver()
        s.initialize(1e-2, 0.5, False)
        # No explicit finalize. The C++ destructor finalizes when `s` is
        # collected on the next loop iteration; the next initialize() also
        # defensively resets state regardless.
        del s


# --------------------------------------------------------------------------- #
# High-level test (gated on compas_dem availability — same as test_placeholder)
# --------------------------------------------------------------------------- #

try:
    from compas_dem.models import BlockModel
    from compas_dem.templates import ArchTemplate
    _HAS_COMPAS_DEM = True
except ImportError:
    _HAS_COMPAS_DEM = False


@pytest.mark.skipif(
    not _HAS_COMPAS_DEM,
    reason="compas_dem not importable (likely compas_cgal API drift)",
)
def test_reinit_full_solver_cycle():
    """A full Solver(model).preprocess().run().finalize() loop must repeat."""
    from compas_lmgc90.solver import Solver

    for _ in range(3):
        template = ArchTemplate(rise=3, span=10, thickness=0.5, depth=0.5, n=10)
        meshes = template.blocks()
        for block in meshes:
            centroid = block.centroid()
            block.translate([-centroid[0], -centroid[1], -centroid[2]])
            block.scale(0.9)
            block.translate(centroid)

        model = BlockModel.from_boxes(meshes)
        solver = Solver(model, density=2750.0)
        solver.set_supports(z_threshold=0.4)
        solver.preprocess()
        solver.run(nb_steps=2)
        solver.finalize()
        del solver
