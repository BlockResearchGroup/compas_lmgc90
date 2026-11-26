"""Regression test: two touching blocks that are both fully driven.

A contact between two bodies whose 6 velocity dofs are all imposed cannot move
either of them, so LMGC90's contact operator is zero there (Wnn = 0). nlgs_3D
logs it and carries on with 1/Wnn, which gives NaN reactions; comp_dof_RBDY3
then rotates the driven bodies' inertia frames with that NaN before it resets
their prescribed velocity, and the next detection ends in a Fortran ``STOP 1``
("body POLYR 1 out of boxes"): the process exits, there is no exception.

Whether the NaN appears depends on rounding (two unit cubes stay clean), so
the test uses two real blocks of a COMPAS-Masonry support row that do fail
without the fix. Fully driven blocks now get their own contactor colour and
the see tables have no entry between two of them, so no such contact exists.
"""

import numpy as np

from compas_lmgc90.solver import Solver

FACES = [[0, 1, 3, 2], [0, 4, 5, 1], [4, 6, 7, 5], [6, 2, 3, 7], [1, 5, 7, 3], [2, 6, 4, 0]]


def _support_block(y0, y1):
    """One block of the support row of the COMPAS-Masonry session that crashed Rhino."""
    xz = [
        (-2.7161429366366288, 0.43019477886063484),
        (-3.2099871069341974, 0.5084120113807502),
        (-2.75, 0.0),
        (-3.25, 3.061616997868381e-17),
    ]
    vertices = [[x, y0, z] for x, z in xz] + [[x, y1, z] for x, z in xz]
    return {"vertices": vertices, "faces": FACES}


# blocks 19 and 39 of the session: they share the face y = -2.55
BLOCK_19 = _support_block(-2.85, -2.5500000000000003)
BLOCK_39 = _support_block(-2.5500000000000003, -2.25)


def _drive(solver, block_index, vx):
    for component in ("Vx", "Vy", "Vz", "Rx", "Ry", "Rz"):
        solver.apply_velocity(block_index, component, vx if component == "Vx" else 0.0)


def test_is_fully_driven():
    solver = Solver(dt=5e-3, density=2400.0)
    solver.geometry_from_v_f([BLOCK_19, BLOCK_39])
    for component in ("Vx", "Vy", "Vz", "Rx", "Ry"):
        solver.apply_velocity(0, component, 0.0)
    assert not solver.is_fully_driven(0)
    solver.apply_velocity(0, "Rz", 0.0)
    assert solver.is_fully_driven(0)
    assert not solver.is_fully_driven(1)
    solver.finalize()


def test_two_driven_blocks_do_not_touch():
    """The minimal case of the crash: both blocks driven, nothing else."""
    solver = Solver(dt=5e-3, theta=0.5, density=2400.0)
    solver.geometry_from_v_f([BLOCK_19, BLOCK_39])
    _drive(solver, 0, -0.01)
    _drive(solver, 1, -0.01)
    solver.contact_law("IQS_CLB", 0.7)
    solver.preprocess()
    start = [np.array(mesh.centroid()) for mesh in solver.trimeshes]
    for _ in range(5):  # it used to STOP in the third step
        solver.run(nb_steps=1)
        assert len(solver.last_result.interaction_gap) == 0
        assert np.all(np.isfinite(solver.last_result.body_frames))
    # both blocks moved by 5 * dt * vx
    for mesh, centroid in zip(solver.trimeshes, start):
        assert np.allclose(np.array(mesh.centroid()) - centroid, [-2.5e-4, 0, 0], atol=1e-9)
    solver.finalize()


def _cube(x0, y0, z0):
    vertices = [[x0 + dx, y0 + dy, z0 + dz] for dz in (0, 1) for dx, dy in ((0, 0), (1, 0), (1, 1), (0, 1))]
    faces = [[3, 2, 1, 0], [4, 5, 6, 7], [0, 1, 5, 4], [1, 2, 6, 5], [2, 3, 7, 6], [3, 0, 4, 7]]
    return {"vertices": vertices, "faces": faces}


def test_free_block_still_touches_driven_blocks():
    """Two touching supports and a free cube across both: only support-support is left out."""
    solver = Solver(dt=1e-2, density=2400.0)
    solver.geometry_from_v_f([_cube(0, 0, 0), _cube(1, 0, 0), _cube(0.5, 0, 1)])
    solver.set_supports(z_threshold=0.6)
    solver.contact_law("IQS_CLB", 0.5)
    solver.preprocess()
    solver.run(nb_steps=20)
    pairs = {tuple(sorted(p)) for p in solver.last_result.interaction_bodies}
    assert (1, 2) not in pairs  # support - support (1-based body numbers)
    assert {(1, 3), (2, 3)} <= pairs  # support - free
    # the free cube stays on the supports
    assert np.allclose(solver.trimeshes[2].centroid(), [1.0, 0.5, 1.5], atol=1e-3)
    solver.finalize()
