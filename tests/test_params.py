"""Solver.contact_solver / contact_detection: what they record, what they refuse,
and that a new solver never inherits what an earlier one set.

LMGC90 cannot report its parameters back, so the carry-over test compares the
results of runs: a stack of cubes solved with a contact solver made useless on
purpose must not change the result of the next, default, solver.
"""

import numpy as np
import pytest

from compas_lmgc90.solver import CONTACT_DETECTION_DEFAULTS
from compas_lmgc90.solver import CONTACT_SOLVER_DEFAULTS
from compas_lmgc90.solver import Solver


def _cube(x0, y0, z0, size=1.0):
    s = size
    vertices = [
        [x0, y0, z0],
        [x0 + s, y0, z0],
        [x0 + s, y0 + s, z0],
        [x0, y0 + s, z0],
        [x0, y0, z0 + s],
        [x0 + s, y0, z0 + s],
        [x0 + s, y0 + s, z0 + s],
        [x0, y0 + s, z0 + s],
    ]
    faces = [[3, 2, 1, 0], [4, 5, 6, 7], [0, 1, 5, 4], [1, 2, 6, 5], [2, 3, 7, 6], [3, 0, 4, 7]]
    return {"vertices": vertices, "faces": faces}


def _top_of_stack(solver=None, nb_steps=20):
    """Final centroid of the top cube of a stack of three, bottom one a support."""
    solver = solver or Solver(dt=1e-2, density=2400.0)
    solver.geometry_from_v_f([_cube(0, 0, 0), _cube(0, 0, 1.0), _cube(0.2, 0, 2.0)])
    solver.set_supports(z_threshold=0.6)
    solver.contact_law("IQS_CLB", 0.5)
    solver.preprocess()
    solver.run(nb_steps=nb_steps)
    top = np.array(solver.trimeshes[2].centroid())
    solver.finalize()
    return top


def test_new_solver_records_defaults():
    solver = Solver()
    assert solver.params["contact_solver"] == CONTACT_SOLVER_DEFAULTS
    assert solver.params["contact_detection"] == CONTACT_DETECTION_DEFAULTS
    assert solver.params["max_neighbours"] == 10
    solver.finalize()


def test_contact_solver_sets_only_what_is_given():
    solver = Solver()
    assert solver.contact_solver(tolerance=1e-5) is solver
    assert solver.params["contact_solver"]["tolerance"] == 1e-5
    assert solver.params["contact_solver"]["relaxation"] == CONTACT_SOLVER_DEFAULTS["relaxation"]
    solver.contact_solver(norm="Quad", iterations=20, max_loops=100)
    assert solver.params["contact_solver"] == {**CONTACT_SOLVER_DEFAULTS, "tolerance": 1e-5, "norm": "Quad", "iterations": 20, "max_loops": 100}
    solver.finalize()


def test_contact_detection_sets_only_what_is_given():
    solver = Solver()
    assert solver.contact_detection(method="CpCundall", cundall_iterations=300) is solver
    assert solver.params["contact_detection"]["method"] == "CpCundall"
    assert solver.params["contact_detection"]["cundall_iterations"] == 300
    assert solver.params["contact_detection"]["f2f_tolerance"] == CONTACT_DETECTION_DEFAULTS["f2f_tolerance"]
    solver.finalize()


def test_bad_values_are_refused():
    solver = Solver()
    with pytest.raises(ValueError):
        solver.contact_solver(relaxation=0.0)
    with pytest.raises(ValueError):
        solver.contact_solver(norm="L2")
    with pytest.raises(ValueError):
        solver.contact_detection(method="Voronoi")
    # STO options while another method is in use, or switched to in the same call
    solver.contact_detection(method="CpCundall")
    with pytest.raises(ValueError):
        solver.contact_detection(sto_decompression=0.5)
    solver.contact_detection(method="STO")
    with pytest.raises(ValueError):
        solver.contact_detection(method="CpF2f", sto_explicit=False)
    # read only when the solver is created
    with pytest.raises(ValueError):
        solver.set_param("Low Size Array POLYR", 20)
    solver.finalize()


def test_max_neighbours_is_recorded():
    solver = Solver(max_neighbours=20)
    assert solver.params["max_neighbours"] == 20
    solver.finalize()


def test_parameters_do_not_carry_over_to_the_next_solver():
    reference = _top_of_stack()

    # a contact solver that barely solves anything: one sweep, and anything converges
    sloppy = Solver(dt=1e-2, density=2400.0).contact_solver(tolerance=1.0, iterations=1, max_loops=1)
    assert not np.array_equal(_top_of_stack(sloppy), reference), "the sloppy solver should change the result, else this test proves nothing"

    assert np.array_equal(_top_of_stack(), reference)


def test_setting_the_defaults_changes_nothing():
    reference = _top_of_stack()
    solver = Solver(dt=1e-2, density=2400.0)
    solver.contact_solver(**CONTACT_SOLVER_DEFAULTS).contact_detection(**CONTACT_DETECTION_DEFAULTS)
    assert np.array_equal(_top_of_stack(solver), reference)
