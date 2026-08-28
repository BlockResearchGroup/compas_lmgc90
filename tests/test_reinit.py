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

2. A full solve **with real contacts**, repeated in one process, built from
   raw vertices/faces so it needs neither ``compas_dem`` nor a model. This
   is the one that catches the 0.1.10 crash: STO contact detection keeps a
   procedure-local ``save``d first-call flag that ``clean_memory_PRPRx``
   does not reset, so a second detection pass in the same process walked
   freed arrays. Layer 1 never allocates those arrays (no geometry -> no
   detection) and could not see it.

3. The "rebound global" form: ``solver = Solver(...)`` re-executed in ONE
   persistent namespace (Rhino's ScriptEditor, a notebook cell) without any
   ``finalize()``. Python builds and initialises the new solver first and
   drops the old one afterwards, so the old destructor used to wipe the new
   solver's state; the C++ ownership token makes a superseded solver a
   no-op on release and a ``RuntimeError`` on use.

4. Misuse that used to end in a Fortran ``STOP`` (process exit, no Python
   traceback) now raises before any native call.

5. The high-level ``Solver(model)`` lifecycle (skipped if ``compas_dem`` isn't
   importable, same convention as ``test_placeholder.py``).
"""

import pytest

from compas_lmgc90._lmgc90 import LMGC90Solver
from compas_lmgc90.solver import Solver


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
# Solve-with-contacts test (no compas_dem needed — runs in the wheel test env)
# --------------------------------------------------------------------------- #


def _cube(x0, y0, z0, size=1.0):
    """Axis-aligned unit cube as a ``geometry_from_v_f`` block dict."""
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


def _solve_stacked_cubes(nb_steps=3, explicit_finalize=True):
    """One complete solve: two stacked cubes, bottom one is a support.

    Returns the number of contacts LMGC90 detected on the last step. It is
    asserted to be > 0 by the caller, because a run without contacts would
    not exercise the detection state this test is guarding.
    """
    solver = Solver(dt=1e-2, density=2400.0)
    solver.geometry_from_v_f([_cube(0, 0, 0), _cube(0, 0, 1.0)])
    solver.set_supports(z_threshold=0.6)
    solver.contact_law("IQS_CLB", 0.5)
    solver.preprocess()
    solver.run(nb_steps=nb_steps)
    nb_contacts = len(solver.last_result.interaction_coords)
    if explicit_finalize:
        solver.finalize()
    return nb_contacts


def test_reinit_solve_with_contacts():
    """Three full solves with contacts in one process must not crash.

    Regression test for the 0.1.10 "second solve segfaults in Rhino" bug:
    ``clean_memory_PRPRx`` frees the STO detection arrays but does not reset
    STO's ``save``d first-call flag, so the second solve dereferenced freed
    memory inside ``STO_compute_contact_PRPRx``. ``reset_all_state()`` now
    resets STO explicitly. Three runs, not two, so that a fix that only
    masks the second run is still caught.
    """
    for _ in range(3):
        assert _solve_stacked_cubes() > 0


def test_reinit_solve_with_contacts_implicit_finalize():
    """Same as above without an explicit ``finalize()``.

    The solver is a function local, so CPython releases it on return and the
    C++ destructor finalizes *before* the next ``Solver()`` is built. The
    other order — new solver initialised first, old one released after — is
    what a rebound global produces and is covered by
    :func:`test_reinit_rebound_global_without_finalize`.
    """
    for _ in range(3):
        assert _solve_stacked_cubes(explicit_finalize=False) > 0


def _solve_cube_grid(nx, ny, nz, nb_steps=3):
    """One complete solve of an ``nx * ny * nz`` grid of touching unit cubes."""
    blocks = [_cube(i, j, k) for i in range(nx) for j in range(ny) for k in range(nz)]
    solver = Solver(dt=1e-2, density=2400.0)
    solver.geometry_from_v_f(blocks)
    solver.set_supports(z_threshold=0.6)
    solver.contact_law("IQS_CLB", 0.5)
    solver.preprocess()
    solver.run(nb_steps=nb_steps)
    nb_bodies = len(solver.last_result.bodies)
    nb_contacts = len(solver.last_result.interaction_coords)
    solver.finalize()
    return nb_bodies, nb_contacts


def test_reinit_solve_varying_model_size():
    """Consecutive solves of models of DIFFERENT sizes in one process.

    A user re-running a script in Rhino rarely re-runs the same model: the
    second run typically has more or fewer blocks. Stale per-body or
    per-contact state sized for the previous model is exactly what a
    same-size test can miss, so go small -> large -> small -> large and
    check LMGC90 reports the body count of the *current* model each time.
    """
    for nx, ny, nz in [(1, 1, 2), (3, 3, 3), (1, 1, 2), (4, 4, 2)]:
        nb_bodies, nb_contacts = _solve_cube_grid(nx, ny, nz)
        assert nb_bodies == nx * ny * nz
        assert nb_contacts > 0


# --------------------------------------------------------------------------- #
# Rebound global / superseded solver (C++ ownership token)
# --------------------------------------------------------------------------- #

_REBOUND_SCRIPT = """
solver = Solver(dt=1e-2, density=2400.0)   # the previous `solver` is dropped AFTER this initialised
solver.geometry_from_v_f([_cube(0, 0, 0), _cube(0, 0, 1.0)])
solver.set_supports(z_threshold=0.6)
solver.contact_law("IQS_CLB", 0.5)
solver.preprocess()
solver.run(nb_steps=3)
nb_contacts = len(solver.last_result.interaction_coords)
"""


def test_reinit_rebound_global_without_finalize():
    """Re-running ``solver = Solver(...)`` in one namespace must keep working.

    This is what a script run twice in Rhino's ScriptEditor or a notebook
    cell executed twice does. Evaluation order is what makes it dangerous:
    the new ``Solver`` is constructed and ``initialize()``d first, and only
    then is the old object released. Before the ownership token in
    ``lmgc90.cpp``, the old destructor called ``lmgc90_finalize()`` and
    wiped the new solver's fresh state, and the next native call ended in
    ``STOP 1`` inside ``POLYR::read_bodies`` (process exit, no traceback).
    """
    namespace = {"Solver": Solver, "_cube": _cube}
    for _ in range(3):
        exec(_REBOUND_SCRIPT, namespace)
        assert namespace["nb_contacts"] > 0


def test_superseded_solver_raises_instead_of_crashing():
    """Two live solvers: the older one must raise, not drive the newer one.

    LMGC90's state is process-global, so ``second.__init__`` resets whatever
    ``first`` had. ``first`` must then refuse every native call with a clear
    ``RuntimeError`` (it used to return uninitialised coordinates), and
    finalizing or collecting ``first`` must leave ``second`` intact.
    """
    first = Solver(dt=1e-2, density=2400.0)
    first.geometry_from_v_f([_cube(0, 0, 0), _cube(0, 0, 1.0)])
    first.set_supports(z_threshold=0.6)
    first.contact_law("IQS_CLB", 0.5)
    first.preprocess()
    first.run(nb_steps=1)

    second = Solver(dt=1e-2, density=2400.0)
    second.geometry_from_v_f([_cube(0, 0, 0), _cube(0, 0, 1.0), _cube(0, 0, 2.0)])
    second.set_supports(z_threshold=0.6)
    second.contact_law("IQS_CLB", 0.5)
    second.preprocess()
    second.run(nb_steps=1)

    with pytest.raises(RuntimeError, match="superseded"):
        first.run(nb_steps=1)

    first.finalize()  # must NOT release the state that now belongs to `second`
    del first
    second.run(nb_steps=2)
    assert len(second.last_result.bodies) == 3
    assert len(second.last_result.interaction_coords) > 0
    second.finalize()


# --------------------------------------------------------------------------- #
# Misuse that used to be a Fortran STOP (process exit) must raise instead
# --------------------------------------------------------------------------- #


def test_run_before_preprocess_raises():
    solver = Solver(dt=1e-2)
    solver.geometry_from_v_f([_cube(0, 0, 0)])
    solver.contact_law("IQS_CLB", 0.5)
    with pytest.raises(RuntimeError, match="preprocess"):
        solver.run(nb_steps=1)
    solver.finalize()


def test_preprocess_twice_raises():
    solver = Solver(dt=1e-2)
    solver.geometry_from_v_f([_cube(0, 0, 0), _cube(0, 0, 1.0)])
    solver.set_supports(z_threshold=0.6)
    solver.contact_law("IQS_CLB", 0.5)
    solver.preprocess()
    with pytest.raises(RuntimeError, match="once"):
        solver.preprocess()
    with pytest.raises(RuntimeError, match="before preprocess"):
        solver.contact_law("IQS_CLB", 0.5)
    solver.run(nb_steps=1)  # the first preprocess is still good
    solver.finalize()


def test_preprocess_without_contact_law_raises():
    solver = Solver(dt=1e-2)
    solver.geometry_from_v_f([_cube(0, 0, 0), _cube(0, 0, 1.0)])
    with pytest.raises(RuntimeError, match="contact law"):
        solver.preprocess()
    solver.finalize()


def test_overlong_contact_law_name_raises():
    """A >30 character law name used to overflow a 30-byte stack buffer."""
    solver = Solver(dt=1e-2)
    with pytest.raises(ValueError):
        solver.contact_law("IQS_CLB" + "X" * 40, 0.5)
    solver.finalize()


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
    """A full geometry_from_model -> preprocess -> run -> finalize loop must repeat.

    Mirrors the reported Rhino flow: a ``compas_dem`` arch whose end blocks
    carry ``is_support``, solved three times in one process. The blocks are
    deliberately NOT shrunk: an earlier version of this test scaled every
    block by 0.9 about its centroid, which opened gaps larger than the alert
    distance, produced zero contacts, skipped contact detection entirely,
    and therefore passed on the 0.1.10 wheel that crashed on every second
    real solve. ``nb_contacts > 0`` is asserted so it cannot regress into a
    detection-free test again.
    """
    for _ in range(3):
        template = ArchTemplate(rise=3, span=10, thickness=0.5, depth=0.5, n=10)
        model = BlockModel.from_boxes(template.blocks())
        elements = list(model.elements())
        elements[0].is_support = True
        elements[-1].is_support = True

        solver = Solver(density=2750.0)
        solver.geometry_from_model(model)
        solver.set_supports_from_model()
        # A contact law is mandatory once blocks actually touch: without it
        # LMGC90 aborts the whole process with a Fortran STOP the first time
        # a contact looks up the law ("nickname iqsc0 ... unknown in lawty").
        solver.contact_law("IQS_CLB", 0.6)
        solver.preprocess()
        solver.run(nb_steps=2)
        assert len(solver.last_result.bodies) == len(elements)
        assert len(solver.last_result.interaction_coords) > 0
        solver.finalize()
        del solver
