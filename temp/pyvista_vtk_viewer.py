import pathlib

import compas.geometry as cg

__all__ = ["vtk_viewer"]

# Per-series call counters for vtk_viewer(step="auto"), keyed by (folder, name).
_STEP_COUNTERS = {}


def _face_resultants(result):
    """Aggregate contact interactions into one resultant per face (block-pair).

    Returns
    -------
    list of tuple
        ``(pair, position, force)`` per interface, where ``force`` is the unscaled
        resultant vector ``F = w*Fn + u*Ft + v*Fs`` applied at the ``|Fn|``-weighted
        contact position (geometric centroid for a pure-shear interface).
    """
    faces = {}
    for i in range(len(result.interaction_coords)):
        point = cg.Point(*result.interaction_coords[i])
        w = cg.Vector(*result.interaction_normals[i])  # frame.zaxis
        u = cg.Vector(*result.interaction_tangent1[i])  # frame.xaxis
        v = cg.Vector(*result.interaction_tangent2[i])  # frame.yaxis
        Ft, Fn, Fs = result.interaction_rloc[i]
        pair = tuple(sorted(result.interaction_bodies[i]))
        face = faces.setdefault(pair, {"sum_fn": 0.0, "sum_ft": 0.0, "sum_fs": 0.0, "wpos": cg.Vector(0, 0, 0), "weight": 0.0, "pts": [], "frame": (w, u, v)})
        face["sum_fn"] += Fn
        face["sum_ft"] += Ft
        face["sum_fs"] += Fs
        face["wpos"] += cg.Vector(*point) * abs(Fn)  # position weighted by |Fn|
        face["weight"] += abs(Fn)
        face["pts"].append(point)

    resultants = []
    for pair, face in faces.items():
        w, u, v = face["frame"]
        if face["weight"] > 1e-9:
            pos = cg.Point(*(face["wpos"] * (1.0 / face["weight"])))
        else:  # pure shear / no normal load -> geometric centroid of the contact points
            pos = cg.Point(*(sum((cg.Vector(*p) for p in face["pts"]), cg.Vector(0, 0, 0)) * (1.0 / len(face["pts"]))))
        force = w * face["sum_fn"] + u * face["sum_ft"] + v * face["sum_fs"]  # unscaled resultant force
        resultants.append((pair, pos, force))
    return resultants


def _mesh_to_polydata(mesh):
    """Convert a COMPAS mesh into a :class:`pyvista.PolyData` surface."""
    import numpy as np
    import pyvista as pv

    vertices, faces = mesh.to_vertices_and_faces()
    cells = []
    for f in faces:
        cells.append(len(f))
        cells.extend(f)
    return pv.PolyData(np.asarray(vertices, dtype=float), faces=np.asarray(cells, dtype=int))


def _polygon_to_polydata(polygon):
    """Convert a COMPAS polygon into a single-face :class:`pyvista.PolyData`."""
    import numpy as np
    import pyvista as pv

    pts = np.asarray([list(p) for p in polygon.points], dtype=float)
    n = len(pts)
    faces = np.hstack([[n], np.arange(n)]).astype(int)
    return pv.PolyData(pts, faces=faces)


def vtk_viewer(solver, output_dir=None, folder="vtk_output", name=None):
    """Export blocks, contact faces and resultant forces to VTK files.

    Mirrors :func:`view` but goes through PyVista, writing three VTK datasets into
    ``<output_dir>/<folder>``. Each call auto-increments a per-series counter, so
    calling it repeatedly inside a run loop builds a numbered VTK time series
    (1, 2, 3, ...) that ParaView can play back:

    >>> for it in range(1, 16501):
    ...     solver.run(nb_steps=1)
    ...     if it % 1000 == 0:
    ...         vtk_viewer(solver, name="arch")

    Parameters
    ----------
    solver : :class:`compas_lmgc90.solver.Solver`
        A solver that has already been run (``solver.last_result`` must be set).
    output_dir : path-like, optional
        Base directory for the output folder. Defaults to the current working
        directory; pass ``pathlib.Path(__file__).parent`` to sit next to a script.
    folder : str, optional
        Name of the sub-folder created under ``output_dir``.
    name : str, optional
        Filename prefix, e.g. ``name="arch"`` -> ``arch_blocks_...``. Useful to
        keep several runs side by side in the same folder.
    """
    import numpy as np
    import pyvista as pv

    # --- Blocks -----------------------------------------------------------
    block_parts = []
    for i, mesh in enumerate(solver.trimeshes):
        part = _mesh_to_polydata(mesh)
        part.cell_data["block_id"] = np.full(part.n_cells, i)
        part.cell_data["is_support"] = np.full(part.n_cells, int(solver.supports[i]))
        block_parts.append(part)
    blocks = pv.merge(block_parts) if block_parts else pv.PolyData()

    # --- Contact faces ----------------------------------------------------
    contacts = solver.get_contacts()
    face_parts = []
    for i, polygon in enumerate(contacts["contact_polygons"]):
        part = _polygon_to_polydata(polygon)
        part.cell_data["face_id"] = np.full(part.n_cells, i)
        face_parts.append(part)
    contact_faces = pv.merge(face_parts) if face_parts else pv.PolyData()

    # --- Resultant forces (one point per face) ----------------------------
    resultants = _face_resultants(solver.last_result)
    positions = np.asarray([list(pos) for _, pos, _ in resultants], dtype=float).reshape(-1, 3)
    vectors = np.asarray([list(force) for _, _, force in resultants], dtype=float).reshape(-1, 3)
    cloud = pv.PolyData(positions) if len(positions) else pv.PolyData()
    if len(positions):
        cloud["resultant"] = vectors
        cloud["magnitude"] = np.linalg.norm(vectors, axis=1)
        cloud["body_a"] = np.asarray([pair[0] for pair, _, _ in resultants])
        cloud["body_b"] = np.asarray([pair[1] for pair, _, _ in resultants])
        # Mark 'resultant' as the active VECTORS so ParaView's Glyph orients arrows
        # along the true xyz force direction (otherwise they default to +X, horizontal).
        cloud.set_active_vectors("resultant")

    # --- Write files ------------------------------------------------------
    base = pathlib.Path(output_dir) if output_dir is not None else pathlib.Path.cwd()
    out = base / folder
    out.mkdir(parents=True, exist_ok=True)

    # Auto-increment a per-series counter (keyed by folder + name) for numbering.
    key = (str(out.resolve()), name)
    number = _STEP_COUNTERS.get(key, 0) + 1
    _STEP_COUNTERS[key] = number

    def _fname(kind):
        parts = ([name] if name else []) + [kind, f"{number:09d}"]
        return out / ("_".join(parts) + ".vtk")

    blocks.save(_fname("blocks"))
    contact_faces.save(_fname("contact_faces"))
    cloud.save(_fname("resultants"))

    return out
