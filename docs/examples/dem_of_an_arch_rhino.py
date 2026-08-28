#! python 3
# r: compas_lmgc90>=0.1.11
# r: compas_dem
# r: numpy
"""
Run an arch DEM simulation with LMGC90 and bake the deformed blocks
into the active Rhino document.

Paste this whole script into Rhino 8's ScriptEditor (Tools > ScriptEditor)
and run it. The first run downloads compas_lmgc90 and its dependencies
into Rhino's CPython environment via the `# r:` directive at the top.
Subsequent runs reuse the cached install.

Memory management note: there is no ``solver.finalize()`` call below
and no ``with`` block. Both are optional — the wrapper's ``initialize``
defensively resets every LMGC90 module's state at the start of each run,
and a solver left over from a previous run becomes inert once a newer one
exists (its methods raise ``RuntimeError`` instead of touching the new
simulation), so re-running this script in Rhino's persistent ScriptEditor
session is safe by construction. LMGC90 holds exactly ONE simulation per
process: keep the results of a run (``solver.trimeshes`` copies,
``solver.get_contacts()``), not the solver object itself, in anything
that outlives the script run (sticky dicts, Grasshopper component state).
"""

import numpy as np
import scriptcontext as sc
import Rhino
import Rhino.Geometry as rg

from compas_dem.models import BlockModel
from compas_dem.templates import ArchTemplate

from compas_lmgc90.solver import Solver

# =============================================================================
# Build the arch (same template + scaling as docs/examples/dem_of_an_arch.py)
# =============================================================================

template = ArchTemplate(rise=3, span=10, thickness=0.5, depth=0.5, n=20)
meshes = template.blocks()

for block in meshes:
    centroid = block.centroid()
    block.translate([-centroid[0], -centroid[1], -centroid[2]])
    block.scale(90.0 / 100.0)
    block.translate(centroid)

model = BlockModel.from_boxes(meshes)

# =============================================================================
# Run the simulation — plain imperative form, no `with`, no finalize().
# Re-running this script in the same Rhino session is safe.
# =============================================================================

solver = Solver(density=2750.0, debug=False)
solver.geometry_from_model(model)
solver.set_supports(z_threshold=0.4)
solver.apply_velocity(
    block_index=10,
    component="Vz",
    value=np.array([[0.0, 0.5, 1.0], [0.0, 0.0, 1e-2]]),
)
solver.contact_law("IQS_CLB", 0.35)

solver.preprocess()
solver.run(nb_steps=100)

# =============================================================================
# Bake the post-solve blocks into Rhino
# =============================================================================

doc = sc.doc

layer_name = "compas_lmgc90"
layer_index = doc.Layers.FindByFullPath(layer_name, -1)
if layer_index < 0:
    layer = Rhino.DocObjects.Layer()
    layer.Name = layer_name
    layer_index = doc.Layers.Add(layer)

attrs = Rhino.DocObjects.ObjectAttributes()
attrs.LayerIndex = layer_index

for compas_mesh in solver.trimeshes:
    rhino_mesh = rg.Mesh()

    vertices, faces = compas_mesh.to_vertices_and_faces(True)
    for x, y, z in vertices:
        rhino_mesh.Vertices.Add(x, y, z)
    for face in faces:
        if len(face) == 3:
            rhino_mesh.Faces.AddFace(face[0], face[1], face[2])
        elif len(face) == 4:
            rhino_mesh.Faces.AddFace(face[0], face[1], face[2], face[3])
        else:
            for i in range(1, len(face) - 1):
                rhino_mesh.Faces.AddFace(face[0], face[i], face[i + 1])

    rhino_mesh.Normals.ComputeNormals()
    rhino_mesh.Compact()

    doc.Objects.AddMesh(rhino_mesh, attrs)

doc.Views.Redraw()
print(f"Baked {len(solver.trimeshes)} blocks onto layer '{layer_name}'.")
