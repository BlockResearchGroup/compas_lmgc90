from compas_dem.templates import BarrelVaultTemplate
from compas_dem.elements import Block
from compas_dem.models import BlockModel
from compas.colors import Color
import numpy as np
from compas_viewer import Viewer

from compas_lmgc90 import _lmgc90

# =============================================================================
# Block Geometry
# =============================================================================

arch = BarrelVaultTemplate()

# =============================================================================
# Block Model
# =============================================================================

model = BlockModel()
for block in arch.blocks():
    block = Block.from_mesh(block)
    model.add_element(block)

# =============================================================================
# Contacts
# =============================================================================

model.compute_contacts()

# =============================================================================
# Supports
# =============================================================================

z_threshold = 0.3
bottom_elements = [elem for elem in model.elements() if elem.point.z < z_threshold]
for element in bottom_elements:
    element.is_support = True

# =============================================================================
# Convert meshes to Eigen types and NumPy arrays
# =============================================================================

gravity = -9.81
is_support = np.array([elem.is_support for elem in model.elements()], dtype=bool)
nodes = np.array(list(model.graph.nodes()))
edges = np.array(list(model.graph.edges()))

vertices_arrays = _lmgc90.VectorNumpyArrayDouble()
faces_arrays = _lmgc90.VectorNumpyArrayInt()
for element in model.elements():
    vertices_arrays.append(np.array([element.geometry.vertex_coordinates(vkey) for vkey in element.geometry.vertices()], dtype=np.float64))
    faces_arrays.append(np.array([element.geometry.face_vertices(fkey) for fkey in element.geometry.faces()], dtype=np.int32))

# =============================================================================
# Solver - lmgc90
# =============================================================================
print(edges)
_lmgc90.solve(
    gravity,
    is_support,
    nodes,
    edges,
    vertices_arrays,
    faces_arrays
)

# =============================================================================
# Update Meshes
# =============================================================================

for i, element in enumerate(model.elements()):
    geometry = element.geometry
    for j, vkey in enumerate(geometry.vertices()):
        geometry.vertex_attributes(vkey, 'xyz', vertices_arrays[i][j])
    element.geometry = geometry

# =============================================================================
# Viz
# =============================================================================

viewer = Viewer()
for element in model.elements():
    viewer.scene.add(element.modelgeometry, show_faces=False)

for contact in model.contacts():
    viewer.scene.add(contact.polygon, surfacecolor=Color.cyan())

viewer.show()