import pathlib

from compas.datastructures import Mesh
from compas.files import OBJ

from compas_dem.elements import Block
from compas_dem.models import BlockModel
from compas_dem.viewer import DEMViewer
from compas_lmgc90.solver import Solver

# =============================================================================
# Data
# =============================================================================

FILE = pathlib.Path(__file__).parent.parent / "data" / "crossvault.obj"

obj = OBJ(FILE)
obj.read()

meshes = []
for name in obj.objects:  # type: ignore
    vertices, faces = obj.objects[name]  # type: ignore
    mesh: Mesh = Mesh.from_vertices_and_faces(vertices, faces)
    mesh.scale(0.025, 0.025, 0.025)
    mesh.name = name
    meshes.append(mesh)

# =============================================================================
# Model and interactions
# =============================================================================

model = BlockModel()
for mesh in meshes:
    element = Block.from_mesh(mesh)
    model.add_element(element)

model.compute_contacts(tolerance=0.001)

# =============================================================================
# Supports
# =============================================================================

for element in model.elements():
    if model.graph.degree(element.graphnode) == 1:
        element.is_support = True

# =============================================================================
# Scale meshes for quick test
# =============================================================================

for block in model.blocks():
    mesh = block.modelgeometry
    centroid = mesh.centroid()
    # mesh.translate([-centroid[0], -centroid[1], -centroid[2]])
    # mesh.scale(99.0/100.0)
    # mesh.translate(centroid)

# =============================================================================
# Solver
# =============================================================================

solver = Solver(model)  # Process model once
solver.set_supports_from_model()  # Use supports already set in model
solver.preprocess()  # Setup LMGC90
solver.run(nb_steps=100)  # Run simulation
solver.finalize()

# =============================================================================
# Viz - Create model from transformed blocks
# =============================================================================

viewer = DEMViewer(BlockModel.from_boxes(solver.trimeshes))
for i, element in enumerate(viewer.model.elements()):
    element.is_support = solver.supports[i]
viewer.setup()
viewer.show()
