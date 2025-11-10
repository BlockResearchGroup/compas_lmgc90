from compas.datastructures import Mesh
from compas.geometry import SphericalSurface

from compas_dem.elements import Block
from compas_dem.models import BlockModel
from compas_dem.viewer import DEMViewer
from compas_lmgc90.solver import Solver

# =============================================================================
# Geometry
# =============================================================================

surface = SphericalSurface(radius=5)
patch = surface.to_polyhedron(nu=32, nv=12, du=[0, 1.0], dv=[0.1, 0.5])

blocks = []
for polygon in patch.polygons:
    bottom = polygon.points
    top = []
    for point in bottom:
        vector = point - surface.frame.point
        direction = vector.unitized()
        top.append(point + direction * 0.3)
    vertices = bottom + top
    faces = [[0, 3, 2, 1], [4, 5, 6, 7], [2, 3, 7, 6], [1, 2, 6, 5], [0, 1, 5, 4], [3, 0, 4, 7]]
    blocks.append(Mesh.from_vertices_and_faces(vertices, faces))

# Use individual blocks as bricks (mesh.join() has issues)
bricks = []
for i, block in enumerate(blocks):
    brick: Mesh = block.copy()
    brick.attributes["is_support"] = brick.centroid()[2] < 0.4
    bricks.append(brick)

# Scale bricks, what happens when we scale the meshes?
for block in bricks:
    centroid = block.centroid()
    block.translate([-centroid[0], -centroid[1], -centroid[2]])
    block.scale(90.0/100.0)
    block.translate(centroid)

# =============================================================================
# Model and interactions
# =============================================================================

model = BlockModel()

for brick in bricks:
    element = Block.from_mesh(brick)
    element.is_support = brick.attributes["is_support"]
    model.add_element(element)

# LMGC90 performs its own contact detection - don't pre-compute
# model.compute_contacts(tolerance=0.001)

# =============================================================================
# Solver
# =============================================================================

solver = Solver(model)  # Process model once
solver.set_supports_from_model()  # Use supports already set in model
solver.preprocess()  # Setup LMGC90
solver.run(nb_steps=50)  # Run simulation
solver.finalize()

# =============================================================================
# Viz
# =============================================================================

viewer = DEMViewer(BlockModel.from_boxes(solver.trimeshes))

for i, element in enumerate(viewer.model.elements()):
    element.is_support = solver.supports[i]
viewer.setup()
viewer.show()