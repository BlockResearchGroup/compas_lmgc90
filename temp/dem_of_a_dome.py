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

columns = [blocks[i : i + 12] for i in range(0, len(blocks), 12)]
rows = list(zip(*columns))

bricks = []
for i in range(len(rows)):
    for j in range(0, len(rows[0]), 2):
        if i % 2 == 0:
            a: Mesh = rows[i][j]
            b: Mesh = rows[i][j + 1]
        else:
            a: Mesh = rows[i][j - 1]
            b: Mesh = rows[i][j]
        brick: Mesh = a.copy()
        brick.join(b, True)
        brick.attributes["is_support"] = i == len(rows) - 1
        bricks.append(brick)

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
# Scale meshes for quick test
# =============================================================================

for block in model.blocks():
    mesh = block.modelgeometry
    centroid = mesh.centroid()
    mesh.translate([-centroid[0], -centroid[1], -centroid[2]])
    mesh.scale(99.0/100.0)
    mesh.translate(centroid)

# =============================================================================
# Solver
# =============================================================================

solver = Solver(model)  # Process model once
solver.set_supports_from_model()  # Use supports already set in model
solver.preprocess()  # Setup LMGC90
solver.run(nb_steps=10)  # Run simulation
solver.finalize()

# =============================================================================
# Viz
# =============================================================================

viewer = DEMViewer(BlockModel.from_boxes(solver.trimeshes))

for i, element in enumerate(viewer.model.elements()):
    element.is_support = solver.supports[i]
viewer.setup()
viewer.show()