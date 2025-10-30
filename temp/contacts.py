from compas_viewer import Viewer
from compas_lmgc90.solver import Solver
from compas_dem.models import BlockModel
from compas_dem.templates import ArchTemplate

# =============================================================================
# Create simple model
# =============================================================================

template = ArchTemplate(rise=3, span=10, thickness=0.5, depth=0.5, n=20)
meshes = template.blocks()

model = BlockModel.from_boxes(meshes)

# =============================================================================
# Run solver
# =============================================================================

solver = Solver(model)
solver.set_supports(z_threshold=0.4)
solver.preprocess()
solver.run(nb_steps=100)  # Run a few steps to generate contacts

# =============================================================================
# Get contact visualization data (lines created automatically)
# =============================================================================

contacts = solver.get_contacts(scale_normal=0.2, scale_force=0.00002, polygon_size=0.03)

# =============================================================================
# Visualization with COMPAS viewer
# =============================================================================

viewer = Viewer()

# Group: Blocks
group_blocks = viewer.scene.add_group(name="Blocks")
for i, mesh in enumerate(solver.trimeshes):
    is_support = solver.supports[i]
    color = (255,0,0) if is_support else (200, 200, 200)
    group_blocks.add(mesh, name=f"block_{i}", facecolor=color, show_edges=True, opacity=0.25)

# Group: Contact Polygons
group_polygons = viewer.scene.add_group(name="Contact_Polygons")
for i, polygon in enumerate(contacts['contact_polygons']):
    group_polygons.add(polygon, name=f"polygon_{i}", color=(0, 100, 0))

# Group: Compression Forces (blue) - Fn > 0
group_forces_compression = viewer.scene.add_group(name="Forces_Compression")
for i, line in enumerate(contacts['force_compression_lines']):
    group_forces_compression.add(line, name=f"force_compression_{i}", linewidth=5, color=(0, 0, 255))

# Group: Tension Forces (red) - Fn < 0
group_forces_tension = viewer.scene.add_group(name="Forces_Tension")
for i, line in enumerate(contacts['force_tension_lines']):
    group_forces_tension.add(line, name=f"force_tension_{i}", linewidth=5, color=(255, 0, 0))

# Group: Shear Forces Fs (black)
group_forces_shear = viewer.scene.add_group(name="Forces_Shear_Fs")
for i, line in enumerate(contacts['force_tangent2_lines']):
    group_forces_shear.add(line, name=f"force_shear_{i}", linewidth=5, color=(0, 0, 0))

# Group: Resultant Forces (dark green)
group_forces_resultant = viewer.scene.add_group(name="Forces_Resultant")
for i, line in enumerate(contacts['force_resultants']):
    group_forces_resultant.add(line, name=f"force_resultant_{i}", linewidth=5, color=(0, 100, 0))

viewer.show()

# Cleanup
solver.finalize()
