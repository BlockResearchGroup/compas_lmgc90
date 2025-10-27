import numpy as np
from compas.geometry import Transformation
from compas.datastructures import Mesh
from compas_viewer import Viewer
from compas_viewer.config import Config
from compas_lmgc90 import _lmgc90 as lmgc90

def cview(blocks):
    """Compas view"""

    config = Config()
    config.camera.target = [0, 0, 0]
    config.camera.position = [3, 3, 3]

    viewer = Viewer(config=config)

    for i, b in enumerate(blocks):
        viewer.scene.add(b, name=f"Body {i}")
    viewer.show()

def update(blocks, init_coor, init_frame, current_state):
    """Update mesh positions using incremental transformation (matches chipy.update)"""
    trans = np.zeros([4, 4])
    trans[3, 3] = 1.0
    
    for i, b in enumerate(blocks):
        # Get new coordinates and frame from current state
        new_coor = np.array(current_state.bodies[i])
        new_frame = np.array(current_state.body_frames[i]).reshape(3, 3)
        
        # Compute incremental transformation (EXACT same as chipy)
        df = np.matmul(new_frame.T, init_frame[i])
        dc = new_coor - np.matmul(df, init_coor[i])
        
        trans[:3, :3] = df[:, :]
        trans[:3, 3] = dc[:]
        
        # Apply transformation
        T = Transformation.from_matrix(trans.tolist())
        b.transform(T)
        
        # Update for next iteration (IMPORTANT!)
        init_frame[i][:, :] = new_frame[:, :]
        init_coor[i][:] = new_coor[:]

# Hardcoded geometry (EXACT same as chipy example)
faces = np.array([[0, 1, 2],
                  [0, 1, 3],
                  [1, 2, 3],
                  [2, 0, 3]])

vertices0 = np.array([[0.0, 0.0, 0.0],
                      [1.0, 0.0, 0.0],
                      [0.0, 0.7, 0.3],
                      [0.3, -0.1, 0.9]])

vertices1 = np.array([[0.0, 0.0, 0.0],
                      [1.0, 0.0, 0.0],
                      [0.0, 1.0, 0.0],
                      [0.3, 0.3, -1.0]])

pos0 = np.array([0.0, 0.0, 1.0])
pos1 = np.array([0.0, 0.0, 0.8])

# Create meshes
mesh0 = Mesh.from_vertices_and_faces(vertices0, faces)
mesh1 = Mesh.from_vertices_and_faces(vertices1, faces)
blocks = [mesh0, mesh1]

nb_steps = 5000

# Initialize simulation (nanobind equivalent of chipy setup)
lmgc90.initialize_simulation(dt=1e-2, theta=0.5)
lmgc90.set_materials(1)
lmgc90.set_tact_behavs(1)
lmgc90.set_see_tables()
lmgc90.set_nb_bodies(2)

# Add bodies (nanobind equivalent of chipy.RBDY3_setOneTactor)
# Convert geometry to format expected by nanobind
faces_flat_1indexed = (faces + 1).ravel().tolist()  # 1-indexed for LMGC90
vert0_flat = vertices0.ravel().tolist()
vert1_flat = vertices1.ravel().tolist()

lmgc90.set_one_polyr(pos0.tolist(), faces_flat_1indexed, vert0_flat, False)  # Body 0: free
lmgc90.set_one_polyr(pos1.tolist(), faces_flat_1indexed, vert1_flat, True)   # Body 1: fixed

# Close initialization (nanobind equivalent of chipy.RBDY3_synchronize + LoadBehaviours, etc.)
lmgc90.close_before_computing()

# Get initial coordinates and frames (EXACT same as chipy)
result_init = lmgc90.get_initial_state()
init_coor = []
init_frame = []
for i in range(2):
    init_coor.append(np.array(result_init.init_bodies[i]))
    init_frame.append(np.array(result_init.init_body_frames[i]).reshape(3, 3))

# Translate blocks to INPUT position (EXACT same as chipy)
blocks[0].translate(pos0.tolist())
blocks[1].translate(pos1.tolist())

# Optional: view initial position
# cview(blocks)

# Simulation time loop (EXACT same as chipy)
for k in range(1, nb_steps + 1):
    # Compute one simulation step (nanobind equivalent of all chipy compute calls)
    result = lmgc90.compute_one_step()
    
    # Update mesh positions every 25 steps (same as chipy: k % (nb_steps//2) == 0)
    if k % (nb_steps // 2) == 0:
        update(blocks, init_coor, init_frame, result)

# Final view
cview(blocks)

# Cleanup (nanobind equivalent of chipy.Finalize)
lmgc90.finalize_simulation()
