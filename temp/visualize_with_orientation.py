import os
import sys
from compas.datastructures import Mesh
from compas.geometry import Vector, Transformation, Line
from compas.colors import Color
from compas_viewer import Viewer

# Add the project root to Python path
sys.path.insert(0, '/home/pv/brg/code_fortran/compas_lmgc90')

# Change to the correct working directory first
os.chdir('/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Sandbox/Compas')

try:
    from compas_lmgc90 import _lmgc90, parse_bodies_dat
    print("Successfully imported _lmgc90")
except ImportError as e:
    print(f"Import error: {e}")
    print("Make sure you're running from the correct directory and the module is built")
    sys.exit(1)

# Run simulation
result = _lmgc90.run_simulation()
bodies = parse_bodies_dat(os.path.join('DATBOX', 'BODIES.DAT'))
viewer = Viewer()

# Visualization settings
force_scale = 0.00002

# Add body meshes
for i, body in enumerate(bodies):
    mesh = Mesh.from_vertices_and_faces(body['vertices'], body['faces'])
    
    # Create transformation from rotation matrix (column-major) and translation
    frame = result.body_frames[i]
    pos = result.bodies[i]
    matrix = [[frame[0], frame[3], frame[6], pos[0]],
              [frame[1], frame[4], frame[7], pos[1]],
              [frame[2], frame[5], frame[8], pos[2]],
              [0, 0, 0, 1]]
    
    mesh.transform(Transformation.from_matrix(matrix))
    color = (0.3, 0.3, 0.3) if body.get('color') == 'GRND_' else (0.8, 0.6, 0.4)
    viewer.scene.add(mesh, facecolor=color, opacity=0.1, hide_coplanaredges=True)

# Add contact forces (3 separate lines: T, N, S)
for coord, forces, frame in zip(result.interaction_coords, result.interaction_rloc, result.interaction_uc):
    T, N, S = frame[0:3], frame[3:6], frame[6:9]
    
    # Tangent force (green)
    end_T = [coord[0] + T[0]*forces[0]*force_scale, 
             coord[1] + T[1]*forces[0]*force_scale, 
             coord[2] + T[2]*forces[0]*force_scale]
    viewer.scene.add(Line(coord, end_T), linecolor=Color.red(), linewidth=3)
    
    # Normal force (red/blue)
    end_N = [coord[0] + N[0]*forces[1]*force_scale, 
             coord[1] + N[1]*forces[1]*force_scale, 
             coord[2] + N[2]*forces[1]*force_scale]
    color_N = (1, 0, 0) if forces[1] > 0 else (0, 0, 1)
    viewer.scene.add(Line(coord, end_N), linecolor=Color.blue(), linewidth=3)
    
    # Slip force (yellow)
    end_S = [coord[0] + S[0]*forces[2]*force_scale, 
             coord[1] + S[1]*forces[2]*force_scale, 
             coord[2] + S[2]*forces[2]*force_scale]
    viewer.scene.add(Line(coord, end_S), linecolor=Color.green(), linewidth=3)

viewer.show()
