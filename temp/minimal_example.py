from compas.geometry import Frame, Vector
from compas.datastructures import Mesh
from compas_viewer import Viewer
from compas.geometry import Transformation

# Hardcoded geometry from C example
faces_flat = [1, 2, 3, 1, 2, 4, 2, 3, 4, 3, 1, 4]
vert0 = [0.0, 0.0, 0.0 ,1.0, 0.0, 0.0 ,0.0, 1.0, 0.0 ,0.3, 0.3, 1.0]
vert1 = [0.0, 0.0, 0.0 ,1.0, 0.0, 0.0 ,0.0, 1.0, 0.0 ,0.3, 0.3, -1.0]

# Convert to vertices and faces
vertices0 = [[vert0[i], vert0[i+1], vert0[i+2]] for i in range(0, len(vert0), 3)]
vert1[11] = -1.0
vertices1 = [[vert1[i], vert1[i+1], vert1[i+2]] for i in range(0, len(vert1), 3)]
faces = [[faces_flat[i]-1, faces_flat[i+1]-1, faces_flat[i+2]-1] for i in range(0, len(faces_flat), 3)]

# Body 0 transformation (from C output)
pos0 = [0.325000, 0.325000,  1.250000]
frame0 = [
    [-0.707107, 0.707107, -0.000000],
    [-0.059597, -0.059597, 0.996442],
    [0.704591, 0.704591, 0.084283]
]

# Body 1 transformation (from C output)
pos1 = [0.325000, 0.325000, 1.240000]
frame1 = [
    [-0.707107, 0.707107, 0.000000],
    [-0.059597, -0.059597, 0.996442],
    [0.704591, 0.704591, 0.084283]
]

pos2 = [0.325000, 0.325000, 1.240000]
frame2 = [
    [-0.707107, 0.707107, 0.000000],
    [-0.059597, -0.059597, 0.996442],
    [0.704591, 0.704591, 0.084283]
]

# Body 1 transformation (from C output)
pos3 = [0.325000, 0.325000, 0.740000]
frame3 = [
    [-0.707107, 0.707107, 0.000000],
    [0.059597, 0.059597, 0.996442],
    [0.704591, 0.704591, -0.084283]
]

def get_T0_T1(pos, frame):
    return Transformation.from_matrix([
        [frame[0][0], frame[0][1], frame[0][2], pos[0]],
        [frame[1][0], frame[1][1], frame[1][2], pos[1]],
        [frame[2][0], frame[2][1], frame[2][2], pos[2]],
        [0.0, 0.0, 0.0, 1.0]
    ])

# Create 4x4 transformation matrices
T0 = get_T0_T1(pos0, frame0)
T1 = get_T0_T1(pos1, frame1)
T2 = get_T0_T1(pos2, frame2)
T3 = get_T0_T1(pos3, frame3)

# Create and transform meshes
mesh0 = Mesh.from_vertices_and_faces(vertices0, faces)
mesh1 = Mesh.from_vertices_and_faces(vertices1, faces)

# Orientation to xy frame does not work
# _T0 = Transformation.from_frame_to_frame(Frame(mesh0.centroid(), Vector.Xaxis(), Vector.Yaxis()),Frame.worldXY())
# _T1 = Transformation.from_frame_to_frame(Frame(mesh1.centroid(), Vector.Xaxis(), Vector.Yaxis()),Frame.worldXY())
# mesh0.transform(_T0)
# mesh1.transform(_T1)

mesh0.transform(T0)
mesh1.transform(T1)
mesh0.transform(T2)
mesh1.transform(T3)

# Viewer
viewer = Viewer()
viewer.scene.add(mesh0, name="Body 0")
viewer.scene.add(mesh1, color=(255, 0, 0), name="Body 1")
viewer.show()