import os
import sys
from compas.datastructures import Mesh
from compas.geometry import Transformation, Line
from compas.colors import Color
from compas_viewer import Viewer
from compas_lmgc90 import _lmgc90_iterative, parse_bodies_dat
from compas import json_dump
import time

# Change to the correct working directory first
os.chdir('/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Sandbox/Compas')

_lmgc90_iterative.initialize_simulation()
bodies = parse_bodies_dat(os.path.join('DATBOX', 'BODIES.DAT'))
force_scale = 0.0000002
transforms = []
meshes = []
objects_to_remove = []

viewer = Viewer()
for i, body in enumerate(bodies):
    mesh = Mesh.from_vertices_and_faces(body['vertices'], body['faces'])
    meshes.append(mesh)
    objects_to_remove.append(viewer.scene.add(mesh, opacity=1.0, hide_coplanaredges=True))


@viewer.on(interval=200)
def dynamic_update(frame):
# for step in range(1000):

    # Remove all previous objects
    # for obj in objects_to_remove:
    #     viewer.scene.remove(obj)
    # objects_to_remove.clear()

    # data = {
    #     'bodies': [],
    #     'result': []
    # }

    # 
    for i in range(1):
        result = _lmgc90_iterative.compute_one_step()
    
    # Bodies
    # start = time.time()

    for i, body in enumerate(bodies):

        if len(transforms) > 0:
            transfrom_inv = Transformation.inverse(transforms[i])
            meshes[i].transform(transfrom_inv)
    transforms.clear()


    for i, body in enumerate(bodies):

        frame = result.body_frames[i]
        pos = result.bodies[i]
        matrix = [[frame[0], frame[3], frame[6], pos[0]],
                  [frame[1], frame[4], frame[7], pos[1]],
                  [frame[2], frame[5], frame[8], pos[2]],
                  [0, 0, 0, 1]]
        transform = Transformation.from_matrix(matrix)
        transforms.append(transform)

        meshes[i].transform(transform)
        objects_to_remove[i].update(update_data=True)

    # end = time.time()
    # print("_____________________________ Elapsed time:" + str(end - start))

    # data['bodies'].append({
    #     'mesh': mesh,
    #     'color': color
    # })
    # # Forces
    # for coord, forces, frame in zip(result.interaction_coords, result.interaction_rloc, result.interaction_uc):
    #     T, N, S = frame[0:3], frame[3:6], frame[6:9]
        
    #     end_T = [coord[0] + T[0]*forces[0]*force_scale, 
    #              coord[1] + T[1]*forces[0]*force_scale, 
    #              coord[2] + T[2]*forces[0]*force_scale]
    #     data['result'].append({
    #         'line': Line(coord, end_T),
    #         'color': Color.red()
    #     })
    #     # viewer.scene.add(Line(coord, end_T), linecolor=Color.red(), linewidth=3)
        
    #     end_N = [coord[0] + N[0]*forces[1]*force_scale, 
    #              coord[1] + N[1]*forces[1]*force_scale, 
    #              coord[2] + N[2]*forces[1]*force_scale]
    #     # viewer.scene.add(Line(coord, end_N), linecolor=Color.blue(), linewidth=3)
    #     data['result'].append({
    #         'line': Line(coord, end_N),
    #         'color': Color.blue()
    #     })
        
    #     end_S = [coord[0] + S[0]*forces[2]*force_scale, 
    #              coord[1] + S[1]*forces[2]*force_scale, 
    #              coord[2] + S[2]*forces[2]*force_scale]
    #     # viewer.scene.add(Line(coord, end_S), linecolor=Color.green(), linewidth=3)
    #     data['result'].append({
    #         'line': Line(coord, end_S),
    #         'color': Color.green()
    #     })
    
    # if step == 0:
    #     viewer.show()
    # if (step%100 == 0):
    #     json_dump(data, 'result_{}.json'.format(step))
viewer.show()
_lmgc90_iterative.finalize_simulation()
