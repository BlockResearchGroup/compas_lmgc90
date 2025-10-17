from compas_viewer import Viewer
from compas import json_load
from compas.geometry import Line
viewer = Viewer()


for i in range(10):
    group = viewer.scene.add_group('bodies_{}'.format(i*100))
    data = json_load('/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Sandbox/Compas/result_{}.json'.format(i*100))
    for body in data['bodies']:
        group.add(body['mesh'], facecolor=body['color'],hide_coplanaredges=True,opacity=0.5)
    for interaction in data['result']:
        group.add(interaction['line'], color=interaction['color'])
viewer.show()
