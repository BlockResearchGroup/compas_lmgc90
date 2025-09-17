
import os, sys

import math, numpy as np

from copy import deepcopy

from pylmgc90 import pre

bodies = None
mods   = None
mats   = None
tacts  = None
svs    = None

def init(dist):
  global bodies, mods, mats, tacts, svs

  dim = 3

  pre.setStopMode('exception')

  bodies = pre.avatars()
  mods   = pre.models()
  mats   = pre.materials()
  tacts  = pre.tact_behavs()
  svs    = pre.see_tables()

  mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
  mat = pre.material(name='TDURx', materialType='RIGID', density=1000.)
  mats.addMaterial(mat)

  iqsc0 = pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.5)
  tacts.addBehav(iqsc0)
  
  sv1 = pre.see_table(CorpsCandidat   ='RBDY3', candidat   ='POLYR', colorCandidat   ='POLYR', behav=iqsc0,
                      CorpsAntagoniste='RBDY3', antagoniste='POLYR', colorAntagoniste='POLYR', alert=dist*1.1)
  svs.addSeeTable(sv1)

  return mat, mod

def cube(radius, mat, mod):

  vertices = np.array([ [-radius,  -radius,  -radius],
                        [ radius,  -radius,  -radius],
                        [ radius,   radius,  -radius],
                        [-radius,   radius,  -radius],
                        [-radius,  -radius,   radius],
                        [ radius,  -radius,   radius],
                        [ radius,   radius,   radius],
                        [-radius,   radius,   radius] ])

  cube = pre.rigidPolyhedron(model=mod, material=mat, generation_type='vertices', vertices=vertices , color='POLYR')

  return cube

def diamond(radius, mat, mod):

  vertices = np.array([ [     0.,      0., -radius],
                        [-radius, -radius,      0.],
                        [-radius,  radius,      0.],
                        [ radius, -radius,      0.],
                        [ radius,  radius,      0.],
                        [     0.,      0.,  radius] ])

  diam = pre.rigidPolyhedron(model=mod, material=mat, generation_type='vertices', vertices=vertices , color='POLYR')

  return diam

def cube(radius, mat, mod):

  vertices = np.array([ [-radius,  -radius,  -radius],
                        [ radius,  -radius,  -radius],
                        [ radius,   radius,  -radius],
                        [-radius,   radius,  -radius],
                        [-radius,  -radius,   radius],
                        [ radius,  -radius,   radius],
                        [ radius,   radius,   radius],
                        [-radius,   radius,   radius] ])

  cube = pre.rigidPolyhedron(model=mod, material=mat, generation_type='vertices', vertices=vertices , color='POLYR')

  return cube

def ushaped(radius, hole, mat, mod):

  vertices = np.array([
                        [     0.,  -radius,  -radius],
                        [ radius,  -radius,  -radius],
                        [ radius,   radius,  -radius],
                        [ hole  ,   radius,  -radius],
                        [ hole  ,  -hole  ,  -radius],
                        [-hole  ,  -hole  ,  -radius],
                        [-hole  ,   radius,  -radius],
                        [-radius,   radius,  -radius],
                        [-radius,  -radius,  -radius],
                        [     0.,  -radius,   radius],
                        [ radius,  -radius,   radius],
                        [ radius,   radius,   radius],
                        [ hole  ,   radius,   radius],
                        [ hole  ,  -hole  ,   radius],
                        [-hole  ,  -hole  ,   radius],
                        [-hole  ,   radius,   radius],
                        [-radius,   radius,   radius],
                        [-radius,  -radius,   radius],
                      ] )

  faces = np.empty( [32,3], dtype=int )
  tri1  = np.array([ 2, 3,11])
  tri2  = np.array([11, 3,12])
  for i in range(7):
    faces[2*i  ,:] = tri1 + i
    faces[2*i+1,:] = tri2 + i
  faces[14,:] = [1, 2,11]
  faces[15,:] = [1,11,10]
  faces[16,:] = [1,10,18]
  faces[17,:] = [1,18, 9]
  #
  faces[18,:] = [ 8, 7, 6]
  faces[19,:] = [ 8, 6, 9]
  faces[20,:] = [ 1, 9, 6]
  faces[21,:] = [ 1, 6, 5]
  faces[22,:] = [ 1, 5, 2]
  faces[23,:] = [ 2, 5, 3]
  faces[24,:] = [ 3, 5, 4]
  faces[25:33,:] = faces[18:25,::-1] + 9
  ushaped = pre.rigidPolyhedron(model=mod, material=mat, generation_type='full', vertices=vertices , faces=faces, color='POLYR')

  return ushaped

def write(path=None):
  global bodies, mods, mats, tacts, svs

  dim = 3

  if path is None:
    path = os.getcwd()

  chemin = os.path.join(path,'DATBOX')

  if( not os.path.isdir(chemin) ):
     os.makedirs(chemin)

  pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, datbox_path=chemin)


# generate an example
def test(ray, dist, nb=1):
  global bodies

  assert (nb > 0 and nb < 11), "unknown test number"

  # create everything
  mat, mod = init(dist)

  # depending on the test choose the fixed body
  body_c = cube(ray, mat, mod)
  body_d = diamond(ray, mat, mod)
  body_u = ushaped(ray, ray/3., mat, mod)

  # two identical faces
  if nb == 1:
    path  = 'face_face_1'
    body1 = deepcopy(body_c)
    body2 = deepcopy(body1)
    body2.translate(dz=2.*ray+dist)
  # two faces slightly shifted
  elif nb == 2 :
    path  = 'face_face_2'
    body1 = deepcopy(body_c)
    body2 = deepcopy(body1)
    body2.translate(dx=-ray/2., dy=-ray/2., dz=2.*ray+dist)
  # an edge on a face
  elif  nb == 3 :
    path  = 'edge_face_1'
    body1 = deepcopy(body_c)
    body2 = deepcopy(body1)
    body2.rotate('axis', axis=[1.,0.,0.],alpha=math.pi/4.)
    body2.translate(dz=ray+ray*math.sqrt(2.)+dist)
  # an vertices on a face
  elif  nb == 4 :
    path = 'vert_face_1'
    body1 = deepcopy(body_c)
    body2 = diamond(ray, mat, mod)
    body2.translate(dz=2.*ray+dist)
  # two parallel edges
  elif  nb == 5 :
    path = 'edge_edge_1'
    body1 = deepcopy(body_c)
    body1.rotate('axis', axis=[1.,0.,0.], alpha=math.pi/4.)
    body2 = deepcopy(body1)
    body2.translate(dz=2.*ray*math.sqrt(2.)+dist)
  # two crossing edges
  elif  nb == 6 :
    path = 'edge_edge_2'
    body1 = deepcopy(body_c)
    body1.rotate('axis', axis=[1.,0.,0.], alpha=math.pi/4.)
    body2 = deepcopy(body1)
    body2.rotate('axis', axis=[0.,0.,1.], alpha=math.pi/2.)
    body2.translate(dz=2.*ray*math.sqrt(2.)+dist)
  # vertex on edge
  elif  nb == 7 :
    path = 'vert_edge_1'
    body1 = deepcopy(body_c)
    body1.rotate('axis', axis=[1.,0.,0.], alpha=math.pi/4.)
    body2 = diamond(ray, mat, mod)
    body2.translate(dz=ray+ray*math.sqrt(2.)+dist)
  # vertex on vertex
  elif  nb == 8 :
    path = 'vert_vert_1'
    body1 = deepcopy(body_d)
    body2 = deepcopy(body1)
    body2.translate(dz=2.*ray+dist)
  # two faces with intersection not as rectangular
  elif nb == 9 :
    path  = 'face_face_3'
    body1 = deepcopy(body_c)
    body2 = deepcopy(body1)
    body2.rotate(phi=1., center=body2.getNodeCoor())
    body2.translate(dx=-ray/8., dy=-ray/8., dz=2.*ray+dist)
  # two faces with intersection giving two polygons
  elif nb == 10 :
    path  = 'face_face_4'
    body1 = deepcopy(body_c)
    body2 = deepcopy(body_u)
    body2.translate(dy=-ray, dz=2.*ray+dist)


  body1.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')

  bodies.addAvatar(body1)
  bodies.addAvatar(body2)

  if '--with-visu' in sys.argv:
    try:
      pre.visuAvatars(bodies, True)
    except:
      pass

  # ecriture des fichiers de donnees pour LMGC90
  write(path)

  return path
