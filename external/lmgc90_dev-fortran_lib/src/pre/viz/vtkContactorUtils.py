import math
import numpy

def getShift(tactor, coor, frame):
  """getShift(tactor, coor, frame)
  
  compute the shift and rotation to apply to the vertices of the geometry
  from the coordinates and frame of the supporting avatar and the shift
  and rotation matrix of the contactor (if any).

  - tactor: a contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - returned value: 

    - the shift in absolute frame
    - the cumulated rotation matrix
  """

  try: 
    f_loc = numpy.dot(frame, tactor.frame)
  except:
    f_loc = frame
  shift = coor + numpy.dot(frame, tactor.shift)
  return shift, f_loc


def getVtkObjFromPointsCells(vertices, cells):
  """getVtkObjectFromPointsCells(vertices, cells)

  Get an unstructured grid vtk object from a list of vertices
  and the connectivities.

  parameters:

  - vertices: 2D numpy array with 3D coordinates of the vertices
  - cells: list of connectivity of the faces
  - returned value: a vtk unstructured grid
  """
  import vtk

  points = vtk.vtkPoints()
  for vertex in vertices:
    points.InsertNextPoint(vertex)

  grid = vtk.vtkUnstructuredGrid()
  grid.Allocate(len(cells),1)
  for k in range(len(cells)):  
    nb_vertices = len(cells[k])
    obj = vtk.vtkPolygon()
    obj.GetPointIds().SetNumberOfIds(nb_vertices)
    for i in range(nb_vertices):
      obj.GetPointIds().SetId(i, cells[k][i]-1)
    grid.InsertNextCell(obj.GetCellType(),obj.GetPointIds())

  grid.SetPoints(points)
  
  return grid


def getPointsCellsJoncx(tactor, coor, frame):
  """ getPointsCellsJoncx(tactor, coor, frame)

  Get the list of vertices and the connectivity
  corresponding to a discretized JONCx contactor

  - tactor: a JONCx contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - returned value:

    - the coordinates of the vertices defining the discretized geometry in a 2D numpy array
    - the connectivity of the JONCx
  """

  nb  = 9 #todo: resolution depends on something
  dtt = math.pi/nb

  a1  = tactor.axes[0]
  a2  = tactor.axes[1]
  nb_vertices = 2*(nb+1)
  vertices    = numpy.zeros([nb_vertices,3],float)
  tt=0.
  for i in range(0,nb+1):   
     vertices[i,0]= (a1 + a2*math.sin(tt)) 
     vertices[i,1]=-a2*math.cos(tt)
     vertices[nb+1+i,0]=-1.*(a1 + a2*math.sin(tt)) 
     vertices[nb+1+i,1]= a2*math.cos(tt)
     tt+=dtt

  shift, frame = getShift(tactor, coor, frame)
  vertices[:,0:2] = numpy.dot(vertices[:,0:2],frame.transpose())

  vertices[:,0] += shift[0]
  vertices[:,1] += shift[1]

  cells = [ list(range(1,nb_vertices+1)) ]

  return vertices, cells

def getPointsCellsPolyg(tactor, coor, frame):
  """ getPointsCellsPolyg(tactor, coor, frame)

  Get the list of vertices and the connectivity
  corresponding to a POLYG contactor

  - tactor: a POLYG contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - returned value:

    - the coordinates of the vertices of the POLYG in a 2D numpy array
    - the connectivity of the POLYG
  """

  nb_vertices = tactor.vertices.shape[0]
  vertices = numpy.zeros([nb_vertices,3],float)

  shift, frame = getShift(tactor, coor, frame)
  vertices[:,0:2] = numpy.dot(tactor.vertices[:,0:2],frame.transpose())

  vertices[:,0] += shift[0]
  vertices[:,1] += shift[1]

  cells = [ list(range(1,nb_vertices+1)) ]

  return vertices, cells

def getPointsCellsPlanx(tactor, coor, frame):
  """ getPointsCellsPlanx(tactor, coor, frame)

  Get the list of vertices and the connectivity
  corresponding to a discretized PLANx contactor

  - tactor: a PLANx contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - returned value:

    - the coordinates of the vertices defining the discretized geometry in a 2D numpy array
    - the list of the connectivity of the faces defining the PLANx
  """

  vertices = numpy.zeros([8,3])
  vertices[0,0:3] = [-tactor.axes[0], -tactor.axes[1], -tactor.axes[2]]
  vertices[1,0:3] = [ tactor.axes[0], -tactor.axes[1], -tactor.axes[2]]
  vertices[2,0:3] = [ tactor.axes[0],  tactor.axes[1], -tactor.axes[2]]
  vertices[3,0:3] = [-tactor.axes[0],  tactor.axes[1], -tactor.axes[2]]
  vertices[4,0:3] = [-tactor.axes[0], -tactor.axes[1],  tactor.axes[2]]
  vertices[5,0:3] = [ tactor.axes[0], -tactor.axes[1],  tactor.axes[2]]
  vertices[6,0:3] = [ tactor.axes[0],  tactor.axes[1],  tactor.axes[2]]
  vertices[7,0:3] = [-tactor.axes[0],  tactor.axes[1],  tactor.axes[2]]

  shift, frame = getShift(tactor, coor, frame)
  vertices = numpy.dot(vertices,frame.transpose())

  vertices[:,0] += shift[0]
  vertices[:,1] += shift[1]
  vertices[:,2] += shift[2]

  cells = []
   
  cells.append([1,2,3,4])
  cells.append([5,6,7,8])
  cells.append([1,2,6,5])
  cells.append([3,4,8,7])
  cells.append([2,3,7,6])
  cells.append([4,1,5,8])

  return vertices, cells

def getPointsCellsPolyr(tactor, coor, frame):
  """ getPointsCellsPolyr(tactor, coor, frame)

  Get the list of vertices and the connectivities
  corresponding to a POLYR contactor

  - tactor: a POLYR contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - returned value:

    - the coordinates of the vertices of the POLYR in a 2D numpy array
    - the list of the connectivity of the faces defining the POLYR
  """

  shift, frame = getShift(tactor, coor, frame)
  vertices = numpy.dot(tactor.vertices,frame.transpose())

  vertices[:,0] += shift[0]
  vertices[:,1] += shift[1]
  vertices[:,2] += shift[2]

  cells = []
  for cell in tactor.connectivity:
    cells.append(cell)

  return vertices, cells

