from .vtkContactorUtils import *

# list of available contactors
availables = [ 'DISKx', 'JONCx', 'POLYG', 'POLYF', 'xKSID',
               'SPHER', 'CYLND', 'DNLYC', 'PLANx', 'POLYR', 'POLYO']
# todo...
# missing = ['xKSID' 'PTD2x',
#            'PT3Dx']

def getVtkObjectFromDISKx(tactor, coor, frame):
  """getVtkObjectFromDISKx(tactor, coor, frame)

  Get a vtk object describing discretized geometry of a DISKx contactor

  parameters:

  - tactor: a DISKx contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - returned value: a vtk object to use as input for a vtkMapper
  """

  import vtk

  shift, frame = getShift(tactor, coor, frame)

  obj = vtk.vtkRegularPolygonSource()
  obj.SetRadius(tactor.getOption('byrd'))
  obj.SetCenter( shift[0], shift[1], 0. )
  # \todo: number of sides should depend on radius and a resolution parameter
  obj.SetNumberOfSides(50)

  return obj

def getVtkObjectFromxKSID(tactor, coor, frame):
  """getVtkObjectFromxKSID(tactor, coor, frame)

  Get a vtk object describing discretized geometry of a xKSID contactor

  parameters:

  - tactor: a xKSID contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - returned value: a vtk object to use as input for a vtkMapper
  """

  import vtk

  shift, frame = getShift(tactor, coor, frame)

  obj = vtk.vtkRegularPolygonSource()
  obj.GeneratePolygonOff()
  obj.SetRadius(tactor.getOption('byrd'))
  obj.SetCenter( shift[0], shift[1], 0. )
  # \todo: number of sides should depend on radius and a resolution parameter
  obj.SetNumberOfSides(50)

  return obj

def getVtkObjectFromJONCx(tactor, coor, frame):
  """getVtkObjectFromJONCx(tactor, coor, frame)

  Get a vtk object describing discretized geometry of a JONCx contactor

  parameters:

  - tactor: a JONCx contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - returned value: an unstructured grid
  """

  vertices, cells = getPointsCellsJoncx(tactor, coor, frame)
  grid = getVtkObjFromPointsCells(vertices, cells)

  return grid

def getVtkObjectFromPOLYG(tactor, coor, frame):
  """getVtkObjectFromPOLYG(tactor, coor, frame)

  Get a vtk object describing the geometry of a POLYG contactor

  parameters:

  - tactor: a POLYG contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - returned value: an unstructured grid
  """

  vertices, cells = getPointsCellsPolyg(tactor, coor, frame)
  grid = getVtkObjFromPointsCells(vertices, cells)

  return grid

def getVtkObjectFromSPHER(tactor, coor, frame):
  """getVtkObjectFromSPHER(tactor, coor, frame)

  Get a vtk object describing discretized geometry of a SPHER contactor

  parameters:

  - tactor: a SPHER contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - returned value: a vtk object to use as input for a vtkMapper
  """

  import vtk

  obj = vtk.vtkSphereSource()
  obj.SetRadius(tactor.getOption('byrd'))

  shift, frame = getShift(tactor, coor, frame)
  obj.SetCenter( shift )

  return obj

def getVtkObjectFromCYLND(tactor, coor, frame, capping=True):
  """getVtkObjectFromCYLND(tactor, coor, frame, capping=True)

  Get a vtk object describing discretized geometry of a CYLND contactor

  parameters:

  - tactor: a CYLND contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - capping: (optional) if set to false generate a DNLYC instead of a CYLND
  - returned value: a vtk object to use as input for a vtkMapper
  """

  import vtk

  cyl = vtk.vtkCylinderSource()
  cyl.SetResolution(12)
  cyl.SetRadius(tactor.getOption('byrd'))
  cyl.SetHeight(2*tactor.getOption('High'))
  shift, frame = getShift(tactor, coor, frame)

  cyl.CappingOff()
  obj = [cyl]
  if capping:
    # adding half spheres...
    for p in [1, -1]:

      t = vtk.vtkTransform()
      t.PostMultiply()
      t.RotateX(90)
      t.Translate(0., p*tactor.getOption('High'), 0.)

      s = vtk.vtkSphereSource()
      if p > 0:
          s.SetStartPhi(90)
      else:
          s.SetEndPhi(90)
      s.SetThetaResolution(12)
      s.SetPhiResolution(12//2)
      s.LatLongTessellationOn()
      s.SetRadius(tactor.getOption('byrd'))

      rf = vtk.vtkTransformPolyDataFilter()
      rf.SetTransform(t)
      rf.SetInputConnection(s.GetOutputPort())

      obj.append(rf)

  # cylinder source generate along y-axis so frame is first oriented along z-axis
  r = vtk.vtkTransform()

  # very important...
  r.PostMultiply()

  new_frame = numpy.eye(4)
  new_frame[0:3,0] = [1., 0., 0.]
  new_frame[0:3,1] = [0., 0.,-1.]
  new_frame[0:3,2] = [0., 1., 0.]
  new_frame.shape  = [16]

  r.SetMatrix(new_frame)

  # then updating position and rotation
  new_frame = numpy.eye(4)
  new_frame[0:3,0:3] = frame[:,:]
  new_frame[0:3,3] = shift[:]
  new_frame.shape  = [16]

  r.Concatenate(new_frame)
  rfs = []
  for o in obj:
      rf= vtk.vtkTransformPolyDataFilter()
      rf.SetTransform(r)
      rf.SetInputConnection(o.GetOutputPort())
      rfs.append(rf)
  return rfs

def getVtkObjectFromDNLYC(tactor, coor, frame):
  """getVtkObjectFromDNLYC(tactor, coor, frame)

  Get a vtk object describing discretized geometry of a DNLYC contactor

  parameters:

  - tactor: a DNLYC contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - returned value: a vtk object to use as input for a vtkMapper
  """
  rf = getVtkObjectFromCYLND(tactor,coor,frame,False)

  return rf

def getVtkObjectFromPLANx(tactor, coor, frame):
  """getVtkObjectFromPLANx(tactor, coor, frame)

  Get a vtk object describing the geometry of a PLANx contactor

  parameters:

  - tactor: a PLANx contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - returned value: an unstructured grid
  """

  vertices, cells = getPointsCellsPlanx(tactor, coor, frame)
  grid = getVtkObjFromPointsCells(vertices, cells)

  return grid

def getVtkObjectFromPOLYR(tactor, coor, frame):
  """getVtkObjectFromPOLYR(tactor, coor, frame)

  Get a vtk object describing the geometry of a POLYR contactor

  parameters:

  - tactor: a POLYR contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - returned value: an unstructured grid
  """

  vertices, cells = getPointsCellsPolyr(tactor, coor, frame)
  grid = getVtkObjFromPointsCells(vertices, cells)

  return grid

def getVtkObjectFromPOLYF(tactor, coor, frame):
  """getVtkObjectFromPOLYR(tactor, coor, frame)

  Get a vtk object describing the geometry of a POLYR contactor

  parameters:

  - tactor: a POLYR contactor object
  - coor: the coordinates of the body 'tactor' is attached to
  - frame: the frame of the body 'tactor' is attached to
  - returned value: an unstructured grid
  """

  import numpy as np
  import vtk

  grids = []

  for p in tactor.patches:

    points = vtk.vtkPoints()
    grid = vtk.vtkUnstructuredGrid()
    grid.Allocate(1,1)
    grid.SetPoints(points)
    for k in sorted(p.nodes.keys()):
      #print( 'node : ', k, ' ', p.nodes[k].coor )
      new_coor = coor + np.dot(frame, p.nodes[k].coor + tactor.shift[:] )
      points.InsertNextPoint( new_coor )
    for ele in p.bulks:
      #print( 'element : ', ele.connectivity)
      obj = vtk.vtkTriangle()
      for k in range(len(ele.connectivity)):
        #print( 'p ', k, ' ', ele.connectivity[k]-1)
        obj.GetPointIds().SetId(k, ele.connectivity[k]-1)
      grid.InsertNextCell(obj.GetCellType(),obj.GetPointIds())

    grids.append(grid)

  return grids

# ugly trick to use eval in viz.py
getVtkObjectFromPOLYO = getVtkObjectFromPOLYF

