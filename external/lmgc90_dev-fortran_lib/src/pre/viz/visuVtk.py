import numpy as np
from .vtkContactor import *

wedgeMap = list(range(15))
wedgeMap[1] = 2; wedgeMap[2]  = 1
wedgeMap[4] = 5; wedgeMap[5]  = 4
wedgeMap[6] = 8; wedgeMap[8]  = 6
wedgeMap[9] =11; wedgeMap[11] = 9
wedgeMap[13]=14; wedgeMap[14] =13

def getVtkObjectsFromAvatar(avatar):
  """getVtkObjectFromAvatar(avatar):

     Create the list of vtk objects corresponding to an avatar.
     Object from the nodes of the mesh or the center of inertia
     for the body and objects from its contactors for a rigid.

     parameter:

     - avatar: avatar object to display
     - returned value: list of vtk objects to use in input in a vtkMapper
  """

  import vtk

  vtk_objs = []

  dim = avatar.dimension

  # putting nodes in vtk
  points = vtk.vtkPoints()
  if dim == 3:
    for k in sorted(avatar.nodes.keys()):
      points.InsertNextPoint(avatar.getNodeCoor(k))
  else:
    for k in sorted(avatar.nodes.keys()):
      coor = avatar.getNodeCoor(k)
      points.InsertNextPoint(coor[0], coor[1], 0.)

  grid = vtk.vtkUnstructuredGrid()
  grid.Allocate(1,1)

  # putting elements in vtk
  for ele in avatar.bulks:
    obj = None
    if ele.etype == 'Q4xxx':
      obj = vtk.vtkQuad()
    elif ele.etype == 'Q8xxx':
      obj = vtk.vtkQuadraticQuad()
    elif ele.etype == 'T3xxx':
      obj = vtk.vtkTriangle()
    elif ele.etype == 'T6xxx':
      obj = vtk.vtkQuadraticTriangle()
    elif ele.etype == 'S2xxx':
      obj = vtk.vtkLine()
    elif ele.etype == 'Point':
      obj = vtk.vtkVertex()
    elif ele.etype == 'H20xx':
      obj = vtk.vtkQuadraticHexahedron()
    elif ele.etype == 'H8xxx':
      obj = vtk.vtkHexahedron()
    elif ele.etype == 'PRI6x':
      obj = vtk.vtkWedge()
    elif ele.etype == 'PRI15':
      obj = vtk.vtkQuadraticWedge()
    elif ele.etype == 'TE10x':
      obj = vtk.vtkQuadraticTetra()
    elif ele.etype == 'TE4xx':
      obj = vtk.vtkTetra()
    else:
      print('[getVtkObjectsFromAvatar] unable to draw element of type : ', ele.etype)

    if obj != None:
      if ele.etype == 'PRI6x' or ele.etype == 'PRI15':
        for k in range(len(ele.connectivity)):
          obj.GetPointIds().SetId(k, ele.connectivity[wedgeMap[k]]-1)
      else:
        for k in range(len(ele.connectivity)):
          obj.GetPointIds().SetId(k, ele.connectivity[k]-1)
      grid.InsertNextCell(obj.GetCellType(),obj.GetPointIds())

  grid.SetPoints(points)
  vtk_objs.append(grid)

  # to improve !!!
  if avatar.atype == 'MAILx':
    return vtk_objs

  coor = avatar.getNodeCoor()
  frame= avatar.getBulkFrame()

  # drawing contactors
  for contactor in avatar.contactors:
    if contactor.shape in availables:
      obj = eval("getVtkObjectFrom"+contactor.shape+"(contactor, coor, frame)")
      if isinstance(obj,list):
        vtk_objs.extend(obj)
      else:
        vtk_objs.append(obj)
    else:
      print('WARNING: Unable to create vtk object of contactor type: ', contactor.shape, \
      'for avatar number: ', avatar.number)

  return vtk_objs
      

def add_actors(cont, ren, drvdof_color):

  import vtk

  v = vtk.vtkVersion()
  if v.GetVTKVersion().split('.')[0] > '5':

    vtk.vtkLogger.SetStderrVerbosity(vtk.vtkLogger.VERBOSITY_OFF)

    for avatar in cont:
      vtk_objs = getVtkObjectsFromAvatar(avatar)
      for obj in vtk_objs:

        mapper = vtk.vtkDataSetMapper()
        mapper.SetScalarVisibility(0)

        if isinstance(obj,vtk.vtkAlgorithm) :
          mapper.SetInputConnection(obj.GetOutputPort())
        else:
          mapper.SetInputData(obj)

        actor   = vtk.vtkActor()
        actor.SetMapper(mapper)
        ren.AddActor(actor)
  
        # change color of object if any dof has a BC
        if drvdof_color is not None:
          for n in avatar.nodes:
            if n.dof is not None:
              if any(n.dof.pilote):
                actor.GetProperty().SetColor(drvdof_color[:])
                break

        # for edges visualization in blue
        edges = vtk.vtkExtractEdges()
        if isinstance(obj,vtk.vtkAlgorithm) :
          edges.SetInputConnection(obj.GetOutputPort())
        else:
          edges.SetInputData(obj)
  
        edge_mapper = vtk.vtkPolyDataMapper()
        edge_mapper.SetInputConnection(edges.GetOutputPort())
        edge_actor = vtk.vtkActor()
        edge_actor.SetMapper(edge_mapper)
        edge_actor.GetProperty().SetColor(0,0,1)
        ren.AddActor(edge_actor)

  else:
    for avatar in cont:
      vtk_objs = getVtkObjectsFromAvatar(avatar)
      for obj in vtk_objs:
        mapper  = vtk.vtkDataSetMapper()
        if isinstance(obj,vtk.vtkAlgorithm) :
          mapper.SetInput(obj.GetOutput())
        else:
          mapper.SetInput(obj)
        #mapper.SetInput(obj)
        actor   = vtk.vtkActor()
        actor.SetMapper(mapper)
        ren.AddActor(actor)
  
        # for edges visualization in blue
        edges = vtk.vtkExtractEdges()
        if isinstance(obj,vtk.vtkAlgorithm) :
          edges.SetInput(obj.GetOutput())
        else:
          edges.SetInput(obj)
        edge_mapper = vtk.vtkPolyDataMapper()
        edge_mapper.SetInputConnection(edges.GetOutputPort())
        edge_actor = vtk.vtkActor()
        edge_actor.SetMapper(edge_mapper)
        edge_actor.GetProperty().SetColor(0,0,1)
        ren.AddActor(edge_actor)

      
def visuAvatars(cont,with_axis=False,drvdof_color=None):
  """visuAvatars(cont):

     Create a visualization window of a container of avatars using vtk.

     parameter:

     - cont: container of avatars
     - with_axis (optional) : (boolean) add normalized axis to visualization
     - drvdof_color (optional) : ignored if None, otherwise must be a list of 3 values to define the color of bodies having a boundary condition
  """

  from .. import config

  # In novisu mode, don't popup any window
  if ( config.novisu ):
    return

  try:
      import vtk
  except ImportError:
      msg  = '[WARNING:pre.visuAvatars] not python vtk module found,\n'
      msg += 'please install it before trying to visualize avatars'
      print(msg)
      return

  ren    = vtk.vtkRenderer()
  renWin = vtk.vtkRenderWindow()
  renWin.AddRenderer(ren)
  renWin.SetSize(800, 600)

  style =  vtk.vtkInteractorStyleTrackballCamera()

  iren = vtk.vtkRenderWindowInteractor()
  iren.SetRenderWindow(renWin)
  iren.SetInteractorStyle(style)

  #ren.ResetCamera()
  #ren.GetActiveCamera().Azimuth(30)
  #ren.GetActiveCamera().Elevation(20)
  #ren.GetActiveCamera().Dolly(2.8)
  #ren.ResetCameraClippingRange()
  ren.SetBackground(.1, .2, .4)

  add_actors(cont,ren,drvdof_color)

  if with_axis:
    axes = vtk.vtkAxesActor()
    ren.AddActor(axes)

  # for edges visualization
  vtk.vtkPolyDataMapper().SetResolveCoincidentTopologyToPolygonOffset()

  iren.Initialize()
  renWin.Render()
  iren.Start()

  #too violent... may close automatically the window in Anaconda notebook
  #iren.GetRenderWindow().Finalize()  # equivalent: renWin.Finalize()
  #iren.TerminateApp()
  #del renWin, iren

