
from . import config
from .lmgc90 import utilities_logMes

try :

  import vtk
  config.is_vtk_display = True

  v = vtk.vtkVersion()
  config.vtkVersion = tuple( map( int, v.GetVTKVersion().split('.') ) )

except ImportError :
  utilities_logMes("WARNING : vtk display not available")
  utilities_logMes("          to activate it install python 'vtk' package")
  config.is_vtk_display = False

if config.is_vtk_display:

  from .vtk_display import *

