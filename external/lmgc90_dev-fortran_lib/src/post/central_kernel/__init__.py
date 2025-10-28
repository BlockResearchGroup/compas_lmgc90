
from ...chipy.config import is_vtk_display

from .macro import get


if is_vtk_display:
    from .write_vtk import addCkToVtkBlocks
