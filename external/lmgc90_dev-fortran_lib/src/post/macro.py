from pathlib import Path

from ..chipy.vtk_display import startCollection

from . import central_kernel

def OpenCentralKernelFiles(restart=1, path='./'):
    """ Initialize visualization file writing.

    Parameters
    ----------
    restart : integer
              First index of file to write
    path : Path
           directory in which to write files
    """

    global wd, wdf, fck

    wd  = path/Path('DISPLAY')
    wd.mkdir(parents=True, exist_ok=True)

    wdf = restart-1

    fck = startCollection(wd/'ck.pvd',wdf)


def WriteCentralKernelFiles(time, f2f, inters):
    """Write current central kernel paraview file.

    Parameters
    ----------
    time : float
           Simulation time corresponding to the file to write
    f2f : integer array
          The face to face structure as a flatten list of integers
    inters : array
          The interactions array
    """
    global wdf, fck

    wdf += 1

    fname = wd/f"ck_{str(wdf)}.vtmb"

    polyg, ck = central_kernel.get( f2f, inters )
    central_kernel.write_vtk(time, fname, fck, polyg, ck)


