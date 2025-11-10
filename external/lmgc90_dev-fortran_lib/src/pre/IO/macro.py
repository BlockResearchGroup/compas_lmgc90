# no pathlib yet with python2.7
#from pathlib import Path
import os, sys
import numpy as np

from .file2BulkBehav import read_bulk_behav
from .file2Models    import read_models
                     
from .file2Bodies    import read_bodies
from .file2DrvDof    import read_drv_dof
from .file2Dofs      import read_dofs
from .file2Gpvs      import read_gpvs

from .file2TactBehav import read_tact_behav
from .file2VlocRloc  import read_vloc_rloc

from .hfile2state    import read_state_from_hfile

from .bulkBehav2File   import writeBulkBehav
from .model2File       import writeModels

from .bodies2File      import writeBodies
from .drvDof2File      import writeDrvDof
from .dofIni2File      import writeDofIni
from .gpvIni2File      import writeGPVIni

from .tactBehav2File   import writeTactBehav
from .vlocrlocIni2File import writeVlocRlocIni

from .postpro2File     import writePostpro

from . import utils

def readState(bodies, box_path="./DATBOX", step=0, hfile=None, tacts=None, with_graph=False, with_xl=False):
    """
    Read a a set of DOF, GPV and VlocRloc file to initialize an avatar container

    :param bodies: the avatar container to initialize, must not have been renumbered !
    :param box_path: (optional) the path where to read file (usually DATBOX or OUTBOX)
    :param step: (optional) if 0 (default value), read .INI files, if -1, read .LAST files,
                 else read .OUT.step fiels
    :param hfile: (optional) instead of reading from a .INI or .OUT file, read from an hdf5 file (tacts must be provided in this case).
    :param tacts: (optional) contact tact law containers must be provided with hfile.
    :param with_graph: (optional) ask to append an igraph object on output.
                       Value ignored if igraph module not available.
    :param with_xl: (optional) if using XL format for inters

    :returns: - inters : the interactions read in a numpy array (check dtype for content)
              - ginters : (optional) igraph.Graph object linking inters array with
                          the related avatar of input bodies. Only if with_graph is True.
    """

    if sys.version_info.major < 3 :
        print( "ERROR : reader is not available in python 2")
        raise RuntimeError

    if hfile is not None:
        inters = read_state_from_hfile(bodies, tacts, hfile, step)
    else:

        nstep_d, ntime_d = read_dofs(bodies, box_path, step)
        nstep_g, ntime_g = read_gpvs(bodies, box_path, step)

        if nstep_g:
            assert nstep_d == nstep_g
        if ntime_g:
            assert ntime_d == ntime_g

        dim = bodies[0].dimension
        inters, nstep_v, ntime_v = read_vloc_rloc(dim, box_path, step, with_xl)

        assert nstep_d == nstep_v
        assert ntime_d == ntime_v

    if with_graph:

        ginters = utils.generate_graph(bodies, inters)

        return inters, ginters

    return inters
 

def readDatbox(dim, datbox_path="./DATBOX", step=0, hfile=None, renumber=False, with_graph=False, with_xl=False):
    """
    Read the files of a DATBOX directory to create the pre containers
    reprensenting it.

    If there is not MODELS.DAT file because there are only rigids,
    a rigid models is still created and added. If the GPV.INI or
    VlocRloc.INI file do not exists, their reading is just skipped.

    :param dim: integer with the dimension of data to read
    :param datbox_path: the string of the DATBOX directory to read
    :param step: (optional) step file to read if reading from OUTBOX (will read .INI file by default)
    :param hfile: (optional) instead of reading from a .INI or .OUT file, read from an hdf5 file.
    :param renumber: (optional) boolean forcing renumbering of bodies after reading... break the 'readState' calls.
    :param with_graph: (optional) boolean asking to generate a graph of interaction (needs igraph module).
    :param with_xl: (optional) if using XL format for inters

    :returns: - mats : the materials container
              - mods : the models container
              - bodies : the avatar container
              - tacts : the tact_behav container
              - sees : the visibilit table container
              - inters : the interaction numpy array (check its dtype for content)
              - ginters : (optional) the igraph.Graph object of interaction if 'with_graph' is True
    """

    if sys.version_info.major < 3 :
        print( "ERROR : reader is not available in python 2")
        raise RuntimeError

    inters = None

    #if not isinstance(datbox_path, Path):
    #    datbox_path = Path(datbox_path)

    mats, gravy = read_bulk_behav(dim, datbox_path)
    mods = read_models(dim, datbox_path)

    bodies = read_bodies(dim, mats, mods, datbox_path)
    if renumber:
        bodies.renumber()
    read_drv_dof(bodies, datbox_path)

    tacts, sees = read_tact_behav(datbox_path)

    inters = readState(bodies, datbox_path, step, hfile, tacts, with_graph, with_xl)
    if with_graph:
      # then inters is (inters,ginters)
      #return mats, mods, bodies, tacts, sees, *inters
      # for python 3.6
      return mats, mods, bodies, tacts, sees, inters[0], inters[1]
    else:
      return mats, mods, bodies, tacts, sees, inters


def writeDatbox(dim, mats, mods, bodies, tacts=None, sees=None, inters=None, post=[], datbox_path='DATBOX', gravy=None, with_xl=False) :
    """
    Write all containers in correct files in provided DATBOX directory

    :param dim: the dimension of the avatars container
    :param mats: the material container
    :param mods: the model container
    :param bodies: the avatar container
    :param tacts: (optional) the contact law container
    :param sees: (optional) the visibility table container
    :param inters: (optional) the numpy array of interactions
    :param post: (optional) the postpro commands container
    :param datbox_path: (optional) the directory in which to write the files (default to DATBOX)
    :param gravy: (optional) the gravity vector to write in BULK_BEHAV.DAT
    :param with_xl: (optional) if using XL format
    """

    if not os.path.isdir( datbox_path ):
        os.mkdir( datbox_path )

    writeBulkBehav(mats, chemin=datbox_path, dim=dim, gravy=gravy)

    writeBodies(bodies, chemin=datbox_path)
    writeDrvDof(bodies, chemin=datbox_path)
    writeDofIni(bodies, chemin=datbox_path)

    if len( bodies.getFemAvatar() ) > 0:
        writeModels(mods,chemin=datbox_path)
        writeGPVIni(bodies, chemin=datbox_path)

    if tacts is not None and sees is not None:
        writeTactBehav(tacts, sees, chemin=datbox_path)
        writeVlocRlocIni(datbox_path, inters, tacts, with_xl)

    writePostpro(post, bodies, datbox_path)

