import os
import collections
import numpy as np

from ..config import lmgc90dicts
from ..avatar import avatar

from .utils import str2float

def read_line(fid):
    """
    Read a line from DOF.INI/OUT file.

    :param fid: file object from which to read
 
    :return: the line read as a string or None if end of file
    """

    line = fid.readline()

    # checking end of file
    if not line:
        return None

    # if comment or empty line reading next
    while line[0] == "!" or not line.strip() :
        line = fid.readline()
        if not line:
            return line

    # managing next block
    if line.startswith("$$$$$$") :
        return read_line(fid)

    return line


def get_list(line):
    """
    Read a line as a list of 3 floats

    :param line: the string line to interpret

    :return: the float read in a list
    """
    return [ str2float(line[idx:idx+14]) for idx in (34,55,76,) if line[idx:idx+14].strip() ]


def read_dofs(bodies, fpath, step=0 ):
    """
    Read a DOF.INI, DOF.OUT.step or DOF.LAST file

    :param bodies: the container of avatars to initialize (modified in place)
    :param fpath: the path to DATBOX or OUTBOX if step is not 0
    :param step: (optional) a step number of DOF.OUT file to read instead of DOF.INI.
                 If -1, read the DOF.LAST file.

    :return: - the step number read in header
             - the time read in header
    """

    if step:
      if step == -1:
          fread = 'DOF.LAST'
      else :
          fread = 'DOF.OUT.'+str(step)
    else:
      fread = 'DOF.INI'

    #assert fpath.is_dir()
    assert os.path.isdir(fpath)

    #fname = fpath/'BODIES.DAT'
    #assert  fname.is_file()
    fname = os.path.join(fpath,fread)
    assert  os.path.isfile(fname)

    print()
    print('Start reading file\t:\t'+fname)

    #remap (avatar_type,avatar_modelType,number) to index bodies in container
    #numbering for each physics/modelType pair

    avatar_index = { (av.atype,av.modelType,av.m_num):idx for idx,av in enumerate(bodies) }

    with open(fname,'r') as fid:

        line = read_line(fid)

        assert line[:7] != "$steps", "ERROR: should be reading 'steps' keyword"

        nstep =   int( line[ 7:16] )
        ntime = float( line[35:49].replace('D','E') )

        line = read_line(fid)

        atype  = None
        number = None

        while line:

           if line.startswith('$bdyty') :

               line   = read_line(fid)
               atype  = line[1:6]
               number = int( line[6:15] )

               if atype[:4] == 'RBDY':
                   mtype = 'MECAx'

           elif line.startswith('$model') :

               line = read_line(fid)
               mtype= line[1:6]
               line = read_line(fid)

               # so beurgly !!!
               if mtype == 'THERM':
                   mtype = 'THERx'

           elif line.startswith('$nodty') :

               line = read_line(fid)

               av_id = avatar_index[ (atype,mtype,number,) ]
               # assert  that this avatar exists ?
               assert isinstance(bodies[av_id], avatar.avatar), 'avatar not found'
               assert bodies[av_id].atype == atype, 'avatar of wrong type'
               assert bodies[av_id].modelType == mtype, 'wrong model type'

               no_type = line[1:6]
               no_id   = int( line[6:15] )
               if atype == 'RBDY2':

                   assert no_type == 'NO3xx', 'ERROR reading wrong node type in DOF file'
                   assert no_id   == 1      , 'ERROR reading wrong node number in DOF file'

                   X = np.array( get_list(line), dtype=float )

                   # building axis frame from angle...
                   axis = np.empty( [2,2], dtype=float )
                   axis[0,0] = np.cos(X[2])
                   axis[1,0] = np.sin(X[2])
                   axis[0,1] =-axis[1,0]
                   axis[1,1] = axis[0,0]

                   line = read_line(fid)
                   V = np.array( get_list(line), dtype=float )

                   bodies[av_id].nodes[no_id].dof.disp[:] = X[:2]
                   bodies[av_id].nodes[no_id].dof.rot     = axis
                   #bodies[av_id].bulks[0].axis = axis
                   bodies[av_id].nodes[no_id].dof.values = V
                   bodies[av_id].iniDof = True

               elif atype == 'RBDY3':
                   assert no_type == 'NO6xx', 'ERROR reading wrong node type in DOF file'
                   assert no_id   == 1      , 'ERROR reading wrong node number in DOF file'

                   X = get_list(line)
                   line = read_line(fid)
                   X.extend( get_list(line) )

                   X = np.array( X , dtype=float )

                   line = read_line(fid)
                   V = get_list(line)
                   line = read_line(fid)
                   V.extend( get_list(line) )
                   V = np.array( V, dtype=float )

                   axis = []
                   for i in range(3):
                       line = read_line(fid)
                       axis.extend( get_list(line) )
                   axis = np.array( axis, dtype=float )
                   axis.shape=[3,3]

                   bodies[av_id].nodes[no_id].dof.disp[:] = X[:3]
                   bodies[av_id].nodes[no_id].dof.rot     = axis.T
                   bodies[av_id].nodes[no_id].dof.values  = V
                   bodies[av_id].iniDof = True

               else: # atype == 'MAILx'

                   while line[1:6] in lmgc90dicts.dimensionTypeNode.values():

                       no_type = line[1:6]
                       no_id   = int( line[6:15] )

                       if mtype == 'THERx':

                           assert int(no_type[2]) == 1

                           T = get_list(line)
                           T = np.array( T , dtype=float )
                           bodies[av_id].nodes[no_id].dof.values = T
                           bodies[av_id].iniDof = True

                           # attempting to read a new node
                           line = read_line(fid)

                       # mecax, porox, multi
                       else:

                           X = get_list(line)
                           if int(no_type[2]) > 3:
                               line = read_line(fid)
                               X.extend( get_list(line) )

                           #do not work with poro...
                           #assert len(X) == bodies[av_id].dimension
                           X = np.array( X, dtype=float )

                           line = read_line(fid)
                           V = get_list(line)
                           if int(no_type[2]) > 3:
                               line = read_line(fid)
                               V.extend( get_list(line) )
                           V = np.array( V, dtype=float )

                           # sooooooo wrong !!!
                           if mtype == 'POROx':
                               bodies[av_id].nodes[no_id].dof.ntype = no_type
                           else:
                               assert bodies[av_id].nodes[no_id].dof.ntype == no_type

                           bodies[av_id].nodes[no_id].dof.disp = X
                           bodies[av_id].nodes[no_id].dof.values = V
                           bodies[av_id].iniDof = True

                           # attempting to read a new node
                           line = read_line(fid)

           # going to next bdyty block
           elif line.startswith('$$$$$$') :
               line = read_line(fid)
               atype  = None
               number = None

           else:
               line = read_line(fid)

    print('End reading file\t:\t'+fname)

    return nstep, ntime


