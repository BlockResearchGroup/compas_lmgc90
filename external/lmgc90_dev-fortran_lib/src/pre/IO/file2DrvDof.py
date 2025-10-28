import os

from ..avatar import avatar
from ..avatar.node import node

from .utils import read_line

LIST_PARAM = [ 'ct', 'amp', 'omega', 'phi', 'rampi', 'ramp' ]


def read_drv_dof(bodies, fpath):
    """
    Read a DRV_DOF.DAT file

    :param bodies: the avatar container in which to set the drvdofs read (modified in place)
    :param fpath: the directory path to the DRV_DOF.DAT file to read
    """

    #assert fpath.is_dir()
    assert os.path.isdir(fpath)

    #fname = fpath/'BODIES.DAT'
    #assert  fname.is_file()
    if 'DAT' in fpath:
        ext = '.DAT'
    else:
        ext = '.OUT'
    fname = os.path.join(fpath,'DRV_DOF'+ext)
    assert  os.path.isfile(fname)

    print()
    print('Start reading file\t:\t'+fname)

    #remap (avatar_type,avatar_number) to index bodies in container
    #single numbering of all MAILx bodies
    avatar_index = { (av.atype,av.number,):idx for idx,av in enumerate(bodies) }

    with open(fname,'r') as fid:

        atype  = None
        number = None
        no_id  = None
        mtype  = None

        line = read_line(fid)
        while line:

           if line.startswith('$bdyty') :
               line = read_line(fid)
               atype  = line[1:6]
               number = int( line[6:15] )
               if atype[:4] == 'RBDY':
                   mtype = 'MECAx'

               # avatar identification
               av_id  = avatar_index[ (atype,number,) ]
               # assert  that this avatar exists ?
               assert isinstance(bodies[av_id], avatar.avatar), 'avatar not found'
               assert bodies[av_id].atype == atype, 'avatar of wrong type'

           elif line.startswith('$model') :
               line = read_line(fid)
               mtype = line[1:6]
               # so beurgly !!!
               if mtype == 'THERM':
                   mtype = 'THERx'

           elif line[:6] == '$nodty' :

               assert bodies[av_id].modelType == mtype, 'wrong model type'

               line = read_line(fid)
               no_type = line[1:6]
               no_id   = int( line[6:15] ) 
               assert isinstance(bodies[av_id].nodes[no_id], node.node), 'node not found in avatar'


           elif line[:6] == '$dofty' :
               line = read_line(fid)

               #while line[:6] not in {'$$$$$$', '$bdyty', '$nodty', '$dofty'}:
               while line[0] != '$' :
                   drv_type = line[1:6].rstrip()
                   dof_id   = int(line[6:13])
                   param = { 'dofty'    :drv_type,
                             'component':dof_id   }
                   if line[14:23] == 'evolution':
                       param['description']    = 'evolution'
                       param['evolutionFile'] = line[24:-1].strip()
                   else:
                       param['description'] = 'predefined'
                       vals = [ float( v.replace('D','E') ) for v in line[14:-1].split() ]
                       for k, v in zip( LIST_PARAM, vals ):
                           param[k] = v

                   bodies[av_id].nodes[no_id].imposeDrivenDof(**param)
                   bodies[av_id].drvDof = True
                   line = read_line(fid)

           else:
               line = read_line(fid)

    print('End reading file\t:\t'+fname)

