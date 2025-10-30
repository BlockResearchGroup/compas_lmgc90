import os
import re
import itertools
import numpy as np

from ..avatar import avatar
from ..avatar.bulk import element
from ..avatar.bulk import bulk

from .utils import read_line, str2float

#
from ..utilities import check_compiled_modules

if check_compiled_modules.import_lmgc90():
    try:
        from ...chipy import lmgc90
    except:
        raise

def get_list(line):
    """
    Read a line as a list of float

    :param line: the string line read

    :return: the floats read in a list
    """

    # count if line is a mutltiple of 14 or 15
    # this depends on how old the file is...
    length = 14 if len(line[:-1].rstrip()) % 14 == 0 else 15
    nb = len(line[:-1].rstrip())//length
    return [ str2float(line[i*length:i*length+14]) for i in range(nb) ]


def read_gpvs(bodies, fpath, step=0 ):
    """
    Read a GPV.INI, GPV.OUT.step or GPV.LAST file

    :param bodies: the container of avatars to initialize (modified in place)
    :param fpath: the path to DATBOX or OUTBOX if step is not 0
    :param step: (optional) a step number of GPV.OUT file to read instead of GPV.INI
                 If -1, read the GPV.LAST file.

    :return: - the step number read in header
             - the time read in header
    """

    if step:
      if step == -1:
          fread = 'GPV.LAST'
      else :
          fread = 'GPV.OUT.'+str(step)
    else:
      fread = 'GPV.INI'

    #assert fpath.is_dir()
    assert os.path.isdir(fpath)

    #fname = fpath/'BODIES.DAT'
    #assert  fname.is_file()
    fname = os.path.join(fpath,fread)
    if not os.path.isfile(fname):
        print('Skip reading file\t:\t'+fname)
        return None, None

    print()
    print('Start reading file\t:\t'+fname)

    #remap (avatar_type,avatar_number) to index in bodies container
    avatar_index = { (av.atype,av.number,):idx for idx,av in enumerate(bodies) }

    lmgc90.overall_DIME( bodies[0].dimension, 1 )
    e, n = lmgc90.mecaMAILx_GetNbGpByElem()
    meca2gp = { k:(v,0,)   for k, v in zip(e,n) }
    e, n = lmgc90.therMAILx_GetNbGpByElem()
    ther2gp = { k:(0,v,)   for k, v in zip(e,n) }
    e, n1, n2 = lmgc90.poroMAILx_GetNbGpByElem()
    poro2gp = { k:(mv,tv,) for k, mv,tv in zip(e,n1,n2) }
    element2gpsize = { 'MECAx' : meca2gp, 'THERx' : ther2gp, 'POROx' : poro2gp }


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

                line = read_line(fid)
                atype  = line[1:6]
                number = int( line[6:15] )
                av_id  = avatar_index[ (atype,number,) ]

                bodies[av_id].iniGpv = True

                # assert that this avatar exists ?
                assert isinstance(bodies[av_id], avatar.avatar), 'avatar not found'
                assert bodies[av_id].atype == atype, 'avatar of wrong type'

                line = read_line(fid)

            if line.startswith('$blmty') :

                line = read_line(fid)
                etype  = line[1:6]
                number = int( line[6:15] )

                # assert that this bulk exists ?
                assert isinstance(bodies[av_id].bulks[number-1], element.element), 'element not found'
                assert bodies[av_id].bulks[number-1].etype == etype, 'element found of wrong type'

                line = read_line(fid)

            if line.startswith('$model') :

                mtype = bodies[av_id].modelType
                line  = read_line(fid)
                if line[1:6] == 'THERM':
                  lim = 5
                else:
                  lim = 6
                assert line[1:lim] == mtype[:lim-1], 'wrong model type'

                # read line by line and store all values and size:
                gp_values = []
                line = read_line(fid)
                while line[0] != '$':
                    gp_values.append( get_list(line) )
                    line = read_line(fid)

                ef = bodies[av_id].bulks[number-1].model.element
                m_gp, t_gp = element2gpsize[mtype][ ef ]

                # size numpy arrays according to physics
                if mtype == 'MECAx':

                    mgrad = np.empty( [m_gp,len(gp_values[0])], dtype=float )
                    mflux = np.empty( [m_gp,len(gp_values[1])], dtype=float )
                    if len(gp_values) // m_gp == 3 :
                        minte   = np.empty( [m_gp,len(gp_values[2])], dtype=float )
                        fields = itertools.chain.from_iterable( zip(mgrad, mflux, minte) )
                    elif len(gp_values) // m_gp == 2:
                        minte = None
                        fields = itertools.chain.from_iterable( zip(mgrad, mflux) )
                    else :
                        print( 'ERROR when reading MECAx GPV... too many lines...')
                        raise ValueError
 
                    temp = tgrad = tflux = tinte = None
                    for f, v in zip( fields, gp_values ):
                        f[:] = v

                elif mtype == 'THERx':
                    temp = np.empty( [t_gp,len(gp_values[0])], dtype=float )
                    tgrad= np.empty( [t_gp,len(gp_values[1])], dtype=float )
                    tflux= np.empty( [t_gp,len(gp_values[2])], dtype=float )
                    if len(gp_values) // t_gp == 4 :
                        tinte   = np.empty( [m_gp,len(gp_values[3])], dtype=float )
                        fields = itertools.chain.from_iterable( zip(temp, tgrad, tflux, tinte) )
                    elif len(gp_values) // t_gp == 3 :
                        tinte = None
                        fields = itertools.chain.from_iterable( zip(temp, tgrad, tflux) )
                    else :
                        print( 'ERROR when reading THERM GPV... too many (or too few) lines...')
                        raise ValueError
 
                    mgrad = mflux = minte = None
                    for f, v in zip( fields, gp_values ):
                        f[:] = v

                elif mtype == 'POROx':

                    # try to guess if internal is to be read
                    # for meca and/or ther by counting the number of line
                    # that should be read in each case... 
                    # ... there will be a problem when the number of gp
                    #     for ther and meca will be the same.
                    s2323 = np.array( [2, 3, 2, 3] )
                    s2233 = np.array( [2, 2, 3, 3] )
                    combi = s2323*m_gp+s2233*t_gp
                    idx   = np.where( combi == len(gp_values) )
                    assert len( idx ) == 1

                    # creating array to store read data
                    mgrad= np.empty( [m_gp,len(gp_values[0])], dtype=float )
                    mflux= np.empty( [m_gp,len(gp_values[1])], dtype=float )
                    if idx[0] == 0 or idx[0] == 2:
                        minte = None
                    else:
                        minte = np.empty( [m_gp,len(gp_values[2])], dtype=float )

                    # jump to section of ther part to get size of fields
                    vidx = m_gp*s2323[idx[0]][0]
                    tgrad= np.empty( [t_gp,len(gp_values[vidx  ])], dtype=float )
                    tflux= np.empty( [t_gp,len(gp_values[vidx+1])], dtype=float )
                    if idx[0] == 0 or idx[0] == 1:
                        tinte = None
                    else:
                        tinte = np.empty( [t_gp,len(gp_values[vidx+2])], dtype=float )


                    # generating the correct iterator to set the value read into the correct
                    # section of array
                    if idx[0] == 0 or idx[0] == 2:
                        itermeca = itertools.chain.from_iterable( zip(mgrad, mflux) )
                    else:
                        itermeca = itertools.chain.from_iterable( zip(mgrad, mflux, minte) )

                    if idx[0] == 0 or idx[0] == 1:
                        iterther = itertools.chain.from_iterable( zip(tgrad, tflux) )
                    else:
                        iterther = itertools.chain.from_iterable( zip(tgrad, tflux, tinte) )

                    temp = None
                    fields = itertools.chain( itermeca, iterther )
                    for f, v in zip( fields, gp_values ):
                        f[:] = v
                else:
                    print( 'ERROR when reading GPV file, unknown avatar modelType {}'.format(mtype) )
                    raise ValueError

                for f in ('mgrad', 'mflux', 'minte', 'tgrad', 'tflux', 'tinte', 'temp'):
                    setattr(bodies[av_id].bulks[number-1], f, eval(f) if eval(f) is not None else [] )
 

            # going to next bdyty block
            if line.startswith('$$$$$$') :
                line = read_line(fid)
                atype  = None
                number = None
                etype  = None
                mtype  = None


    print('End reading file\t:\t'+fname)
    return nstep, ntime

