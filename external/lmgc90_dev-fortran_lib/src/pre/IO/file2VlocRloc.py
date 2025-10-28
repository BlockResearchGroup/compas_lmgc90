import os
import numpy as np

from .formatVlocRloc import INTERS2FORMAT, FORMATS, COO_FIRST, XL, \
                            STN_INTERS, TNS_INTERS, STN_ORDER, TNS_ORDER

from .utils import read_line, str2float

#TODO : XL management...

# lmgc internal value at compile time
MAX_INT = 8

# mapping name to type
FIELDS2TYPE = {'inter'    : 'S5',
               'icdan'    : 'i4',
               'cdbdy'    : 'S5',
               'icdbdy'   : 'i4',
               'cdtac'    : 'S5',
               'icdtac'   : 'i4',
               'anbdy'    : 'S5',
               'ianbdy'   : 'i4',
               'antac'    : 'S5',
               'iantac'   : 'i4',
               'icdsci'   : 'i4',
               'iansci'   : 'i4',
               'behav'    : 'S5',
               'status'   : 'S5',
               'iadj'     : 'i4',
               'nb_int'   : 'i4',
               'rl'       : '(dim,)f8'    ,
               'vl'       : '(dim,)f8'    ,
               'gapTT'    : 'f8'          ,
               'coor'     : '(dim,)f8'    ,
               'uc'       : '(dim,dim)f8' ,
               'internals': '(max_int,)f8',
              }


def read_vloc_rloc(dim, fpath, step=0, with_xl=False ):
    """
    Read a VlocRloc.INI, VlocRloc.OUT.step or VlocRloc.LAST file

    :param dim: the space dimension
    :param fpath: the path to DATBOX or OUTBOX if step is not 0
    :param step: (optional) a step number of VlocRloc.OUT file to read instead of DOF.INI
                 If -1, read the VlocRloc.LAST file.
    :param with_xl: (optional) if using XL format

    :return: - inters_array: a numpy array of dedicated dtype with all interactions
             - nstep : the time step id read
             - ntime : the corresponding time read
    """

    fields  = [ f for f in FIELDS2TYPE.keys() ]
    formats = [ f.replace('dim',str(dim)).replace('max_int',str(MAX_INT)) 
                for f in FIELDS2TYPE.values()
              ]
    dtype = np.dtype( {'names':fields, 'formats':formats} )

    if step:
      if step == -1 :
          fread = 'Vloc_Rloc.LAST'
      else :
          fread = 'Vloc_Rloc.OUT.'+str(step)
    else:
      fread = 'Vloc_Rloc.INI'

    #assert fpath.is_dir()
    assert os.path.isdir(fpath)

    #fname = fpath/'BODIES.DAT'
    #assert  fname.is_file()
    fname = os.path.join(fpath,fread)
    assert  os.path.isfile(fname)

    print()
    print('Start reading file\t:\t'+fname)


    with open(fname,'r') as fid:

        inters = list()

        line = read_line(fid)

        assert line[:7] != "$steps", "ERROR: should be reading 'steps' keyword"

        nstep =   int( line[ 7:16] )
        ntime = float( line[35:49].replace('D','E') )

        line = read_line(fid)

        while line:

           if line.startswith('$icdan') :

               # no better ?
               new_inter = np.zeros( [1], dtype=dtype )

               inter_type = line[8:13]
               new_inter['inter'] = inter_type
               new_inter['icdan'] = int( line[13:-1].strip() )

               # read format line
               line = read_line(fid)
               format_id = INTERS2FORMAT[ inter_type ]
               #assert HEADER[ INTERS2HEADER[inter_type] ] == line[:-1]
               if inter_type in XL and with_xl:
                 format_id = XL[inter_type]

               line = read_line(fid)

               # depending on inter, change read format of the idata line:
               start = 0
               for field, convert, prefix, length in FORMATS[ format_id ]:
                   start += len(prefix)
                   value = convert( line[start:start+length] ) if line[start:start+length].strip() else 0
                   new_inter[0][field] = value
                   start += length

               # now reading rdata:
               indices = (34, 55, 76,)
               line = read_line(fid)
               cmp_idx = [ 'tns'.find(line[idx]) for idx in (31, 52, 73,)[:dim] ]
               new_inter[0]['rl'][cmp_idx] = [ str2float(line[idx:idx+14].replace('D','E')) for idx in indices[:dim] ]
               line = read_line(fid)
               # index search of rl is used
               new_inter[0]['vl'][cmp_idx] = [ str2float(line[idx:idx+14].replace('D','E')) for idx in indices[:dim] ]
               line = read_line(fid)
               new_inter[0]['gapTT'] = str2float(line[34:34+14].replace('D','E'))

               if inter_type in COO_FIRST:
                   line = read_line(fid)
                   new_inter[0]['coor'][:] = [ str2float(line[idx:idx+14].replace('D','E')) for idx in indices[:dim] ]

               if dim == 2:
                   line = read_line(fid)
                   new_inter[0]['uc'][1,:] = [ str2float(line[idx:idx+14].replace('D','E')) for idx in indices[:dim] ]
                   new_inter[0]['uc'][0,0] = new_inter[0]['uc'][1,1]
                   new_inter[0]['uc'][0,1] =-new_inter[0]['uc'][1,0]
               elif dim == 3:
                   if inter_type in STN_INTERS:
                       cmp_list = STN_ORDER
                   elif inter_type in TNS_INTERS:
                       cmp_list = TNS_ORDER
                   else :
                       cmp_list = ( (1,'n',), )

                   for i, c in cmp_list:
                       line = read_line(fid)
                       # paranoid ?
                       new_inter[0]['uc'][i,:] = [ str2float(line[idx:idx+14].replace('D','E')) for idx in indices[:dim] ]

               if inter_type not in COO_FIRST:
                   line = read_line(fid)
                   new_inter[0]['coor'][:] = [ str2float(line[idx:idx+14].replace('D','E')) for idx in indices[:dim] ]

               # attempt to read internals
               line = read_line(fid)
               if line and line.strip() and line[0] != '$' :
                   # internal comment is skipped by read_line
                   #line = read_line(fid)
                   line = line[:-1].split()
                   nb_int =  len(line)
                   new_inter[0]['nb_int'] = nb_int
                   new_inter[0]['internals'][:nb_int] = [ str2float(v.replace('D','E')) for v in line ]
                   line = read_line(fid)

               inters.append(new_inter)

           # going to next icdan block
           else:
               line = read_line(fid)


    inters_array = np.empty( [len(inters)], dtype=dtype )
    for i, inter in enumerate(inters):
        inters_array[i] = inter[0]
    print('End reading file\t:\t'+fname)

    return inters_array, nstep, ntime

