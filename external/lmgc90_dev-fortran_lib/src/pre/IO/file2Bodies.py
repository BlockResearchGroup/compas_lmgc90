import os
import numpy as np

from .. import avatars
from ..avatar import avatar
from ..avatar.bulk import rigid2d, rigid3d, element
from ..avatar.node import node
from ..avatar.group import group
from ..avatar.contactor import meshedContactor
from ..shared import model
from ..config import lmgc90dicts

from .utils import read_line, str2float

CONTACTOR_LIST = [ c for c in lmgc90dicts.listeContactor]
CONTACTOR_LIST.append( 'DISKb' )
CONTACTOR_LIST.append( 'SPHEb' )
# to handle old file format...
CONTACTOR_LIST.append( 'ASpx3' )
CONTACTOR_LIST.append( 'ASpx4' )

for i in range( 3 ):
    CONTACTOR_LIST.append( 'CSpx'+str(i) )

OLD_CONTACTOR_LIST = [ 'CSpx4', ]

# mapping the element type of BODIES.DAT
# to element dimension and connectivity size
ETYPE2DC = {}
for edim, consize in lmgc90dicts.geoAndnbNodes2Element.items():
    for cs, etype in consize.items():
        ETYPE2DC[ etype ] = (edim, cs)

# ugly remap of contactor parameter name
# between BODIES.DAT name and pre.avatar.contactor names...
OPT_REMAP = { 'ax1'   : 'axe1',
              'ax2'   : 'axe2',
              'apab=' : 'apab',
            }

# index in line to read node coor
IDX_COOR = (34, 55, 76)



def read_bulk(line, fid):
    """
    Read a $blmty block of a mesh from BODIES.DAT file.

    :param line: the current line of the file to parse
    :param fid : the file object to read next line if needed

    :return: - the type of element
             - the dimension of element
             - the id number of the blmty
             - the connectivity of the element
             - the name of the model associated
             - the name of the material associated
    """

    etype = line[1:6]
    idx   = int( line[6:13] )
    # paranoid ?
    assert line[15:20] == 'nodes'
    conn  = line[21:-1].split()
    conn  = [ int(n) for n in conn ]
    #connsize = etype2connsize[ etype ]
    edim, connsize = ETYPE2DC[ etype ]
    # reading next line(s) if connectivity is too long
    nb_lines = connsize//8 if connsize%8 else connsize//8-1
    for i in range( nb_lines ) :
        line = fid.readline()
        conn.extend( [ int(n) for n in line[21:-1].split() ] )

    line = fid.readline()
    mod_name = line[22:27]
    mat_name = line[36:41]

    return etype, edim, idx, conn, mod_name, mat_name
 

def read_node(line):
    """
    Read a node coordinates

    :param line: the string line with the nodes values

    :return: the float read in a list
    """
    nb_coor = len( line[27:-1].rstrip() ) // 21
    return [ str2float(line[i:i+14].replace('D','E')) for i in IDX_COOR[:nb_coor] ]


def get_opt_line(line):
    """
    Read the parameters of a line from BODIES.DAT file.

    :param line: the string of the line currently read

    :return: the parameters read on the line as a list of tuple of
             the form (param, value,)
    """

    params = list()

    # specific treatment of tactors...
    if line.strip() == 'localframe':
        return params
    elif line[29:38] == 'nb_vertex':
        params.append( ('nb_vertices',int(line[39:46]),) )
        if line[48:59].strip().strip("=").lstrip().startswith('nb_face') :
            start = 49+line[48:59].index('=')
            params.append( ('nb_faces',int(line[start:-1]),) )
        return params

    int_param = line[29:33] in ('noda', 'nodb', 'nodc', 'nodd')

    # length of parameter depends on int or float
    bound0 = { True : (slice(29,41,1), slice(41,None,1),),
               False: (slice(27,48,1), slice(48,None,1),),
             }
    boundi = { True : (slice(None,12,1), slice(12,None,1),),
               False: (slice(None,21,1), slice(21,None,1),),
             }
 
    # no choice but to split according to length of "param=value"
    left_line, right_line = line[bound0[int_param][0]], line[bound0[int_param][1]]

    # remap parameter name if needed
    llb = 4 if int_param else 6
    k = left_line[:llb].strip()
    k = OPT_REMAP[k] if k in OPT_REMAP.keys() else k

    # add parameter and read as float (with replace)
    if int_param or k[:3] == 'ver' :
        params.append( (k , int(left_line[llb+1:]),) )
    else:
        params.append( (k , float( left_line[7:].replace('D','E') ),) )

    # same as above for all other parameters on the line
    while right_line.strip():
        int_param = right_line[:4] in ('noda', 'nodb', 'nodc', 'nodd')
        left_line, right_line = right_line[boundi[int_param][0]], right_line[boundi[int_param][1]]
        # update left line bound:
        llb = 4 if int_param else 6
        k = left_line[:llb].strip()
        k = OPT_REMAP[k] if k in OPT_REMAP.keys() else k
        if int_param or k[:3] == 'ver' :
            params.append( (k, int(left_line[llb+1:]),) )
        else:
            llb = llb+1 if k!='apab' else llb
            params.append( (k, float( left_line[llb:].replace('D','E')),) )

    return params


def remap_params( param_list, shape ):
    """
    Depending on the contactor shape, remap the list of parameters
    into a dictionary.

    :param param_list: a list of parameters in the form of a tuple
    :param shape: the 5 character string of the contactor currently read

    :return: a dictionary in the form 'opt:value' usable to add contactor to an avatar object
    """
    if shape == 'POLYG' :
        # getting number of vertices
        k, nb_v = param_list[0]
        dparam = {k:nb_v}
        # then all coordinates
        coor = np.array( [ v for k,v in param_list[1:2*nb_v+1] ], dtype=float )
        coor.shape = [nb_v,coor.size//nb_v]
        dparam['vertices'] = coor
    elif shape == 'POLYR':
        kv, nb_v = param_list[0]
        kf, nb_f = param_list[1]
        dparam = {kv:nb_v,kf:nb_f}
        coor = np.array( [ v for k,v in param_list[2:3*nb_v+2] ], dtype=float )
        face = np.array( [ v for k,v in param_list[3*nb_v+2: ] ], dtype=int   )
        coor.shape = [nb_v,coor.size//nb_v]
        face.shape = [nb_f,face.size//nb_f]
        dparam['vertices'] = coor
        dparam['connectivity'] = face
    elif shape == 'PLANx':
        dparam = { k:v for k,v in param_list[:3] }
        if len( param_list ) > 3:
            frame = np.array( [ v for k,v in param_list[ 3:12] ], dtype=float )
            frame.shape = [3,3]
            dparam['frame'] = frame.T
            coor  = np.array( [ v for k,v in param_list[12:15] ], dtype=float )
            dparam['shift'] = coor
    elif shape == 'CYLND':
        dparam = { k:v for k,v in param_list[:2] }
        if len( param_list ) > 3:
            frame = np.array( [ v for k,v in param_list[ 2:11] ], dtype=float )
            frame.shape = [3,3]
            dparam['frame'] = frame.T
            coor  = np.array( [ v for k,v in param_list[11:14] ], dtype=float )
            dparam['shift'] = coor
    else:
        dparam = { k:v for k,v in param_list }
        # rename [coo1/2/3] to shift
        to_del = [ k for k in dparam.keys() if 'coo' in k ]
        if to_del:
            dparam['shift'] = np.array( [dparam[k] for k in to_del], dtype=float )
            for k in to_del:
                del(dparam[k])
    return dparam


def read_bodies(dim, mats, mods, fpath):
    """
    Read a BODIES.DAT file

    :param mats: a material container used to create avatars
    :param mods: a model container used to create avatars
    :param fpath: the directory path to the BODIES.DAT file to read

    :return: avatars container
    """

    #assert fpath.is_dir()
    assert os.path.isdir(fpath)
    if 'DAT' in fpath:
        ext = '.DAT'
    else:
        ext = '.OUT'

    #fname = fpath/'BODIES.DAT'
    #assert  fname.is_file()

    fname = os.path.join(fpath,'BODIES'+ext)
    assert  os.path.isfile(fname)

    print()
    print('Start reading file\t:\t'+fname)

    bodies = avatars()

    with open(fname,'r') as fid:

        m_counting = { ('RBDY2','MECAx',) : 0,
                       ('RBDY3','MECAx',) : 0,
                       ('MAILx','MECAx',) : 0,
                       ('MAILx','THERx',) : 0,
                       ('MAILx','POROx',) : 0,
                       ('MAILx','MULTI',) : 0,
                     }
        line = read_line( fid )
        while line :

            # read bdyty block
            if line.startswith( '$bdyty' ) :
                line = read_line(fid)

                # get the avatar type and number
                atype  = line[:6].strip()
                number = int(line[6:].strip())

                if atype[:-1] == 'RBDY':
                  # works for rbdy2/3
                  assert dim == int( atype[-1] )

                av = avatar.avatar(dimension=dim)
                av.atype = atype
                av.number= number

                mat_name = None
                mod_name = None

                # adding to the list right now
                bodies.append( av )

                # reading next line and 
                line = read_line(fid)

            # read blmty block
            if line.startswith( '$blmty' ) :
                line = read_line(fid)

                if line[1:6] == 'PLAIN':

                    mod_name = line[1:6]
                    idx = int( line[6:13] )
                    mat_name = line[22:27]

                    # there is a space after avrd= value
                    avrd = float( line[34:49].replace('D', 'E') )

                    # trying to read gyrd in 2D or Inertia on next line in 3D
                    if av.atype == 'RBDY2':
                        assert 'gyrd=' in line, 'ERROR no gyrd parameter in bulk'
                        # there is a space after avrd= value
                        gyrd = float( line[55:].replace('D','E') )
                        elem = rigid2d.rigid2d(avrd=avrd, gyrd=gyrd, number=idx)
                        line = read_line(fid)
                    elif av.atype == 'RBDY3':
                        # attempting to read inertia:
                        line = read_line(fid)
                        if line[0] != '$' :
                            inertia = np.array( [ float(line[idx:idx+14].replace('D','E')) for idx in (34,56,77,) ] )
                            line = read_line(fid)
                        else :
                            # and must go to next block without reading next line
                            inertia = np.eye( 3, dtype=float )
                        elem = rigid3d.rigid3d(avrd=avrd, inertia=inertia, number=idx)
                    else:
                        print( 'ERROR reading PLAIN bulk, but not a rigid avatar' )


                    av.bulks.append(elem)
                    assert mat_name in mats.keys(), 'ERROR material {} not found'.format( mat_name )
                    mat = mats[mat_name]
                    if mod_name not in mods.keys() and mod_name=='PLAIN':
                        # use a rigid model
                        melem= 'Rxx'+str(av.dimension)+'D'
                        mod  = model.model(name='PLAIN', physics='MECAx', element=melem, dimension=dim)
                    else:
                        assert mod_name in mods.keys(), 'ERROR model '   +mod_name+' not found'
                        mod  = mods[mod_name]

                    elem.defineModel(mod)
                    elem.defineMaterial(mat)
                    av.modelType = mod.physics

                else:

                    # check at least one element:
                    while line[1:6] in ETYPE2DC.keys() :

                        etype, edim, idx, conn, mod_name, mat_name = read_bulk(line, fid)

                        # how to do this differently ?
                        elem_dim, conn_size = ETYPE2DC[line[1:6]]

                        elem = element.element(elem_dim, conn, number=idx)
                        mod = mods[mod_name]
                        elem.defineModel(mod)
                        mat = mats[mat_name]
                        elem.defineMaterial(mat)

                        av.addBulk(elem)
                        if av.modelType:
                            assert av.modelType == mod.physics, 'wrong model type'
                        else :
                           av.modelType = mod.physics

                        # must go to next line in case it is a new elem in same blmty block
                        line = read_line(fid)

                m_counting[ (av.atype,av.modelType,) ] += 1
                av.m_num = m_counting[ (av.atype,av.modelType) ]

            # read nodty block
            if line.startswith('$nodty') :
                line = read_line(fid)

                while line[1:6] in lmgc90dicts.dimensionTypeNode.values() :

                    no_type = line[:6].strip()
                    no_idx  = int(line[6:13])
                    coor = read_node( line )
                    if int( no_type[2] ) > 3 :
                        line = fid.readline()
                        coor.extend( read_node(line) )

                    rot = None
                    if av.atype != 'MAILx' :
                        if dim == 2:
                            # switch no3xx to no2xx in 2D...
                            # and store rotation to apply it after computeRigidProperties for 2D
                            no_type = no_type.replace('3', '2')
                            rot  = coor.pop()
                            coor = np.array( [v for v in coor], dtype=float )
                        elif dim == 3:
                            # switch no6xx to no3xx in 3D...
                            no_type = no_type.replace('6', '3')
                            coor = np.array( [v for v in coor[:dim]], dtype=float )
                        else:
                            print( 'ERROR while reading nodty block' )
                    else:
                        coor = np.array( [v for v in coor], dtype=float )

                    no = node.node( coor, number=no_idx )
                    assert no.ntype == no_type, 'wrong node type {}/{}'.format(no.ntype, no_type)

                    av.addNode(no)

                    # next node or block
                    line = read_line(fid)


            # hopefully, all nodes and bulks were read for current avatar
            # so we can:
            av.defineGroups()
            # and node dofs can be define from bulk model:
            for b in av.bulks:
                for n in b.connectivity:
                    av.nodes[n].defineDof(b.model)



            # read tacty in block
            if line.startswith('$tacty') :
                line = read_line(fid)
                while line[1:6] in CONTACTOR_LIST  or line[1:6] in OLD_CONTACTOR_LIST:

                    shape  = line[1:6]
                    number = int( line[6:13] )
                    color  = line[22:27]

                    # pfffff
                    if shape == 'DISKb':
                        shape = 'DISKx'
                    elif shape == 'SPHEb':
                        shape = 'SPHER'
                    elif shape == 'CSpx4':
                        shape = 'CSpx0'

                    # in fact float or int depending on the tactor
                    params = get_opt_line( line )

                    # next line
                    line = read_line(fid)
                    # same contactor
                    while line[0] != '$' and line[1:6] not in CONTACTOR_LIST :
                        params.extend( get_opt_line(line) )
                        line = read_line(fid)

                    if av.atype[:-1] == 'RBDY':
                        # add rigid contactor
                        dparams = remap_params( params, shape )
                        av.addContactors(shape, color, number=number, **dparams)
                    else:

                        # add meshed contactor
                        weights = []
                        conn    = []
                        for k, v in params:
                            if k == 'apab':
                                weights.append(v)
                            else :
                                conn.append(v)

                        elem_dim = av.dimension-1

                        elem = element.element(elem_dim=elem_dim, connectivity=conn)
                        av.addBulk(elem)
                        dparams = {'elements'  : [ elem, ] }

                        #if shape[-1] != 'x' and shape != 'PT2DL' :
                        #    #paranoid
                        #    assert shape[:-1] == 'CSpx'
                        #    dparams['quadrature'] = int(shape[-1])
                        #    shape = 'CSpxx'

                        # next line is already read...
                        while line.strip() and line[0] == '+':
                            assert line[1:6] == shape
                            params = get_opt_line(line)
                            conn   = []
                            for k, v in params:
                                if k == 'apab':
                                    weights.append(v)
                                else :
                                    conn.append(v)

                            elem = element.element(elem_dim=elem_dim, connectivity=conn)
                            av.addBulk(elem)
                            dparams['elements'].append(elem)

                            # next line
                            line = read_line(fid)
                            if line[0] == '$':
                                break

                        if shape[:4] == 'ASpx' and shape[4].isdigit():
                            shape = 'ASpxx'
                        if shape[:4] == 'CSpx':
                            cclass = 'cspxx'
                        else:
                            cclass = shape.lower()
                        cont = eval( 'meshedContactor.'+cclass+'.__new__(meshedContactor.'+cclass+')' )
                        setattr(cont, 'shape', shape)
                        super( eval('meshedContactor.'+cclass), cont ).__init__( shape=shape, color=color, **dparams )
                        if weights:
                            setattr(cont, 'weights', weights)
                        av._addContactor(cont)

                    if line[0] == '$' :
                        break


            # now that all tacts are added
            # we can hopefully:
            av.imposeInitValue()
            if ( atype == 'RBDY2' and av.bulks[0].gyrd==0. ) \
            or ( atype == 'RBDY3' and np.all( av.bulks[0].inertia==0. ) ):
                av.computeRigidProperties()
            elif rot is not None:
                av.bulks[0].axis[0,0] = np.cos(rot)
                av.bulks[0].axis[1,0] = np.sin(rot)
                av.bulks[0].axis[0,1] =-av.bulks[0].axis[1,0]
                av.bulks[0].axis[1,1] = av.bulks[0].axis[0,0]

            if line.startswith('$$$$$$'):
                line = read_line(fid)

    # in case there is some error in the numbering read
    print('End reading file\t:\t'+fname)
    return bodies

