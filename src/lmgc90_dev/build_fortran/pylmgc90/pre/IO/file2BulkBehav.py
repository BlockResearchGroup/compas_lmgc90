import os
import numpy as np

from ..bulk_behavs import materials
from ..shared import bulk_behav

from .utils import read_line

# a list of dict used as constant in the module !

# the set of keyword to ignore:
IGNORE_PARAM = set( ('cpl_', 'cplt', 'ther',) )

# dictionary allowing to remap .DAT name versus pre argument
OPT_REMAP = { 'Umas':'density',
              'TCnd':'thermal_conductivity',
              'HPse':'specific_heat',
              'Hspe':'specific_heat',
              'Eth_':'thermal_young',
              'Eeq_':'thermal_young',
              'Nuth':'thermal_nu',
              'NUeq':'thermal_nu',
              'ani_':'anisotropy',
              'EYng':'young',
              'Epss':'nu',
              'EPss':'nu',
              'visc':'viscous_model',
              'vpla':'viscous_model',
              'SPHV':'specific_capacity',
              'COCO':'conductivity',
              'Dila':'dilatation',
              'Tref':'T_ref_meca',
              'BIOT':'hydro_cpl',
              'SIG0':'iso_hard',
              'K___':'isoh_coeff',
              'crit':'critere',
            }

# those a string parameters of the bulk_behav
STR_PARAM = set( ('iso' , 'elas', 'critere', 'isoh', 'cinh',
                 'anisotropy', 'viscous_model',)
               )

# a dictionary to rebuild a list of parameters as a single vector
VEC_REMAP = {('EY11','EY22','EY33',)       :'young'        ,
             ('EP11','EP22','EP33',)       :'nu'           ,
             ('G12_','G13_','G23_',)       :'G'            ,
             ('m1'  ,'m2'  ,'m3'  ,)       :'masses'       ,
             ('k1'  ,'k2'  ,'k3'  ,)       :'stiffnesses'  ,
             ('c1'  ,'c2'  ,'c3'  ,)       :'viscosities'  ,
             ('kncc','ec'         ,)       :'consolidation',
             ('kt'  ,'knc' ,'knt' ,)       :'stiffnesses'  ,
             ('ftrc','phi' ,'C'   ,'zmu' ,):'mc'           ,  
             ('phi' ,'zmu' , 'pf', 'pd', 'ct', 's2', 'G2', 'cn', 's1', 'G1',): 'fczm',  
            }


def get_opt_line(line):
    """
    Read the parameters of line of a $behav block from BULK_BEHAV.DAT file.

    :param line: the string with the line currently read

    :return: the options read as a list of tuple in the form (param, value,)
    """

    # if no param at all...
    if not line[40:].strip():
        return list()

    # return varable 
    params = list()

    # no choice but to split according to length of "param=value"
    left_line, right_line = line[40:61], line[61:]

    # add parameter and read as float (with replace)
    k = left_line[:4].strip().strip(':')
    k = OPT_REMAP[k] if k in OPT_REMAP.keys() else k

    if k not in IGNORE_PARAM:

        if k not in STR_PARAM:
            v = float( left_line[5:].replace('D','E') )
        else:
            v = left_line[5:].strip()
        params.append( tuple((k, v,)) )

    # same as above for all other parameters on the line
    while right_line.strip():

        left_line, right_line = right_line[:21], right_line[21:]
        k = left_line[:4].strip().strip(':')

        if k in IGNORE_PARAM:
            continue

        k = OPT_REMAP[k] if k in OPT_REMAP.keys() else k
        if k not in STR_PARAM:
            v = float( left_line[5:].replace('D','E') )
        else:
            v = left_line[5:].strip()

        params.append( tuple((k,v)) ) 

    return params


def remap_params( param_list, lawty ):
    """
    Remap the list of parameters of a specific law into a dict

    :param param_list: a list of parameters in the form of a tuple
    :param lawty: the string of the type of bulk_behav object to create

    :return: a dictionary in the form 'opt:value' usable to create a bulk_behav object
    """

    # the return value
    dparams = { }

    # to check if managing a viscosity option
    visc = False

    for k, v in param_list :

        # rename young/nu attributes of viscous model
        # and check that elastic attributs are already there
        if visc and (k=='young' or k=='nu') :
            assert k in dparams.keys(), 'adding viscous param, but no elas ones...'
            dparams['viscous_'+k] = v
            continue

        # check that if anisotropy is already present it has the same value
        if k == 'anisotropy' and k in dparams.keys() :
            assert v == dparams[k], 'several ani_ keyword, but with different values...'
            continue

        # otherwise assert that attributs is not already present
        assert k not in dparams.keys()

        # some shitty specification to handle difference of parameter name
        # between VISCO_ELAS and ELAS_PLAS
        if lawty == 'ELAS_PLAS' and k == 'viscous_model':
            dparams['visc'] = v
            continue

        # check if in viscous part from now on
        if k=='viscous_model' and v != 'none':
            visc = True

        if k == 'iso' :
            assert 'anisotropy' not in dparams.keys()
            dparams['anisotropy'] = 'isotropic'
        else:
            dparams[k] = v

    # rework on the generated dict
    # combine some keys in a single vector
    for p, k in VEC_REMAP.items() :
        if p[0] in dparams.keys() :
            vec_p = []
            for ii in p :
               vec_p.append( dparams[ii] )
               del( dparams[ii] )
            dparams[k] = vec_p

    return dparams


def read_bulk_behav(dim, fpath):
    """
    Read a BULK_BEHAV.DAT file

    :param dim: integer with the dimension of the data to read
    :param fpath: the string of the path of the BULK_BEHAV.DAT file to read

    :return: - bulks: pre.bulks container
             - gravy: gravity read
    """

    assert isinstance( dim, int ), 'dim parameter is not an integer'
    assert dim == 2 or dim == 3, 'dim parameter must be 2 or 3 not {}'.format(dim)

    #assert fpath.is_dir()
    assert os.path.isdir(fpath)

    #fname = fpath/'BULK_BEHAV.DAT'
    #assert  fname.is_file()
    if 'DAT' in fpath:
        ext = '.DAT'
    else:
        ext = '.OUT'
    fname = os.path.join(fpath,'BULK_BEHAV'+ext)
    assert  os.path.isfile(fname)

    print()
    print('Start reading file\t:\t'+fname)

    # create empty container to fill
    material_list = materials()

    # reading file
    with open(fname,'r') as fid:

        line = read_line(fid)
        assert line.startswith("$gravy"), "ERROR: should be reading 'gravy' keyword"

        line = read_line(fid)

        gravy = [ float( line[i:i+16].replace('D','E') ) for i in (24, 45, 66,)[:dim] ]
        gravy = np.array( gravy, dtype=float )

        line = read_line(fid)
        while line:

            # new behav block
            if line.startswith("$behav"):

                # get the name and material type
                line = read_line(fid)
                name, materialType = line[1:6], line[8:40].strip()

                params = get_opt_line( line )

                if materialType != 'USER_MAT':

                    # then check next lines for more param
                    line = read_line( fid )
                    while line and line[0] != "$" :
                        params.extend( get_opt_line(line) )
                        line = read_line( fid )

                    # remap param list in dict
                    dparams = remap_params( params, materialType )

                else:

                    line = read_line( fid )
                    dparams = { k:v for k,v in params }
                    dparams['file_mat'] = line.strip()

                new_material = bulk_behav.material(name, materialType, **dparams)
                material_list.addMaterial( new_material )

            # keep looping to next behav block
            else: 
                line = read_line(fid)

    print('End reading file\t:\t'+fname)

    return material_list, gravy


