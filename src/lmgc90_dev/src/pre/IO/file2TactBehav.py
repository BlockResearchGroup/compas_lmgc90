import os
import numpy as np

from ..tact_behavs import tact_behavs, see_tables
from ..shared import tact_behav

from .utils import read_line

# a list of dicts used to remap option name between .DAT and pre
REMAPPERS = [ { 'sfric' : 'fric'     , 'dt___': 'dt__'     , 'pgap_' : 'pgap',
                'F/gap' : 'stiffness', 'etan' : 'viscosity',
                'F/str' : 'stiffness', 'F/gp' : 'stiffness',
                'F/sra' : 'viscosity', 'F/sr' : 'viscosity',
                'snmax' : 'Fmax'     , 'prstr': 'prestrain',
                'Fstr'  : 'stiffness', 'prst' : 'prestrain',
                'G0'    : 'g0'       , },
              { 'sfric' : 'stfr', 'dfric' : 'dyfr',},
              { 'sfric' : 'fric', 'T/H'   : 'ToverH', 'gTot':'gtol' },
              { 'sfric' : 'fric', 'restn' : 'rstn', 'restt' : 'rstt', },
              { 'sfric' : 'stfr', 'dfric' : 'dyfr',
                'cohen' : 'cohn', 'cohet' : 'coht',
                'Wethk' : 'Wthk', 'cohet' : 'coht', },
              { 'dfric': 'dyfr'   , 'sfric': 'stfr'   ,
                'visco': 'b'      , 'dupre': 'w'      ,
                'S1'   : 's1'     , 'S2'   : 's2'     ,
                'lmbds': 'lambdas', 'lmbdc': 'lambdac',
                'lbds' : 'lambdas', 'lbdc' : 'lambdac',
                'l1'   : 'dp1'    , 'l2'   : 'dp2'    ,
                'Du1'  : 'du1'    , 'Du2'  : 'du2'    ,
                'Phi'  : 'phi'    , 'Eta'  : 'eta'    ,
                'nmol' : 'n_mol'  , 'kcoa' : 'kcoal'  , },
            ]

# associate each remapper with the list of laws available
REMAP2LAWS = { 0 : ['IQS_CLB'    , 'IQS_CLB_g0', 'IQS_CLB_nosldt' ,
                    'GAP_SGR_CLB', 'GAP_SGR_CLB_nosldt',
                    'VEL_SGR_CLB', 'preGAP_SGR_CLB', 'GAP_SGR_CLB_g0',
                    'ELASTIC_REPELL_CLB', 'ELASTIC_REPELL_CLB_g0', 'ELASTIC_REPELL_CLB_adapt',
                    'VISCO_ELASTIC_REPELL_CLB', 'ELASTIC_WIRE',
                    'ELASTIC_ROD', 'VOIGT_ROD',
                    'BRITTLE_ELASTIC_WIRE', 'BRITTLE_COATING_CLB' ],
               1 : ['IQS_DS_CLB' , ],
               2 : ['IQS_CLB_RGR' , ],
               3 : ['RST_CLB' , ],
               4 : ['IQS_WET_DS_CLB', 'IQS_MOHR_DS_CLB', 'GAP_MOHR_DS_CLB','xQS_WET_DS_CLB'],
               5 : ['IQS_MAC_CZM', 'IQS_MAL_CZM', 'MAC_CZM', 'MAL_CZM',
                    'MP_CZM', 'MP3_CZM', 'MP3_CZM_THER', 'TH_CZM', 'IQS_TH_CZM',
                    'ABP_CZM', 'IQS_ABP_CZM', 'EXPO_CZM', 'IQS_EXPO_CZM', 'EXPO_CZM_P', 'IQS_EXPO_CZM_P',
                    'EXPO_CZM_SPRING', 'IQS_EXPO_CZM_SPRING', 'EXPO_CZM_SPRING_P', 'IQS_EXPO_CZM_SPRING_P', 'NARD_ROD',
                    'TOSI_CZM', 'TOSI_CZM_INCRE'],
             }

# reverse dict to get the remapper from the law name
OPT_REMAP = { law : REMAPPERS[map_id] for map_id, laws in REMAP2LAWS.items() for law in laws }


def get_opt_line(line, lawty):
    """
    Read the parameters of line of a $behav block from TACT_BEHAV.DAT file.

    :param line: the string with the line currently read

    :return: the options read as a list of tuple in the form (param, value,)
    """

    # if no param at all...
    if not line[40:].strip():
        return dict()

    # no choice but to split according to length of "param=value"
    left_line, right_line = line[40:61], line[61:]

    # remap parameter name if needed
    k = left_line[:5].strip().strip('=').strip()
    if lawty in OPT_REMAP.keys() and k in OPT_REMAP[lawty].keys() :
        k = OPT_REMAP[lawty][k]

    # add parameter and read as float (with replace)
    params = { k : float( left_line[6:].replace('D','E') ) }

    # same as above for all other parameters on the line
    while right_line.strip():
        left_line, right_line = right_line[:21], right_line[21:]
        k = left_line[:5].strip().strip("=").strip()
        if lawty in OPT_REMAP.keys() and k in OPT_REMAP[lawty].keys() :
            k = OPT_REMAP[lawty][k]
        params[k] = float( left_line[5:].replace('D','E') )

    return params


def read_tact_behav(fpath):
    """
    Read a TACT_BEHAV.DAT file

    :param fpath: the string of the path of the TACT_BEHAV.DAT file to read

    :returns : - tacts: container of contact laws
               - sees : container of visibility tables
    """

    #assert fpath.is_dir()
    assert os.path.isdir(fpath)
    if 'DAT' in fpath:
        ext = '.DAT'
    else:
        ext = '.OUT'

    #fname = fpath/'TACT_BEHAV.DAT'
    #assert  fname.is_file()
    fname = os.path.join(fpath,'TACT_BEHAV'+ext)
    assert  os.path.isfile(fname)

    print()
    print('Start reading file\t:\t'+fname)

    tacts = tact_behavs()
    sees  = see_tables()

    with open(fname,'r') as fid:

        line = read_line(fid)
        while line :

            # if a new behav block
            if line[:6] == "$behav" :

                # get the name and contact law type
                line = read_line(fid)
                name, lawty = line[1:6], line[8:40].strip()

                # first params on current line
                params = get_opt_line( line, lawty )

                # then check next lines for more param
                line = read_line( fid )
                while line and line[0] != "$" :
                    params.update( get_opt_line(line, lawty) )
                    line = read_line( fid )

                new_behav = tact_behav.tact_behav(name, lawty, **params)
                tacts.addBehav( new_behav )

            # if a new seety block
            elif line[:6] == "$seety" :

                # read descriptor
                line = read_line(fid)

                # read seety
                line = read_line(fid)
                line = line.split()
                cdbdy, cdtac, cdcol, behav, anbdy, antac, ancol, alert = line
                alert = float( alert.replace('D','E') )

                # read halo or continue reading
                line = read_line( fid )
                if line and line[0] == '+':
                    halo = float( line[1:].replace('D','E') )
                    # to coninue reading
                    line = read_line( fid )
                else:
                    halo = None

                behav = tacts[behav]
                new_seety = tact_behav.see_table(cdbdy, cdtac, cdcol, behav,
                                                 anbdy, antac, ancol, alert, halo
                                                )
                sees.addSeeTable( new_seety )

            # keep looping to next behav/seety block
            else:
                line = read_line(fid)

    print('End reading file\t:\t'+fname)
    return  tacts, sees

