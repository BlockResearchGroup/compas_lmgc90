import os
import collections

from ..config import lmgc90dicts

from ..models import models
from ..shared import model

from .utils import read_line

KEYWORD2MODELOPTION = { v:k for k,v in lmgc90dicts.modelOption2Keyword.items() }


def read_models(dim, fpath):
    """
    Read a MODELS.DAT file

    :param dim: integer with the dimension of the data to read
    :param fpath: the string of the path of the MODELS.DAT file to read

    :returns: pre.models container
    """

    assert isinstance( dim, int ), 'dim parameter is not an integer'
    assert dim == 2 or dim == 3, 'dim parameter must be 2 or 3 not {}'.format(dim)

    #assert fpath.is_dir()
    assert os.path.isdir(fpath)
    if 'DAT' in fpath:
        ext = '.DAT'
    else:
        ext = '.OUT'

    # create empty container
    model_list = models()

    # sometimes with rigides, there is no MODEL file...
    # so no assert, but the possibility to skip
    #fname = fpath/'MODELS.DAT'
    #assert  fname.is_file()
    fname = os.path.join(fpath,'MODELS'+ext)
    if not os.path.isfile(fname) :
        print('Skip reading file\t:\t'+fname)
        return  model_list

    print()
    print('Start reading file\t:\t'+fname)

    # now reading file
    with open(fname,'r') as fid:

        line = read_line(fid)
        while line:

            # new model block
            if line.startswith("$model") :

                # get the model name
                line = read_line(fid)
                name = line.strip()

                # get the physical model and element
                line = read_line(fid).split()
                physic, elem = line[0].strip(), line[1].strip()

                # managing SPRG2/3 in pre vs SPRNG in .DAT
                if elem == 'SPRNG':
                    elem = 'SPRG'+str(dim)

                # renaming THERM in THERx...
                if physic == 'THERM':
                    physic = 'THERx'

                # paranoid ?
                assert physic in lmgc90dicts.listeModel
                assert elem   in lmgc90dicts.listeElement
                assert elem   in lmgc90dicts.dimension2element[dim]


                # first option on current line
                if len(line) > 2:
                    if line[2].strip() == 'u_mdl':
                      params = {'user_model_name': line[3].strip()}
                    else:
                      params = { KEYWORD2MODELOPTION[line[2].strip()] : line[3].strip() }
                else:
                    params = {}

                line = read_line( fid )
                while line and not line.startswith("$model"):
                    line = line.split()
                    k, v = line[0].strip(), line[1].strip()
                    if k == 'extsf':
                      if 'external_fields' not in params.keys():
                          params[ 'external_fields' ] = []
                      params[ 'external_fields' ].append(v)
                    elif k == 'extvf':
                      if 'external_vfields' not in params.keys():
                          params[ 'external_vfields' ] = []
                          params[ 'external_vsizes'  ] = []
                      params[ 'external_vfields' ].append(v)
                      params[ 'external_vsizes'  ].append( int(line[2].strip()) )
                    elif k == 'u_mdl':
                      params[ 'user_model_name' ] = v
                    else:
                      params[ KEYWORD2MODELOPTION[k] ] = v
                    line = read_line( fid )
                # old databox management:
                isext = 'external_model'
                if isext in params.keys() and physic in ['MECAx', 'POROx',]:
                  params[isext] = 'MatL_' if params[isext]=='yes__' else params[isext]

                new_model = model.model(name, physic, elem, dim, **params)
                model_list.addModel( new_model )

            # keep looping to next model block
            else:
                line = read_line(fid)

    print('End reading file\t:\t'+fname)

    return model_list

