import h5py
import pandas as pd
import re

def get_parameters(hfile):
    """
    Extract from an HDF5 file of LMGC90 the mapping between integer id
    and string associated name.

    :param hfile:  (string) name of the file from which to read
    :return: a dict where keys are parameters and values a dict describing
             the mapping between the integer parameter value and the associated name.
    """

    with h5py.File(hfile, 'r') as hf:

        # making dictionnary for each parameters
        basepath = hf['Help/parameters']
        parameters = {}
        for k in basepath.keys():
            parameters[k] = dict(zip(basepath[k+'/id'][()],
                                     map(bytes.decode, basepath[k+'/name'][()])
                                     )
                                 )
    return parameters


def get_data_frame( hfile, basegroup, dset, mapper=None, compo=lambda a,b:a+b):
    """
    Generate and return a pandas dataframe from a dataset of an HDF5 file.

    :param hfile: (string) file from which to read
    :param basegroup: (string) path of group from which to read
    :param hdset: (string) dataset name to extract
    :param mapper: (dict optional) a dictionnary used to remap
                   names from the dataset Help group to something
                   usable in the dtype defintion
    :param compo: (function optional) in case of field storing several componenents
                  it is possible to specifiy how to generate the name of each componenent.
    :return: a pandas dataframe
    """

    with h5py.File(hfile, 'r') as hf:

        # the path of group to read and value associated
        data_path = "/".join( (basegroup,dset) )
        data = hf[data_path][()]

        # sizing column list and make sure mapper is a dict
        data_col = [None,]*data.shape[1]
        if not mapper:
          mapper = {}

        # a dictionnary to store the mapping to apply on parameters
        field_to_replace = {}

        # the magic regex to fetch field name and components
        regex = re.compile( r'(?P<name>(\w|\s)*)(\((?P<comp>(\s?\w*,?)*)\))?' )

        # running through all value of Help
        help_group = 'Help/'+dset
        for k in hf[help_group].keys() :
          help_path = help_group+'/'+k
          ihelp = hf[help_path+'/name'][()].decode('utf8')
          ibeg  = hf[help_path+'/bound'][0]-1
          iend  = hf[help_path+'/bound'][1]

          # matching the pattern : field (comp1, comp2)
          match = regex.search(ihelp)
          name  = match['name'].strip()

          # if there is a description of componenents, build from 'compo' function
          if match['comp']:
              list_comp = match['comp'].split(',')
              for comp, i in zip( list_comp, range(ibeg,iend) ):
                  data_col[i] = compo(name,comp)
                  if name in mapper.keys():
                      field_to_replace[data_col[i]] = mapper[name]
          else:
              # if there is no description of components, build from 'compo' function
              if ibeg+1 != iend:
                for i, ind in enumerate(range(ibeg,iend)):
                  data_col[ind] = compo(name,str(i))
                  if name in mapper.keys():
                    field_to_replace[data_col[ind]] = mapper[name]
              # else use name directly
              else:
                data_col[ibeg] = name
                if name in mapper.keys():
                  field_to_replace[name] = mapper[name]

  
        # make a dataFrame from data and list of columns generated before
        df = pd.DataFrame( data, columns=data_col )

        # remap needed field
        for field, map_field in field_to_replace.items():
            df[field] = df.loc[:,field].apply(lambda i: map_field[i])

    return df
