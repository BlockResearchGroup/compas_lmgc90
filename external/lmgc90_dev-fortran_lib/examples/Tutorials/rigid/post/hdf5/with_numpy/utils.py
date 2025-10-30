import h5py
import numpy as np

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


def get_numpy_frame(hfile, basegroup, hdset, mapper=None):
    """
    Generate and return a numpy array with dedicated dtype
    dataframe from a dataset of an HDF5 file.

    :param hfile: (string) file from which to read
    :param basegroup: (string) path of group from which to read
    :param hdset: (string) dataset name to extract
    :param mapper: (dict optional) a dictionnary used to remap
                   names from the dataset Help group to something
                   usable in the dtype defintion
    :return: a 1D numpy array of a dtype automatically build
             from the content of the Help
    """

    with h5py.File(hfile, 'r') as hf:

        # the path of group to read and value associated
        data_path = "/".join( (basegroup,hdset) )
        data = hf[data_path][()]

        if not mapper:
          mapper = {}

        data_col = {}
        help_group = 'Help/'+hdset
        for k in hf[help_group].keys():
            help_path = help_group+'/'+k
            ihelp = hf[help_path+'/name'][()].decode('utf8')
            ibeg  = hf[help_path+'/bound'][0]-1
            iend  = hf[help_path+'/bound'][1]

            name = ihelp.split(' ')[0]
            if name in mapper:
              repl = mapper[name]
              daty = (f'S{len(mapper[name][1])}', (iend-ibeg,),)
            else:
              repl = None
              daty = (data.dtype,(iend-ibeg,),)
            data_col[ibeg] = (name,daty,ibeg,iend,repl)

        # generting datatype
        data_type = np.dtype([ data_col[k][:2] for k in sorted(data_col) ])
        # init empty array
        nf = np.empty( data.shape[0], dtype=data_type )
        # fill array by copy/mapping
        # name, type, ibeg, iend, replace
        for n, t, ib, ie, r in data_col.values():
            if ib+1 != ie:
              if r :
                for i, idx in enumerate(range(ib, ie)):
                  nf[n][:,i] = np.fromiter( (r[v] for v in data[:,idx]), dtype=t[0] )
              else:
                nf[n][:,:] = data[:,ib:ie]
            else:
              if r:
                nf[n][:] = np.fromiter( (r[v] for v in data[:,ib]), t )
              else:
                nf[n][:,0] = data[:,ib]

    return nf


