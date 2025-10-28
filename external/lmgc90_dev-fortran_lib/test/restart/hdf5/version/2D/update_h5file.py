import numpy as np
import h5py

i_p2p2l = 11
i_mailx = 3

h5_files = ('v0_1.h5', 'v0_2.h5',)
for h5_file in h5_files:

  hf = h5py.File(h5_file, 'r+')

  int_id = hf['Help/VlocRloc/idata/inter_id/bound'][()] - 1
  bdyty  = hf['Help/VlocRloc/idata/bdyty/bound'][()] - 1

  for record in  hf['Evolution']:
    idata = hf['Evolution/'+record+'/VlocRloc/idata'][()]
    p2p2l = np.where( idata[:,int_id] == i_p2p2l )[0]
    for i in p2p2l:
        hf['Evolution/'+record+'/VlocRloc/idata'][i,bdyty] = i_mailx
  hf.close()

