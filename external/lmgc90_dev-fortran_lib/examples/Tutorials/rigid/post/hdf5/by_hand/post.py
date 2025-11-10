from collections import defaultdict

import h5py
import numpy as np

from matplotlib import pyplot as plt


hf = h5py.File('../lmgc90.h5', 'r')

# getting the number of time steps saved
nb_record = hf['Simulation/nb_record'][()]
id_record = 1
assert( id_record <= nb_record ), "[ERROR] wrong id"

# making dictionnary for each parameters
basepath = hf['Help/parameters']
parameters = {}
for k in basepath.keys() :
    parameters[k] = dict( zip( map(bytes.decode,basepath[k+'/name'][()]), basepath[k+'/id'][()] ) )
rev_parameters = {}
for k in basepath.keys() :
    rev_parameters[k] = dict( zip( basepath[k+'/id'][()],  map(bytes.decode,basepath[k+'/name'][()]) ) )

# making dictionnary of field for idata
# and shifting indices to be python style
ik = {}
for k in hf['Help/VlocRloc/idata'].keys() :
    ik[k] = ( hf['Help/VlocRloc/idata/'+k+'/name'][()],
              hf['Help/VlocRloc/idata/'+k+'/bound'][()][0]-1,
              hf['Help/VlocRloc/idata/'+k+'/bound'][()][1]-1
            )

# making dictionnary of field for rdata
# and shifting indices to be python style
rk = {}
for k in hf['Help/VlocRloc/rdata'].keys() :
    rk[k] = ( hf['Help/VlocRloc/rdata/'+k+'/name'][()],
              hf['Help/VlocRloc/rdata/'+k+'/bound'][()][0]-1,
              hf['Help/VlocRloc/rdata/'+k+'/bound'][()][1]-1
            )


# preparing a dict for each disk storing the adjacent disk
adj_map = defaultdict( list )

# getting the correct group for desired record
basepath = hf["Evolution/ID_"+str(id_record)]
idata =  basepath['VlocRloc/idata'][()]
rdata =  basepath['VlocRloc/rdata'][()]

# really really paranoid
assert( idata.shape[0] == rdata.shape[0] )
print( "time step", id_record, " -> nb_inter = ", idata.shape[0] )

# using some variable name
idx_Rn = rk['rl'][2]
idx_cd_tactype = ik['tactype'][1]
idx_an_tactype = ik['tactype'][2]

for inter_i, inter_r in zip(idata,rdata) :

  # skipping JONCx as antagonist
  if inter_i[ idx_an_tactype ] == parameters['tactype']['JONCx'] :
    print( inter_i[1], ' has an antagonist JONCx' )
    continue

    # a contactor is identified by its type (DISKx, JONCx, etc) and its index in this type
  cd = ( inter_i[ idx_cd_tactype ], inter_i[ ik['itacty'][1] ] )
  an = ( inter_i[ idx_an_tactype ], inter_i[ ik['itacty'][2] ] )

  if cd not in adj_map.keys() or an not in adj_map[cd] :
    # if Rn > 0.
    if inter_r[ idx_Rn ] > 0. :
      adj_map[ cd ].append( an )
      adj_map[ an ].append( cd )


coordination_number = { contactor[1] : len(adjac) for contactor, adjac in adj_map.items() }
nbc = np.array( [*coordination_number[()]s()] )
print( np.unique(nbc,return_counts=True) )

plt.hist( nbc )
plt.show()

# do not forget to close !!!
hf.close()
