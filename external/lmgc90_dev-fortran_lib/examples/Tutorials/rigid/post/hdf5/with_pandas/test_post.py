
import h5py
import pandas as pd

from utils import getDataFrame

hf = h5py.File('lmgc90.h5', 'r')

# getting the number of time steps saved
nb_record = int( hf['Simulation/nb_record'][()] )
id_record = 1
assert(id_record <= nb_record), "[ERROR] wrong id"

# making dictionnary for each parameters
basepath = hf['Help/parameters']
parameters = {}
for k in basepath.keys():
    parameters[k] = dict(zip(basepath[k+'/id'][()],
                             map(bytes.decode, basepath[k+'/name'][()])
                             )
                         )
## error management...
#for p in parameters[()]s():
#  p[0] = '?????'

# generating a datafram of all interactions

basegroup = "Evolution/ID_"+str(id_record)


def idata_compo(name, comp):
    return comp.strip() + ' ' + name if name else comp.strip()

print('getting idata vlocrloc')
hgroup = 'VlocRloc/idata'
iinter = getDataFrame(hf, basegroup, hgroup, mapper=parameters, compo=idata_compo)
print('getting rdata vlocrloc')
hgroup = 'VlocRloc/rdata'
rinter = getDataFrame(hf, basegroup, hgroup, compo=lambda n, c: n+"_"+c)
interactions = pd.concat([iinter, rinter], axis=1)

# getting description on 'DKJCx' interactions...
#dkjcx = interactions[ interactions.loc[:,'inter_id'] == 'DKJCx' ]
#print( dkjcx.loc[:,('rl_t','rl_n')].describe() )
#interactions.to_csv('inters.csv')

print('getting idata rbdy2')
hgroup = 'RBDY2/idata'
ibody  = getDataFrame( hf, basegroup, hgroup, mapper=parameters, compo=idata_compo )
print('getting rdata rbdy2')
hgroup = 'RBDY2/rdata'
rbody  = getDataFrame( hf, basegroup, hgroup, compo=lambda n,c:n+"_"+c)
bodies = pd.concat( [ibody,rbody], axis=1 )

#print('getting idata mecax')
#hgroup = 'MAILx/mecax/idata'
#imeca  = getDataFrame( hf, basegroup, hgroup, mapper=parameters, compo=idata_compo )
#print('getting rdata mecax')
#hgroup = 'MAILx/mecax/rdata'
#rmeca  = getDataFrame( hf, basegroup, hgroup, compo=lambda n,c:n+"_"+c)
#bodies = pd.concat( [imeca,rmeca], axis=1 )
#print('getting flux mecax')
#hgroup = 'MAILx/mecax/flux'
#fmeca  = getDataFrame( hf, basegroup, hgroup, compo=lambda n,c:n+"_"+c)

# do not forget to close !!!
hf.close()


# list interactions columns
print( interactions.columns )

# counting each type of interactions:
inter_by_type = interactions.groupby('inter_id')
inter_by_type['icdan'].count()


# adjacence table ?
list_cd = interactions.groupby('cd ibdyty')
for cd, list_an in list_cd.groups.items():
  #print( cd, len(list_an) )
  an_id = interactions.loc[list_an,'an ibdyty']
  print( f"candidate {cd} has {len(list_an)} antagonist :" )
  print( an_id.to_string(index=False) )

