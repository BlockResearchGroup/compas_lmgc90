
import os, shutil
import random
import filecmp

import numpy as np

from pylmgc90 import pre

pre.setStopMode('exception')

if( not os.path.isdir('DATBOX') ):
   os.mkdir('DATBOX')


dim = 3
mats = pre.materials()

bulkBehavOptions = pre.config.lmgc90dicts.bulkBehavOptions
joint_mat = ['JOINT_ELAS', 'JOINT_MC', 'JOINT_FCZM',]

# generate random value for all available
# options of behav :
allOptions = { opt : random.random() \
               for val in bulkBehavOptions.values() \
               for opt in val \
             }

# override of some options which are not floats:
allOptions['anisotropy'] = 'isotropic'
for opt in ('masses', 'stiffnesses', 'viscosities'):
    allOptions[opt] = np.random.random( [dim] )
allOptions['file_mat'] = 'totor.mat'
# not real choices here...
allOptions['elas'] = 'standard'
allOptions['viscous_model'] = 'KelvinVoigt'
allOptions['critere'] = 'Von-Mises'
allOptions['isoh'] = 'linear'
#allOptions['cinh'] = 'linear'
allOptions['cinh'] = 'none'
#allOptions['visc'] = 'power_law'
allOptions['visc'] = 'none'

joint_opt = { 'consolidation':2, 'mc':4, 'fczm':10, }
for opt, s in joint_opt.items():
  allOptions[opt] = np.random.random([s])

# like touch....
with open("DATBOX/totor.mat",'w'):
    pass

# adding all material types to materials container
count = 0
for mat, opts in bulkBehavOptions.items():
    count += 1
    name = "m{:04d}".format(count)

    opt = { o:allOptions[o] for o in opts }
    # groumpf...
    if mat in joint_mat:
       opt['stiffnesses'] = opt['stiffnesses'][:3]

    new_mat = pre.material(name=name, materialType=mat, **opt)
    mats.addMaterial(new_mat)


# first write from pre
pre.writeBulkBehav(mats, chemin='./DATBOX/', dim=dim)
bulk_dat = os.path.join('DATBOX', 'BULK_BEHAV.DAT')

# check that the file written by pre is read correctly:
from pylmgc90.pre.IO import file2BulkBehav
mats2, gravy2 = file2BulkBehav.read_bulk_behav(dim, 'DATBOX' )
if( not os.path.isdir('DATBOX2') ):
   os.mkdir('DATBOX2')
pre.writeBulkBehav(mats2, chemin='./DATBOX2/', dim=dim, gravy=gravy2)
bulk_dat2 = os.path.join('DATBOX2','BULK_BEHAV.DAT')
assert filecmp.cmp(bulk_dat,bulk_dat2,shallow=False), "BULK_BEHAV.DAT file written by pre is not read correctly"


# trying to read/write these laws
from pylmgc90 import chipy

chipy.Initialize()
chipy.checkDirectories()
chipy.SetDimension(dim)

chipy.bulk_behav_ReadBehaviours()
chipy.bulk_behav_WriteBehaviours()

bulk_out = os.path.join('OUTBOX', 'BULK_BEHAV.OUT')
shutil.copy( bulk_out, bulk_dat )

chipy.bulk_behav_CleanMemory()
chipy.bulk_behav_ReadBehaviours()
chipy.bulk_behav_WriteBehaviours()

assert( filecmp.cmp(bulk_dat,bulk_out,shallow=False) )

# check that the file written by chipy is read correctly:
mats3, gravy3 = file2BulkBehav.read_bulk_behav(dim, 'DATBOX' )
# backup TACT_BEHAV.OUT file in DATBOX2
shutil.copy( bulk_out, bulk_dat2 )
pre.writeBulkBehav(mats3, chemin='./DATBOX/', dim=dim, gravy=gravy3)
chipy.bulk_behav_CleanMemory()
chipy.bulk_behav_ReadBehaviours()
chipy.bulk_behav_WriteBehaviours()

assert filecmp.cmp(bulk_out,bulk_dat2,shallow=False), "BULK_BEHAV.DAT file written by chipy in not read correctly"

