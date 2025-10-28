from matplotlib import pyplot as plt
import numpy as np
import h5py

from pylmgc90 import chipy

h5_file = 'lmgc90.h5'
with h5py.File(h5_file) as hf:
    nb_record = int( hf['Simulation/nb_record'][()] )
    dim = int( hf['Simulation/dimension'][()] )

chipy.Initialize()
chipy.checkDirectories()

chipy.Initialize()

chipy.SetDimension(dim)

chipy.ReadDatbox(deformable=False)

chipy.ComputeMass()

start = 1
start = nb_record
end   = nb_record+1
for k in range(start,end):

  chipy.ReadIni(k,h5_file)
  chipy.ComputeFext()
  chipy.ComputeRnod()

  this   = chipy.getInteractions(True)
  verlet = chipy.getInteractions()

  # sanity check
  assert np.all( this == verlet ), 'difference between verlet and this interactions'

# select only contact with status not 'noctc'
i_ctc = verlet['status'] != b'noctc'
verlet_ctc = verlet[i_ctc]

# display bar plot of contacts by type:
ctc_counts = np.unique( verlet_ctc['inter'], return_counts=True )
plt.bar( *ctc_counts )
plt.title( 'contact types' )
plt.show()

# compute multiplicity of all contacts
header = ['cdbdy',  'icdbdy',  'cdtac',  'icdtac',  'anbdy',  'ianbdy',  'antac',  'iantac', ]
multiplicity = np.unique( verlet_ctc[header], return_counts=True )
mult = np.unique( multiplicity[1], return_counts=True )
plt.bar( mult[0], mult[1], tick_label=mult[0] )
plt.title( 'multiplicity of all contacts' )
plt.show()

# compute multiplicity of PRPRx contacts
i_prprx = verlet_ctc['inter'] == b'PRPRx'
verlet_prprx = verlet_ctc[i_prprx]
multiplicity = np.unique( verlet_prprx[header], return_counts=True )
mult = np.unique( multiplicity[1], return_counts=True )
plt.bar( mult[0], mult[1], tick_label=mult[0] )
plt.title( 'multiplicity of PRPRx contacts' )
plt.show()


chipy.Finalize()


