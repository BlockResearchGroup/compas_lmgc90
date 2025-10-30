
import numpy as np
import h5py

from pylmgc90 import chipy

chipy.Initialize()
chipy.SetDimension(2)
chipy.ReadDatbox(deformable=False)

h5_file   = 'lmgc90.h5'
id_record = 1
chipy.ReadIni(id_record,h5_file)

inters = chipy.getInteractions()

print( "inters dtype : ", inters.dtype )
print( "inters.shape : ", inters.shape)
print( "inters['uc'].shape : ", inters['uc'].shape)

print( "mean value of rln on all interaction" )
print( np.mean(inters['rl'][:,1]) )

print( "list all status present in inters['status'] array : ")
status_list = np.unique( inters['status'] )
print(status_list)

# extracting contact with status different from 'noctc'
with_ctc = inters['status'] != b'noctc'

print( "mean value of rln on all interaction really in contact" )
print( np.mean(inters['rl'][with_ctc][:,1]) )

# define a function checking
def in_boundary(c):
  l_bound = np.array([0.5, 0.5])
  u_bound = np.array([5.0, 5.0])
  # do not work you can debug using debugger's breakpoint
  #breakpoint()
  #mask = c > l_bound and c < u_bound
  # solution
  mask = np.all(c > l_bound) and np.all(c < u_bound)
  return mask

is_in = in_boundary(inters['coor'][0])
print( f"{inters['coor'][0]} is in boundary : {is_in}" )
