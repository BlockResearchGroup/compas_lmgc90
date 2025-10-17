
from pathlib import Path

import numpy as np
import h5py

from matplotlib import pyplot as plt

wd = Path("3D_incre_Tangent")

# from hdf5 of inside computation ?
with h5py.File( wd/'lmgc90.h5', 'r' ) as hf:
    nb_record = int( hf['Simulation/nb_record'][()] )
    int_idx   = hf['Help/VlocRloc/rdata/internals/bound'][0] - 1 #Fortran to C
    nb_int    = hf['Help/VlocRloc/rdata/internals/bound'][1] - int_idx

    # because only one law !
    claw = hf['Help/parameters/inter_law/comment'][()].decode()
    claw = { n:i for i,n in enumerate(claw[1:].split()) }

    # index of internal starting with 0
    #d = 'n' if loading == 'Normal' else 't'
    #u_idx = int_idx + claw[f'saut_de_u{d}']
    #f_idx = int_idx + claw[f'energy']
    #b_idx = int_idx + claw[f'beta']

    e_u = np.zeros( [nb_int,nb_record], dtype=float )
    # should I assert that nb_inters == 1 ?
    for i in range(nb_record):
      if 'VlocRloc' not in hf[f"Evolution/ID_{i+1}"].keys() :
        break
      #e_u[0, i] = hf[f"Evolution/ID_{i+1}/VlocRloc/rdata"][0,u_idx]
      #e_u[1, i] = hf[f"Evolution/ID_{i+1}/VlocRloc/rdata"][0,f_idx]
      #e_u[2, i] = hf[f"Evolution/ID_{i+1}/VlocRloc/rdata"][0,b_idx]
      e_u[:, i] = hf[f"Evolution/ID_{i+1}/VlocRloc/rdata"][0,int_idx:]
