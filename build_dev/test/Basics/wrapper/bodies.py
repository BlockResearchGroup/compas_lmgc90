import sys
import itertools
import random

from pathlib import Path

import numpy as np

from pylmgc90 import pre
from pylmgc90 import chipy
from pylmgc90.chipy import computation
  
def gen(dim, nb=1000):

  pre.setStopMode('exception')

  # generate a simple 
  datbox = Path('DATBOX')
  datbox.mkdir(exist_ok=True)
  
  bodies = pre.avatars()
  mods   = pre.models()
  mats   = pre.materials()
  tacts  = pre.tact_behavs()
  sees   = pre.see_tables()
  
  # generate a rigid case:
  mod = pre.model(name='rigid', physics='MECAx', element=f"Rxx{dim}D", dimension=dim)
  mods.addModel(mod)

  mat = pre.material(name='TDURx', materialType='RIGID', density=1000.)
  mats.addMaterial(mat)

  radius = 1.
  nb_per_axis = int( round( nb**(1./3.) ) )
  generator = pre.rigidDisk if dim==2 else pre.rigidSphere
  for i in itertools.product(range(nb_per_axis),repeat=dim):
    coor = np.array(i)*2*radius
    mrad = radius+0.1*random.random()
    body = generator(r=mrad, center=coor, model=mod, material=mat, color='BLUEx')
    bodies.addAvatar(body)

  # definition d'une loi de contact frottant, avec pre-gap
  iqsc0=pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.5)

  tacts.addBehav(iqsc0)

  tactor = 'DISKx' if dim == 2 else 'SPHER'

  sv1 = pre.see_table(CorpsCandidat=f"RBDY{dim}", candidat=tactor, colorCandidat='BLUEx', behav=iqsc0,
                      CorpsAntagoniste=f"RBDY{dim}", antagoniste=tactor, colorAntagoniste='BLUEx', alert=0.1*radius)

  sees.addSeeTable(sv1)

  try:
    pre.visuAvatars(bodies)
  except:
    pass

  # ecriture des fichiers de donnees pour LMGC90
  pre.writeDatbox( dim, mats, mods, bodies, tacts, sees )


def run_test(dim):

  gen(dim)
  
  dt = 1e-2
  theta = 0.5
  
  computation.initialize(dim, dt, theta)
  
  # not tested yet
  # get only field:
  #get_only_field = [ "Coor_", "Coorb", "Coorm", "Vaux_",
  #                   "Ireac", "Iaux_", "Fext_", "Fint_",
  #                 ]
  
  # g/set field
  set_get_field = [ "Coor0", "X____", "Xbeg_", "V____",
                    "Vbeg_", "Vfree", "Reac_", "Raux_",
                    "Ireac", "Iaux_", "Fext_",
                  ]
  
  
  chipy.IncrementStep()
  chipy.ComputeFext()
  
  if dim == 2:
    get = chipy.RBDY2_GetAllBodyVector
    put = chipy.RBDY2_PutAllBodyVector
  else:
    get = chipy.RBDY3_GetAllBodyVector
    put = chipy.RBDY3_PutAllBodyVector
  
  for field in set_get_field:
  
    cf = get(field)
  
    nf = cf.copy()
    nf[:,1] = 0.5*cf[:,2]+np.random.randint(2, size=nf.shape[0])
  
    put(field, nf)
    if field == 'Fext_':
      nf[:,:] = nf[:,:]+cf[:,:]
  
    cf = get(field)
  
      
    assert np.all( nf==cf ), f"diff in Put/Get for field {field}"


if __name__ == "__main__":

  dim = 2 if '2d' in sys.argv else 3
  run_test(dim)
