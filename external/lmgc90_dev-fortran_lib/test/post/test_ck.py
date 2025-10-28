import itertools

import numpy as np
from scipy import spatial
from matplotlib import pyplot as plt

from pylmgc90.post import central_kernel as ck


def axisangle2matrix( axe, angles ):
    norm = np.linalg.norm(axe)
    axe = axe/norm if not np.isclose(norm, 0.) else axe
    un = np.eye(3)
    sk = np.cross( un, axe )
    ou = np.outer( axe, axe)
    ro = [ np.cos(a)*un + np.sin(a)*sk + (1-np.cos(a))*ou for a in angles ]

    return ro


def plot_polygs( ax, *polyg ):
    for p in polyg:
      # close polyg:
      p = np.vstack( (p[:,:], p[:1,:]) )
      ax.plot(p[:,0], p[:,1], p[:,2])


polyg0 = np.array([[-0.5, -0.5, 0.],
                   [ 0.5, -0.5, 0.],
                   [ 0.5,  0.5, 0.],
                   [-0.5,  0.5, 0.],
                  ])

refck0 = np.array( [[ 0.0,  0.5, 0.],
                    [-0.5,  0.0, 0.],
                    [ 0.0, -0.5, 0.],
                    [ 0.5,  0.0, 0.],
                   ])
refck0[:,:] /= 3.


dila = np.zeros([3])
tran = np.zeros([3])
axer = np.array([1.,0.,0.])
angr = 0.

eye  = np.eye( 3 )

all_dila = [ np.array([1.,1.,0.]), np.array([3.,6.,0.]), np.array([6.,3.,0.]) ]
#all_dila = [ np.array([6.,3.,0.]) ]
all_tran = [ np.zeros([3]), np.ones([3]), np.array([-3., -4., -6]) ]
#all_tran = [ np.array([-3., -4., -6]) ]
all_axer = [ eye[0], eye[1], eye[2], eye[0]+eye[1], eye[0]+eye[2], eye[1]+eye[2], eye[0]+eye[1]+eye[2] ]
#all_axer = [ eye[0]+eye[1] ]
all_angr = [ 0., np.pi/3., np.pi/2., 3*np.pi/4. ]

all_rota = []
for axer in all_axer:
  all_rota.extend( axisangle2matrix( axer, all_angr ) )

ax = plt.figure().add_subplot(projection='3d')
for dila, tran, rota in itertools.product(all_dila, all_tran, all_rota):

  polyg = polyg0 * dila
  refck = refck0 * dila
  polyg = np.matmul(rota, polyg.T).T
  refck = np.matmul(rota, refck.T).T
  polyg+= tran
  refck+= tran
  newck = ck.macro.getck(polyg)
  msg = f"error computing ck with:\n  dila={dila},\n  t={tran},\n  rota=\n{rota}"
  assert np.allclose(refck,newck) or np.allclose(refck,np.roll(newck,-1,0)) or np.allclose(refck,np.roll(newck,-2,0)), msg

  plot_polygs(ax, polyg, newck)

#plt.show()

polyg0 = np.array([[-2., -2., 0.],
                   [-1., -2., 0.],
                   [-1.,  2., 0.],
                   [-2.,  2., 0.],
                   [ 1., -2., 0.],
                   [ 2., -2., 0.],
                   [ 2.,  2., 0.],
                   [ 1.,  2., 0.],
                  ])


ax = plt.figure().add_subplot(projection='3d')
for rota in all_rota:
  polyg = np.matmul(rota, polyg0.T).T
  newck = ck.macro.getck(polyg, [0,4,8])
  plot_polygs(ax, polyg, newck)

#plt.show()

polyg0 = np.array([[-2. ,  0.2, 0.],
                   [-2. ,  0. , 0.],
                   [-0.2,  0. , 0.],
                   [-0.2, -2. , 0.],
                   [ 0.2, -2. , 0.],
                   [ 0.2,  0. , 0.],
                   [ 2. ,  0. , 0.],
                   [ 2. ,  0.2, 0.],
                  ])

ax = plt.figure().add_subplot(projection='3d')
for rota in all_rota:
  polyg = np.matmul(rota, polyg0.T).T
  newck = ck.macro.getck(polyg)
  plot_polygs(ax, polyg, newck)

#plt.show()

polyg0 = np.array( [[-1e-0,  0e-0, 0.,],
                    [-5e-1,  1e-0, 0.,],
                    [-1e-1,  2e-1, 0.,],
                    [-1e-1, -2e-1, 0.,],
                    #[-5e-1,  6e-1, 0.,],
                    [-5e-1,  6e-1, 0.,],
                    [-1e-0, -4e-1, 0.,],
                    #[ 5e-1,  6e-1, 0.,],
                    [ 5e-1,  6e-1, 0.,],
                    [ 1e-1, -2e-1, 0.,],
                    [ 1e-1,  2e-1, 0.,],
                    [ 5e-1,  1e-0, 0.,],
                    [ 1e-0,  0e-0, 0.,],
                    [ 1e-0, -4e-1, 0.,],
                   ] )

ax = plt.figure().add_subplot(projection='3d')
newck1= ck.macro.getck(polyg0[:6])
newck2= ck.macro.getck(polyg0[6:])
newck = ck.macro.getck(polyg0,[0,6,12])
plot_polygs(ax, polyg0, newck1, newck2, newck)
#plt.show()

