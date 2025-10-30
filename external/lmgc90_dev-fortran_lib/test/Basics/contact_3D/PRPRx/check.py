from __future__ import print_function

import math, numpy as np

from pylmgc90 import chipy

def do_check( ref_nb, ref_coor, ref_normal, tol):

  assert( chipy.inter_handler_3D_getNb( chipy.PRPRx_ID ) ==  ref_nb )

  all_inter = chipy.inter_handler_3D_getAll( chipy.PRPRx_ID )
  assert np.allclose( all_inter[:,0:3], ref_coor  , atol=tol ), print(all_inter[:,0:3]-ref_coor  )
  assert np.allclose( all_inter[:,6:9], ref_normal, atol=tol ), print(all_inter[:,6:9]-ref_normal)


def face_face_1(ray, dist):

  ref_coor   = np.array([[ -ray,  ray, ray+dist*0.5 ],
                         [  ray,  ray, ray+dist*0.5 ],
                         [  ray, -ray, ray+dist*0.5 ],
                         [ -ray, -ray, ray+dist*0.5 ]
                       ])
  ref_normal = np.array([[0., 0.,-1.],
                         [0., 0.,-1.],
                         [0., 0.,-1.],
                         [0., 0.,-1.] 
                       ])

  do_check( 4, ref_coor, ref_normal, 1.e-18)


def face_face_2(ray, dist):

  ref_coor   = np.array([[ -ray    ,  ray*0.5, ray+dist*0.5 ],
                         [  ray*0.5,  ray*0.5, ray+dist*0.5 ],
                         [  ray*0.5, -ray    , ray+dist*0.5 ],
                         [ -ray    , -ray    , ray+dist*0.5 ]
                       ])
  ref_normal = np.array([[0., 0.,-1.],
                         [0., 0.,-1.],
                         [0., 0.,-1.],
                         [0., 0.,-1.] 
                       ])

  do_check( 4, ref_coor, ref_normal, 1.e-16 )

def face_face_3(ray, dist):
  ref_connec = np.array( [1, 1, 8], dtype=int )
  ref_coor   = np.array( [[-0.3       , -0.22546922,  0.325     ],
                          [-0.3       ,  0.10892519,  0.325     ],
                          [-0.17731227,  0.3       ,  0.325     ],
                          [-0.00788039,  0.3       ,  0.325     ],
                          [ 0.3       ,  0.10231227,  0.325     ],
                          [ 0.3       , -0.06711961,  0.325     ],
                          [ 0.15046922, -0.3       ,  0.325     ],
                          [-0.18392519, -0.3       ,  0.325     ]]
                       )

  f2f_connec, f2f_coor = chipy.PRPRx_GetF2fOutlines()

  assert np.all( ref_connec==f2f_connec )
  assert np.all( np.isclose( ref_coor, f2f_coor, atol=1e-18 ) ), f"diff : {ref_coor-f2f_coor}"

def face_face_4(ray, dist):
  ref_connec = np.array( [1, 2, 4, 4], dtype=int )
  ref_coor   = np.array( [[ 1e-01, -1.5e-09, 3.25e-01],
                          [ 3e-01, -1.5e-09, 3.25e-01],
                          [ 3e-01, -3.0e-01, 3.25e-01],
                          [ 1e-01, -3.0e-01, 3.25e-01],
                          [-3e-01, -1.5e-09, 3.25e-01],
                          [-1e-01, -1.5e-09, 3.25e-01],
                          [-1e-01, -3.0e-01, 3.25e-01],
                          [-3e-01, -3.0e-01, 3.25e-01],
                         ] )

  f2f_connec, f2f_coor = chipy.PRPRx_GetF2fOutlines()

  assert np.all( ref_connec==f2f_connec )
  assert np.all( np.isclose( ref_coor, f2f_coor, atol=1e-08 ) ), f"diff : {ref_coor-f2f_coor}"


def edge_face_1(ray, dist):

  ref_coor   = np.array([[ -ray,  0., ray ],
                         [  ray,  0., ray ],
                       ])
  ref_normal = np.array([[0., 0.,-1.],
                         [0., 0.,-1.],
                       ])

  do_check( 2, ref_coor, ref_normal, 1.e-16)


def vert_face_1(ray, dist):

  ref_coor   = np.array([[ 0.,  0., ray ]])
  ref_normal = np.array([[0., 0.,-1.]])

  do_check( 1, ref_coor, ref_normal, 1.e-17 )


def edge_edge_1(ray, dist):
  ref_coor   = np.array([[  ray,  0., ray*math.sqrt(2.)+dist ],
                         [ -ray,  0., ray*math.sqrt(2.)+dist ],
                       ])
  ref_normal = np.array([[0., 0.,-1.],
                         [0., 0.,-1.],
                       ])

  do_check( 2, ref_coor, ref_normal, 1.e-8 )


def edge_edge_2(ray, dist):

  ref_coor   = np.array([[ 0.,  0., ray*math.sqrt(2.)+dist*0.5 ]])
  ref_normal = np.array([[0., 0.,-1.]])

  do_check( 1, ref_coor, ref_normal, 1.e-8 )


def vert_edge_1(ray, dist):

  ref_coor   = np.array([[ 0.,  0., ray*math.sqrt(2.)+dist*0.5 ]])
  ref_normal = np.array([[0., 0.,-1.]])

  do_check( 1, ref_coor, ref_normal, 1.e-9 )


def vert_vert_1(ray, dist):

  ref_coor   = np.array([[ 0.,  0., ray+dist*0.5 ]])
  ref_normal = np.array([[0., 0.,-1.]])

  do_check( 1, ref_coor, ref_normal, 1.e-18 )


