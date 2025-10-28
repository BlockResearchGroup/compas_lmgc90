
import math

import numpy as np
from scipy.spatial import ConvexHull

def geometric_center(a):
     """
     compute geometrique center of a polygon
     """

     nb_lig = a.shape[0]
     G  = np.sum(a,axis=0)
     G /= nb_lig

     return G

def centroid(a, ptx=None):
    """
    Compute centroid of a polygon, and area as a side effect
    source : http://paulbourke.net/geometry/polygonmesh/
    """

    ptx = [0,a.shape[0]] if ptx is None else ptx

    g = np.zeros( [2] )
    area = 0.
    for i in range( len(ptx)-1 ):
        ibeg = ptx[i  ]
        iend = ptx[i+1]
        coor = a[ibeg:iend]

        cross = np.cross( coor[:-1,:], coor[1:,:] )
        area += np.sum( cross )
        g[:] += np.sum( (coor[:-1,:]+coor[1:,:]) * cross[:,np.newaxis], axis=0 )

        # closing polygon
        cross = np.cross(coor[-1,:], coor[0,:])
        g[:] += cross * (coor[-1,:]+ coor[0,:])
        area += cross

    area *= 0.5
    g[:] /= 6*area

    return g, area


def area(a, ptx=None):
    """
    Compute area of a list of polygon
    source : http://paulbourke.net/geometry/polygonmesh/
    ptx a is list of size number of polytope + 1, to access
    polytope i, use a[ptx[i]:ptx[i+1]]
    """

    ptx = [0,a.shape[0]] if ptx is None else ptx

    area = 0.
    for i in range( len(ptx)-1 ):
        ibeg = ptx[i  ]
        iend = ptx[i+1]
        coor = a[ibeg:iend]

        cross = np.cross( coor[:-1,:], coor[1:,:] )
        area += np.sum( cross )
        # closing polygon
        area += np.cross( coor[ -1,:], coor[0 ,:] )

    return area/2.


def moment_inertia(a, ptx=None, already_centered=False):
    """
    Compute inertia of polygon along x and y axis, relatively to centroid
    source : http://paulbourke.net/geometry/polygonmesh/
    """

    ptx = [0,a.shape[0]] if ptx is None else ptx
 
    polyg = a
    if not already_centered:
        g, _ = centroid(a)
        polyg = a[:,:] - g[:]

    I = np.zeros([2])
    Im= 0.
    for i in range( len(ptx)-1 ):
        ibeg = ptx[i  ]
        iend = ptx[i+1]
        c    = polyg[ibeg:iend,:]
        c2   = c[:,:]*c[:,:]
        cxy  = c[:,0]*c[:,1]*2.

        cross = np.cross(c[:-1,:], c[1:,:])
        I[:] += np.sum( cross[:,np.newaxis] * ( c2[:-1,:] + c[:-1,:]*c[1:,:] + c2[1:,:] ), axis=0 )
        Im   += np.sum( cross[:           ] * ( c[:-1,0]*c[1:,1] + cxy[:-1]+cxy[1:] + c[1:,0]*c[:-1,1] ) )
        # closing polygon
        cross = np.cross(c[-1,:], c[0,:])
        I[:] += cross * ( c2[-1,:] + c[-1,:]*c[0,:] + c2[0,:] )
        Im   += cross * ( c[-1,0]*c[0,1] + cxy[-1]+cxy[0] + c[0,0]*c[-1,1] )

    I[:] /= 12.
    Im   /= 24.

    I[0], I[1] = I[1], I[0]
    iframe = np.eye( 2 )

    # now to diagonalize
    # https://en.wikipedia.org/wiki/Eigenvalue_algorithm#2%C3%972_matrices
    #
    if np.isclose(Im,0.):
        iframe = np.eye( 2 )
        iframe0= np.diag(I)
        iframe0[1,0] = iframe0[0,1] = -Im
    else:
      iframe = np.diag(I)
      iframe[1,0] = iframe[0,1] = -Im
      iframe0 = iframe.copy()
      trace = I[0]+I[1]
      delta = np.sqrt( (I[0]-I[1])**2 + 4*Im*Im )
      I = np.array( [ 1.,-1] )
      I = 0.5 * ( trace + delta*I )
      iframe -= np.diag(I[::-1])
      iframe /= np.linalg.norm(iframe,axis=0)

    assert( np.allclose( np.matmul(iframe,iframe.T), np.eye(2) ) )
    assert( np.allclose( np.matmul(iframe,np.matmul(np.diag(I),iframe.T)), iframe0 ) ), f"\n{iframe}\n{I}\n{iframe0}"

    return I, iframe


def inertial_ellipse(a, t=None):

    t  = np.linspace(0., 2*np.pi, 101) if t is None else t
    S  = area(a)
    I  = moment_inertia(a)
    g  = centroid(a)
    ellipse = np.empty([t.size,2])

    ellipse[:,0]=math.sqrt(I[1]/S)
    ellipse[:,1]=math.sqrt(I[0]/S)
    ellipse[:,0] *= np.cos(t)
    ellipse[:,1] *= np.sin(t)
    ellipse[:,:] += g

    return ellipse

def antipolars(a, ptx=None, I=None):
    """
    Compute the antipolars from the vertices of a polygon
    Uses the convex hull and antipolars are provided as
    slope and intercept.
    Notes : an np.inf value for slope means a y-axis parallel line
    """

    g, S = centroid(a, ptx)
    polyg = a - g

    I, iframe  = moment_inertia(polyg, ptx, True) if I is None else I

    # first getting convex hull of polygon
    hull = ConvexHull(polyg)
    nb_l = hull.vertices.shape[0]

    # moving it in inertial frame
    vertices = np.matmul( hull.points[hull.vertices], iframe )

    # computing antipolars of inertial ellipse as slope and intercept
    h=np.empty([nb_l,2])
    h[:,0] = np.where( vertices[:,1]!=0., -I[0]*vertices[:,0] / (I[1]*vertices[:,1]), np.inf )
    h[:,1] = np.where( vertices[:,1]!=0., -I[0]/(S*vertices[:,1]), -I[1]/(S*vertices[:,0]) )

    return h , g, iframe


def central_kernel(a, ptx=None):

    ptx = [0,a.shape[0]] if ptx is None else ptx

    h, g, iframe = antipolars(a, ptx)
    h_b = h[:,0] != np.inf
    hp  = np.roll(h  ,-1,axis=0)
    hpb = np.roll(h_b,-1)

    nb_l = h.shape[0]
    inter= np.empty([nb_l,2])

    inter[:,0] = (hp[:,1]-h[:,1]) / (h[:,0]-hp[:,0])
    inter[:,0] = np.where( h_b, inter[:,0],  h[:,1])
    inter[:,0] = np.where( hpb, inter[:,0], hp[:,1])
    inter[:,1] = np.where( h_b, h[:,0]*inter[:,0]+ h[:,1],
                               hp[:,0]*inter[:,0]+hp[:,1]
                         )

    # moving intersection of antipolar into polygon orignal frame
    inter = np.matmul(inter,iframe.T) + g

    return inter
    

def space_mapping(a):
    """
    Compute the space mapping (translation and rotation)
    allowing to compute the polygon in its own frame

    The mapping is then computed with:
      new = np.matmul( frame, (old-offset).T ).T
      new = np.matmul( (old-offset), frame.T )
    The reverse mapping is then computed with:
      old = np.matmul( new, frame ) + offset
    """

    f1= a[1,:]-a[0,:]
    f1=f1/np.linalg.norm(f1)

    i = 2
    while i < a.shape[0] :
        v  = a[i,:]-a[0,:]
        f3 = np.cross(f1,v)
        n3 = np.linalg.norm(f3)
        if n3 != 0.:
          break
        i=i+1

    if i == a.shape[0]:
      print('[space_mapping:error] all points aligned')
      raise ValueError

    f3 = f3/n3
    f2 = np.cross(f3,f1)

    frame  = np.array( [f1,f2,f3], dtype=float)
    offset = geometric_center( a )

    return frame, offset

  
def nc_each_rectangle(f2f):
    """
    Compute the central kernel of each surface of f2f using the fonction central_kernel
    """
    final_result = []
    i_f2f = 0
    for coor, reac, rn in f2f:
        i_f2f += 1

        f, g = space_mapping(coor)
        new_coor = np.matmul(f,(coor-g).T).T

        s_n   = sigma(new_coor, rn)

        last_l = np.zeros(new_coor.shape)
        last_l[:,:2] = central_kernel(new_coor[:,:2])
        xc    = center_pression(new_coor, rn)

        is_in = insidePolygon(last_l[:,:2], xc)

        new_last = np.matmul(f.T,last_l.T).T + g
        new_xc   = np.matmul(f.T,xc).T + g
        final_result.append( (new_last, s_n, new_xc, is_in,) )
    return final_result 

def pressure_center(coor, rn):
    """
    Compute the pressure center of the input polygon with associated normal reaction
    """
    s = np.sum(rn)
    return np.sum(coor*rn[:,np.newaxis],axis=0) / s if s != 0. else geometric_center(coor)


def sigma(rn,surf):
    """
    Compute the equivalent normal stress of each force applied on each surface of f2f
    """
    return np.sum(rn) / surf if surf != 0. else 0.

def is_inside(p,polyg):
    """
    Verifies that the input point is inside the provided polygon
    source : http://paulbourke.net/geometry/polygonmesh/
    """
    nb_polyg = len(polyg)
    compteur=0
    p1=polyg[0]
    for i in range (1,nb_polyg+1):
        p2=polyg[i % (nb_polyg)]
        if (p[1]  > min(p1[1],p2[1])   and
           (p[1] <= max (p1[1],p2[1])) and
           (p[0] <= max(p1[0],p2[0]))  and
           (p1[1] != p2[1]) ):
            xinters = (p[1]-p1[1])*(p2[0]-p1[0])/(p2[1]-p1[1])+p1[0]
            if (p1[0] == p2[0]) or (p[0] <= xinters):
                compteur += 1
        p1=p2
        
    if (compteur % 2 == 0):
        return False
    else:
        return True    


