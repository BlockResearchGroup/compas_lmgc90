
import numpy as np

from . import utils

def inters2polyg( f2f_c, f2f_p, f2f_i, inters_prprx ):
    """
    Generate the list of polygons (of reactions)
    and polytopes (of contact surface) from
    the PRPRx interactions array of LMGC90
    (numpy structured array)

    TODO : currrently there is an inconstency between f2f_c/p and f2f_i
           because the former allow to store non convex surface within its structure
           wereas the latter cannot.

    Inputs:
    -------

    - f2f_c : connectivity of f2f structure and its number of surfaces and number of points for each
    - f2f_p : the coordinates of the points of each surface of each f2f
    - f2f_i : the number and index list of interactions of each f2f

    Returns:
    --------

    A list of polygon each contains:
    - spatial coordinates of the contact surface
    - list of index for each non-connex polytopes
    - spatial coordinates of the contact points
    - total reaction in global frame
    - normal reaction value
    """

    assert f2f_i[0] == f2f_c[0]

    polygs = []

    idp1 = 0
    idp2 = 0
    idx1 = 0
    idx2 = 0
    for i_f2f in range(f2f_i[0]):
      idx1 += 1
      idx2 += 1

      nb_v = f2f_i[idx1]

      polyg_idx = np.where( np.isin( inters_prprx['icdan'], f2f_i[idx1+1:idx1+nb_v+1] ) )[0]

      loc  = inters_prprx[polyg_idx]
      rc   = loc['coor']
      rn   = loc['rl'][:,1]

      reac = np.matmul( loc['uc'], loc['rl'][:,:,np.newaxis] )

      idx1 += nb_v

      # index shift of coordinates for each polygon of each surface
      nbt = [0]
      for i_poly in range(f2f_c[idx2]):
        idx2 += 1
        idp2 += f2f_c[idx2]
        nbt.append(idp2-idp1)
      gc = f2f_p[idp1:idp2,:]
      idp1 = idp2

      polygs.append( (gc, nbt, rc, reac[:,:,0], rn) )

    return polygs

def getck( polyg, ptx=None ):
    """
    Only compute the central kernel of a genral single polygon in 3D
    """

    # getting 3D coordinates projected on the contact surface geometry
    frame_g, orig_g = utils.space_mapping(polyg)
    mapped_cg = np.matmul(frame_g,(polyg-orig_g).T).T

    ck_coor2d = utils.central_kernel(mapped_cg[:,:2], ptx)
    ck_coor   = np.zeros( [ck_coor2d.shape[0],3] )
    ck_coor[:,:2] = ck_coor2d[:,:]

    unmapped_ck = np.matmul(ck_coor,frame_g) + orig_g

    return unmapped_ck

def polyg2ck( polygs ):
    """
    Compute the central kernel of each input polygons

    The central kernel is a tuple with coordinates of the kernel,
    the equivalent normal stress on the polygon, coordinates of the center
    of pressure and if this point is inside the central kernel
    """

    ck = []

    i_f2f = 0
    for coor_g, ptx, coor_f, reac, rn in polygs:
        i_f2f += 1

        # getting 3D coordinates projected on the contact surface geometry
        frame_g, orig_g = utils.space_mapping(coor_g)
        mapped_cg = np.matmul(frame_g,(coor_g-orig_g).T).T

        # getting 3D coordinates projected on the contact force plan
        frame_f, orig_f = utils.space_mapping(coor_f)
        mapped_cf = np.matmul(frame_f,(coor_f-orig_f).T).T

        surf = utils.area(mapped_cg[:,:2], ptx)

        xc  = utils.pressure_center(mapped_cf, rn)
        s_n = utils.sigma(rn, surf)

        # in case the polygon is not convex, its shape
        # maybe different from the input...
        ck_coor2d = utils.central_kernel(mapped_cg[:,:2], ptx)
        ck_coor   = np.zeros( [ck_coor2d.shape[0],3] )
        ck_coor[:,:2] = ck_coor2d[:,:]

        # check if center of pressure is in central kernel
        # first expressed xc in frame_g instead of frame_f
        xc = np.matmul(frame_f.T,xc).T + orig_f
        xc = np.matmul(frame_g,(xc-orig_g).T).T
        is_in = utils.is_inside(xc, ck_coor2d)

        unmapped_ck = np.matmul(ck_coor,frame_g) + orig_g
        unmapped_xc = np.matmul(xc,frame_f) + orig_f

        ck.append( (unmapped_ck, s_n, unmapped_xc, is_in,) )

    return ck


def get( f2f_c, f2f_p, f2f_i, inters ):

    polygs = inters2polyg( f2f_c, f2f_p, f2f_i, inters )
    ck     = polyg2ck( polygs )

    return polygs, ck
