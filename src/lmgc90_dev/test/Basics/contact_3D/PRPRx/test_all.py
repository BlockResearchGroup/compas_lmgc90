from pylmgc90 import chipy

import generate
import compute
import check

def detection_choice(i_test, nb_iter, gdist, tol):

  # setting some detection options
  #chipy.PRPRx_LowSizeArrayPolyr(sfactor)
  #chipy.PRPRx_ShrinkPolyrFaces(shrink)
  
  ##Cundall Common plane
  #chipy.PRPRx_UseCpCundallDetection(nb_iter)
  #chipy.PRPRx_SetCundallNeighbor(neighbor)
  ##Face to face common plane
  #chipy.PRPRx_UseCpF2fExplicitDetection(tol)
  #chipy.PRPRx_UseCpF2fDetection(tol,iter)
  #chipy.PRPRx_SetF2fMinimalSurfaceSize(tol)
  ##Non convex detection
  #chipy.PRPRx_UseNcDetection(gdist)
  #chipy.PRPRx_UseNcF2fDetection(gdist,tol)
  #chipy.PRPRx_UseNcF2fExplicitDetection(gdist,tol)
  #chipy.PRPRx_WithNodalContact()

  if i_test in [1, 2, 3, 4, 6, 7, 8]:
    chipy.PRPRx_UseCpCundallDetection(nb_iter)
  elif i_test in [5]:
    chipy.PRPRx_UseNcDetection(gdist)
  elif i_test in [9]:
    chipy.PRPRx_UseCpF2fExplicitDetection(tol)
  else:
    chipy.PRPRx_UseNcF2fExplicitDetection(gdist,tol)
  
if __name__ == '__main__':

  ### common parameters for :
  ### generation

  dim = 3

  dist = 5.e-2
  ray  = 0.3

  ### common parameters for :
  ### computation

  # modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
  mhyp = 0

  # time evolution parameters
  dt = 1e-3
  nb_steps = 1

  # theta integrator parameter
  theta = 0.5

  # deformable  yes=1, no=0
  deformable = 0

  # interaction parameters
  Rloc_tol    = 5.e-3

  # nlgs parameters
  solver_param = { 'conv'  : 1e-4   ,
                   'relax' : 1.0    ,
                   'norm'  : 'Quad ',
                   'gsit1' : 10     ,
                   'gsit2' : 2      ,
                   'stype' : 'Stored_Delassus_Loops         '
                 }

  # write parameter
  freq_write = 1

  # display parameters
  freq_display = 1

  # detection
  nb_iter = 10
  gdist   = 1.1*dist
  f2f_tol = 1e-5

  test_begin = 1
  test_end   = 10

  ### now testing everything:
  for i_test in range(test_begin, test_end+1):

    # DATBOX generation
    wd = generate.test(ray, dist, i_test)

    # initialize
    compute.init_lmgc90(dt, theta, dim, mhyp, deformable, wd)

    # choosing detection methods/options
    detection_choice(i_test, nb_iter, gdist, f2f_tol)

    # computation... only one time step
    #for k in range(nb_steps):
    compute.compute_lmgc90_one_step(solver_param, freq_display, freq_write)

    # check some results
    eval('check.'+wd+'(ray,dist)')

    # clean computation
    compute.finalize_lmgc90()

