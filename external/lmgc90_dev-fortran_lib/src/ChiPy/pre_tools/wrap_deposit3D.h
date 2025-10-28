/* fichier d'entête C++ associe au fichier wrap_deposit3D.f90:
   acces en C++ aux fonction du modules wrap_deposit3D */

#ifndef wrap_deposit3D
#define wrap_deposit3D

   /**
    * @fn void deposit3D_InContainer(double *rvector_in, int rlength_in,
    *                                int i_shape, double p1, double p2, double p3,
    *                                double **r8_vector , int *r8_size,
    *                                double **matrix_out, int *dim1, int *dim2,
    *                                double *rvector_in2=nullptr, int rlength_in2=0,
    *                                double *matrix_in=nullptr, int  idim1=0, int idim2=0,
    *                                int    *ivector_in=nullptr, int ilength_in=0, int with_log=0)
    * @brief Computes a new deposit under gravity in a container
    *
    * i_shape = 0 : box
    * -  a point (x, y, z) is in the box iff x is in [-lx/2, lx/2], y is in [-ly/2, ly/2] and z is in [0, lz]
    * i_shape = 1 : cylinder
    * - a point (x, y, z) is in the cylinder iff x^2 + y^2 is in [0, R^2] and z is in [0, lz]
    * i_shape = 2 : sphere
    * - a point (x, y, z) is in the sphere iff x^2 + y^2 + z^2 is in [0, R^2]
    *
    * @cond PYDOC
    * python call: radii, coor = deposit3D_InContaier(in_radii, shape, p1, p2, p3[, dradii, dcoor, seed, with_log])
    * @param[in] in_radii (double array): given radii list (i.e. granulometry)
    * @param[in] shape(integer): of container (0->box, 1->cylinder, 2->sphere)
    * @param[in] p1 (double): box-> lx, cylinder->R, sphere->R
    * @param[in] p2 (double): box-> ly, cylinder->lz, sphere->ignored
    * @param[in] p3 (double): box-> lz, cylinder->ignored, sphere->ignored
    * @param[in] dradii (double array) (optional) : a list of already deposited radii
    * @param[in] dcoor  (double array) (optional) : a list of already deposited coor (must be of size [nb_dradii,3])
    * @param[in] seed (integer array) (optional) : an input seed to control randomness
    * @param[in] with_log(integer): de/activate log message
    * @return
    *  radii (double array): list of deposited radii
    *  coor  (double array): coordinates of deposited radii (shape [nb_radii,3])
    * @endcond PYDOC
    */
    extern "C" void deposit3D_InContainer(double *rvector_in, int rlength_in,
                                          int i_shape, double p1, double p2, double p3,
                                          double **r8_vector , int *r8_size,
                                          double **matrix_out, int *dim1, int *dim2,
                                          double *rvector_in2=nullptr, int rlength_in2=0,
                                          double *matrix_in=nullptr, int  idim1=0, int idim2=0,
                                          int    *ivector_in=nullptr, int ilength_in=0, int with_log=0);

#endif
