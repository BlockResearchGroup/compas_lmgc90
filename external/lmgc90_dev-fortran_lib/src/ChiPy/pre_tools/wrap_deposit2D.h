/* fichier d'entête C++ associe au fichier wrap_deposit2D.f90:
   acces en C++ aux fonction du modules wrap_deposit2D */

#ifndef wrap_deposit2D
#define wrap_deposit2D

   /**
    * @fn void deposit2D_Potential(double *rvector_in, int rlength_in,
    *                              double lx, int potential,
    *                              double **matrix_out, int *dim1, int *dim2,
    *                              double *rvector_in2=nullptr, int rlength_in2=0,
    *                              double *matrix_in=nullptr, int  idim1=0, int idim2=0
    *                             )
    * @brief Computes a new deposit under potential with or without big particles
    * @cond PYDOC
    * python call: coor = deposit2D_Potential(in_radii, lx, potential[, dradii, dcoor])
    * @param[in] in_radii (double array): given radii list (i.e. granulometry)
    * @param[in] lx (double): width of the box in which to deposit
    * @param[in] potential (integer): for deposit (1->gravity, 2->wall, 3->big_particles)
    * @param[in] dradii (double array) (optional) : a list of already deposited radii
    * @param[in] dcoor  (double array) (optional) : a list of already deposited coor (must be of size [nb_dradii,3])
    * @return
    *  coor  (double array): coordinates of deposited radii (shape [nb_radii,2])
    * @endcond PYDOC
    */
   extern "C" void deposit2D_Potential(double *rvector_in, int rlength_in,
                                       double lx, int potential,
                                       double **matrix_out, int *dim1, int *dim2,
                                       double *rvector_in2=nullptr, int rlength_in2=0,
                                       double *matrix_in=nullptr, int  idim1=0, int idim2=0
                                      );

#endif
