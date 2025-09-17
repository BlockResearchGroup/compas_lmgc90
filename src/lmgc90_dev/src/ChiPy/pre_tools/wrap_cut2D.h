/* fichier d'entête C++ associe au fichier wrap_cut2D.f90:
   acces en C++ aux fonction du modules wrap_cut2D */

#ifndef wrap_cut2D
#define wrap_cut2D

   /* procedure qui realise une nouvelle decoupe */
   /**
    * @fn void cut2D_Cut(double *radii, int nb_particles, double* coor, 
    *    int nb_coor, double *slope_coor, int nb_slope_coor, 
    *    int *nb_inner_particles)
    * @fn void cut2D_Cut(double *rvector_in , int rlength_in,
    *                    double *matrix_in  , int  idim1  , int idim2,
    *                    double *matrix_in_2, int  idim1_2, int idim2_2
    *                    double **r8_vector , int *r8_size,
    *                    double **matrix_out, int *dim1   , int *dim2,
    *                   )
    * @brief Computes a new cut and return the deposited radii and coordinates.\n
    * A polyline is defined by a given set of points. 
    * In the case of a closed polyline, only the inner particles remain. 
    * An open polyline is supposed to link the the two vertical walls of the 
    * box. In this case, only particles under the polyline remain.
    * @cond PYDOC
    * python call: radii, coor = cut2D_Cut(dradii, dcoor, slope_coor)
    * @param[in] dradii (double array): given radii list (i.e. granulometry)
    * @param[in] dcoor (double array): coordinates of the particles [nb_radii,2]
    * @param[in] slope_coor (double array): coordinates of the cutting polyline [nb_pt,2]
    * @return:
    *  - radii (double array): the radii of deposited particles [nb_part]
    *  - coor  (double array): the coordinates of the deposited particles [nb_part,2]
    * @endcond PYDOC
    */
   extern "C" void cut2D_Cut(double *rvector_in , int rlength_in,
                             double *matrix_in  , int  idim1, int idim2,
                             double *rmatrix_in , int  rdim1, int rdim2,
                             double **r8_vector , int *r8_size,
                             double **matrix_out, int *dim1 , int *dim2);

#endif
