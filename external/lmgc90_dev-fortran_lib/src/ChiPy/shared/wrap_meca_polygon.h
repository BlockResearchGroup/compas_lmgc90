/*==========================================================================
 *
 * Copyright 2000-2025 CNRS-UM.
 *
 * This file is part of a software (LMGC90) which is a computer program 
 * which purpose is to modelize interaction problems (contact, multi-Physics,etc).
 *
 * This software is governed by the CeCILL license under French law and
 * abiding by the rules of distribution of free software.  You can  use, 
 * modify and/ or redistribute the software under the terms of the CeCILL
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info". 
 *
 * As a counterpart to the access to the source code and  rights to copy,
 * modify and redistribute granted by the license, users are provided only
 * with a limited warranty  and the software's author,  the holder of the
 * economic rights,  and the successive licensors  have only  limited
 * liability. 
 *
 * In this respect, the user's attention is drawn to the risks associated
 * with loading,  using,  modifying and/or developing or reproducing the
 * software by the user in light of its specific status of free software,
 * that may mean  that it is complicated to manipulate,  and  that  also
 * therefore means  that it is reserved for developers  and  experienced
 * professionals having in-depth computer knowledge. Users are therefore
 * encouraged to load and test the software's suitability as regards their
 * requirements in conditions enabling the security of their systems and/or 
 * data to be ensured and,  more generally, to use and operate it in the 
 * same conditions as regards security. 
 *
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL license and that you accept its terms.
 *
 * To report bugs, suggest enhancements, etc. to the Authors, contact
 * Frederic Dubois.
 *
 * frederic.dubois@umontpellier.fr
 *
 *=========================================================================*/
#ifndef wrap_meca_polygon_h
#define wrap_meca_polygon_h
 
 /**
  * @fn void MecaPolyg_CentralKernel(double * matrix_in, int idim1, int idim2,
  *                                  int * ivector_in, int ilength_in,
  *                                  double ** matrix_out, int * dim1, int * dim2)
  * @brief Compute the central kernel of an input surface (which may be composed of several polygons)
  *
  * @cond PYDOC
  * python usage : ck_pts = MecaPolyg_CentralKernel(points, sizes)
  * @param[in] points (double array) : coordinates of the points of the surface (2D)
  * @param[in] sizes (integer array) : number of vertices of each polygons of the surface
  * @return ck_pts (double array) : coordinates of the points of the central kernel
  * @endcond
  *
  * @cond CDOC
  * @param[in]  matrix_in  (double *)  : coordinates of the 2D surface (not closed)
  * @param[in]  idim1      (int)       : number of vertices
  * @param[in]  idim2      (int)       : space dim, must be 2
  * @param[in]  ivector_in (int *)     : number of vertices of each polygon in surface
  * @param[in]  ilength_in (int)       : number of polygons in surface
  * @param[out] matrix_out (double **) : coordinates of the central kernel points
  * @param[out] dim1       (int *)     : number of points
  * @param[out] dim2       (int *)     : space dim, must be 2
  * @endcond
  */
  extern "C" void MecaPolyg_CentralKernel(double * matrix_in, int idim1, int idim2, int * ivector_in, int ilength_in, double ** matrix_out, int * dim1, int * dim2);


 /**
  * @fn void MecaPolyg_StressField(double * matrix_in, int idim1, int idim2,
  *                                int * ivector_in, int ilength_in,
  *                                double * rvector_in , int rlength_in ,
  *                                double * rvector_in2, int rlength_in2,
  *                                double * rvector_in3, int rlength_in3,
  *                                double ** matrix_out, int * dim1, int * dim2,
  *                                int** i4_vector, int* i4_size,
  *                                double ** matrix_out_2, int * dim1_2, int * dim2_2,
  *                                int** i4_vector_2, int* i4_size_2,
  *                                double ** r8_vector, int * r8_size,
  *                                double * res )
  * @brief Compute the stress field of an input 3D surface from a center of pressure and a normal reaction
  *
  * @cond PYDOC
  * python usage : coorC, sizeC, coordD, sizeD, sigma, decomp = MecaPolyg_StressField(face, sizes, cop, rn)
  * @param[in] face  (double array)  : coordinates the surface as a Nx3 array
  * @param[in] sizes (integer array) : number of vertices of each polygons of the surface
  * @param[in] cop   (double array)  : center of pressure coordinates
  * @param[in] rn    (double array)  : normal reaction
  * @return coorC (double array)  : coordinates of the compressed part
  *         sizeC (integer array) : number of vertices of each polygon of the compressed part
  *         coorD (double array)  : coordinates of the decompressed part
  *         sizeD (integer array) : number of vertices of each polygon of the decompressed part
  *         sigma (double array)  : stress value on each vertices of the compressed part
  *         decomp (double)       : decompression value (between 0. and 1.)
  * @endcond
  *
  * @cond CDOC
  * ... flême !
  * @endcond
  */
  extern "C" void MecaPolyg_StressField(double * matrix_in, int idim1, int idim2,
                                        int * ivector_in, int ilength_in,
                                        double * rvector_in , int rlength_in ,
                                        double * rvector_in2, int rlength_in2,
                                        double ** matrix_out, int * dim1, int * dim2,
                                        int** i4_vector, int* i4_size,
                                        double ** matrix_out_2, int * dim1_2, int * dim2_2,
                                        int** i4_vector_2, int* i4_size_2,
                                        double ** r8_vector, int * r8_size,
                                        double * res );

#endif /* wrap_meca_polygon_h */
